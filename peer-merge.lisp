(in-package :graph-db)

;;; ===========================================================================
;;; Peer replication Branch B: the per-field conflict resolver (B2a).
;;;
;;; When a replica applies an AUTHORED update to a node it also holds, the field
;;; values may have diverged (the ETL laptop, the hub, and mobile peers all edit
;;; the same nodes).  This resolves that divergence field-by-field, dispatching on
;;; an app-declared BUCKET.  Domain semantics (which field is which bucket, what a
;;; SAFE value is) live in the app's conflict policy (mine-action's
;;; conflict-policy.lisp); the engine supplies the mechanism -- the MERGE-POLICY
;;; seam, mirroring the DISCLOSABLE-P/EXPORT-PREDICATE seam.
;;;
;;; This file is the PURE core (no storage, no network): given the local node's
;;; data, the incoming op's new/old data, per-field Lamport stamps, and the
;;; policy, it produces the merged data + the stamps to persist + the conflicts to
;;; surface.  B2b/c/d wire in the stamp store, the policy slot, and the push
;;; transport that calls this.
;;;
;;; Buckets (design §11, as the app collapsed them in bucketing v1):
;;;   :create         new node by UUID -- never reaches the field resolver
;;;   :lww            (default) per-field last-writer-wins by (Lamport, origin)
;;;   :safety         BINARY safety class -- the app's SAFETY-MERGE decides the
;;;                   winner + whether to SURFACE (toward-danger auto-applies;
;;;                   only a release surfaces, keeping the dangerous local value)
;;;   :geometry       concurrent polygon/point edits SURFACE (never silent LWW)
;;;   :safety-surface edge-only in the app; no field case (folded to :geometry here)
;;; ===========================================================================

(defstruct (merge-policy (:constructor %make-merge-policy))
  "The app-supplied conflict contract the Branch B resolver dispatches through.
FIELD-BUCKET-FN is (NODE-TYPE SLOT) -> bucket keyword (default :lww).
SAFETY-MERGE-FN is (SLOT LOCAL INCOMING LOCAL-NEWER-P) -> (values WINNER SURFACE-P),
required once any field is bucketed :safety."
  (field-bucket-fn (lambda (type slot) (declare (ignore type slot)) :lww)
                   :type function)
  (safety-merge-fn nil))

(defun make-merge-policy (&key field-bucket safety-merge)
  "Build a MERGE-POLICY from the app's FIELD-BUCKET and SAFETY-MERGE functions."
  (%make-merge-policy
   :field-bucket-fn (or field-bucket
                        (lambda (type slot) (declare (ignore type slot)) :lww))
   :safety-merge-fn safety-merge))

(defstruct (peer-conflict (:constructor make-peer-conflict))
  "A surfaced field conflict retained for the app's review surface: on SLOT
(bucket BUCKET) of NODE-ID the merge KEPT KEPT-VALUE (authored by KEPT-ORIGIN) and
set aside LOSER-VALUE (authored by LOSER-ORIGIN at LOSER-LAMPORT)."
  node-id slot bucket kept-value kept-origin loser-value loser-origin loser-lamport)

(defun %alist-val (slot alist) (cdr (assoc slot alist)))

(defun peer-stamp-newer-p (a-lamport a-origin b-lamport b-origin)
  "Lexicographic (LAMPORT, ORIGIN): T iff stamp A is strictly later than B.  Origin
breaks a Lamport tie so every replica picks the same LWW winner (convergence).  A
NIL origin sorts lowest (an unstamped local field always loses a real op)."
  (cond ((> a-lamport b-lamport) t)
        ((< a-lamport b-lamport) nil)
        ((and a-origin b-origin)
         (let ((c (peer-compare-ids a-origin b-origin))) (> c 0)))
        (t (and a-origin (not b-origin)))))       ; stamped beats unstamped on a tie

(defun peer-compare-ids (a b)
  "Lexicographic byte compare of two 16-byte ids: -1 / 0 / 1."
  (dotimes (i 16 0)
    (let ((x (aref a i)) (y (aref b i)))
      (cond ((< x y) (return -1)) ((> x y) (return 1))))))

;;; ---------------------------------------------------------------------------
;;; B2b: the per-field (lamport . origin) stamp store.
;;;
;;; Behind this GET/SET API so the substrate can change.  v1 (C) is an in-memory
;;; map node-id -> alist (slot . (lamport . origin)), persisted on open/close;
;;; final target (B) is a heap-backed side store.  A crash between persists loses
;;; recent stamps -> local fields read as unstamped -> the next op wins the LWW
;;; (safe: the loser is retained via MVCC; :safety resolution barely uses stamps).
;;; ---------------------------------------------------------------------------

(defun ensure-field-stamps (graph)
  (or (field-stamps graph)
      (setf (field-stamps graph) (make-hash-table :test 'equalp))))

(defun node-field-stamps (graph node-id)
  "The alist slot -> (lamport . origin) of NODE-ID's stamps (a fresh copy; NIL if none)."
  (and (field-stamps graph)
       (with-recursive-lock-held ((field-stamps-lock graph))
         (copy-alist (gethash node-id (field-stamps graph))))))

(defun node-field-stamp (graph node-id slot)
  "(values LAMPORT ORIGIN) for SLOT of NODE-ID; (0 NIL) if unstamped."
  (let ((s (cdr (assoc slot (node-field-stamps graph node-id)))))
    (if s (values (car s) (cdr s)) (values 0 nil))))

(defun set-node-field-stamp (graph node-id slot lamport origin)
  "Record that SLOT of NODE-ID was last written at (LAMPORT . ORIGIN) on this replica."
  (with-recursive-lock-held ((field-stamps-lock graph))
    (let* ((tbl (ensure-field-stamps graph))
           (alist (gethash node-id tbl))
           (pair (assoc slot alist)))
      (if pair
          (setf (cdr pair) (cons lamport origin))
          (setf (gethash node-id tbl) (acons slot (cons lamport origin) alist)))))
  (values lamport origin))

(defun apply-field-stamp-updates (graph node-id stamp-updates)
  "Persist MERGE-AUTHORED-FIELDS's STAMP-UPDATES (slot -> (lamport . origin)) for NODE-ID."
  (dolist (u stamp-updates)
    (set-node-field-stamp graph node-id (car u) (cadr u) (cddr u))))

(defun remove-node-field-stamps (graph node-id)
  "Drop all of NODE-ID's stamps (on purge/delete)."
  (when (field-stamps graph)
    (with-recursive-lock-held ((field-stamps-lock graph))
      (remhash node-id (field-stamps graph)))))

;;; Persistence (v1: a printed snapshot on open/close; ids/origins as hex so the
;;; form is portable + human-legible).

(defgeneric field-stamps-file (graph)
  (:method (graph)
    (make-pathname :name "field-stamps" :type "dat" :defaults (location graph))))

(defun persist-field-stamps (graph)
  (when (field-stamps graph)
    (with-recursive-lock-held ((field-stamps-lock graph))
      (with-open-file (s (field-stamps-file graph) :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
        (with-standard-io-syntax
          (let ((*package* (find-package :graph-db)))
            (write
             (loop for id being the hash-keys of (field-stamps graph) using (hash-value alist)
                   collect (cons (peer-id->hex id)
                                 (loop for (slot . stamp) in alist
                                       collect (list slot (car stamp)
                                                     (and (cdr stamp) (peer-id->hex (cdr stamp)))))))
             :stream s))))))
  graph)

(defun load-field-stamps (graph)
  (let ((tbl (make-hash-table :test 'equalp))
        (file (field-stamps-file graph)))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (with-standard-io-syntax
          (let ((*package* (find-package :graph-db)) (*read-eval* nil))
            (dolist (entry (read s nil nil))
              (setf (gethash (peer-hex->id (car entry)) tbl)
                    (loop for (slot lamport origin-hex) in (cdr entry)
                          collect (cons slot (cons lamport
                                                   (and origin-hex (peer-hex->id origin-hex)))))))))))
    (setf (field-stamps graph) tbl)))

(defun authored-changed-slots (write)
  "The slots an authored WRITE actually changed -- all of a create's data, or the
diff of an update's new vs old.  Used to stamp locally-authored edits."
  (etypecase write
    (tx-delete '())                                     ; a delete stamps nothing
    (tx-create (mapcar #'car (data (node write))))
    (tx-update (let ((new (data (node write))) (old (data (old-node write))))
                 (loop for (slot . val) in new
                       unless (equalp val (cdr (assoc slot old)))
                       collect slot)))))

(defun merge-authored-fields (policy node-type node-id
                              local-data new-data old-data
                              incoming-lamport incoming-origin local-stamps)
  "Resolve one incoming authored update against local state, per field, by bucket.

LOCAL-DATA   the receiver's current data alist (slot . value).
NEW-DATA     the op's post-edit data; OLD-DATA its base (what the author saw).
             Only slots the author actually changed (new /= old) are resolved --
             an unchanged carried field never clobbers a concurrent local edit.
INCOMING-*   the op's Lamport stamp + 16-byte origin id (one per op).
LOCAL-STAMPS an alist slot -> (lamport . origin) for the receiver's fields.

Returns (values MERGED-DATA STAMP-UPDATES CONFLICTS):
  MERGED-DATA   LOCAL-DATA with the fields the incoming op WON applied;
  STAMP-UPDATES alist slot -> (lamport . origin) to persist for fields that took a
                new value (so future LWW compares against the right stamp);
  CONFLICTS     list of PEER-CONFLICT to retain/surface."
  (let ((merged (copy-alist local-data))
        (stamp-updates '())
        (conflicts '()))
    (labels ((setf-field (slot val)
               (let ((p (assoc slot merged)))
                 (if p (setf (cdr p) val) (push (cons slot val) merged))))
             (stamp (slot lam org) (push (cons slot (cons lam org)) stamp-updates))
             (surface (slot bucket kept kept-org loser loser-org loser-lam)
               (push (make-peer-conflict :node-id node-id :slot slot :bucket bucket
                                         :kept-value kept :kept-origin kept-org
                                         :loser-value loser :loser-origin loser-org
                                         :loser-lamport loser-lam)
                     conflicts)))
      (dolist (pair new-data)
        (let ((slot (car pair)))
          (when (not (equalp (cdr pair) (%alist-val slot old-data)))  ; author changed it
            (let* ((incoming (cdr pair))
                   (local (%alist-val slot local-data))
                   (ls (cdr (assoc slot local-stamps)))
                   (local-lamport (or (car ls) 0))
                   (local-origin (cdr ls))
                   (bucket (funcall (merge-policy-field-bucket-fn policy) node-type slot)))
              (cond
                ;; Values agree -> fast-forward: adopt the value (already equal),
                ;; advance the field clock if the incoming stamp is later.  No conflict.
                ((equalp local incoming)
                 (when (peer-stamp-newer-p incoming-lamport incoming-origin
                                           local-lamport local-origin)
                   (stamp slot incoming-lamport incoming-origin)))
                (t
                 (ecase bucket
                   (:lww
                    (when (peer-stamp-newer-p incoming-lamport incoming-origin
                                              local-lamport local-origin)
                      (setf-field slot incoming)
                      (stamp slot incoming-lamport incoming-origin)))
                   (:safety
                    (multiple-value-bind (winner surface-p)
                        (funcall (merge-policy-safety-merge-fn policy) slot local incoming
                                 (peer-stamp-newer-p local-lamport local-origin
                                                     incoming-lamport incoming-origin))
                      (let ((took-incoming (equalp winner incoming)))
                        (setf-field slot winner)
                        (when took-incoming (stamp slot incoming-lamport incoming-origin))
                        (when surface-p
                          (surface slot bucket
                                   winner (if took-incoming incoming-origin local-origin)
                                   (if took-incoming local incoming)
                                   (if took-incoming local-origin incoming-origin)
                                   (if took-incoming local-lamport incoming-lamport))))))
                   ((:geometry :safety-surface)
                    ;; Concurrent divergent edit -> keep local live, surface the loser.
                    (surface slot bucket
                             local local-origin incoming incoming-origin incoming-lamport)))))))))
      (values merged stamp-updates conflicts))))
