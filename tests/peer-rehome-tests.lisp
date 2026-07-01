;;;; Branch B hub re-home apply (B2d-2): the resolver wired into the HUB push path.
;;;;
;;;; peer-merge-apply-tests drives the DEVICE pull-apply; this drives the HUB
;;;; REHOME-AUTHORED-OP -- the system-of-record side, where a device-pushed authored
;;;; op is applied through a JOURNALING transaction (so every other device pulls the
;;;; result) while merging field-by-field against the hub's live copy and preserving
;;;; the original op-id/origin/lamport on the re-journaled feed entry (design §5).
;;;;
;;;; Single image, no transport: build a device-authored op by hand and re-home it on
;;;; a hub peer-graph.  Assert: the merge lands (LWW / safety), the op-id is preserved
;;;; (a re-push is deduped), a release surfaces a conflict, and -- the hub's whole
;;;; point -- the merged result is RE-JOURNALED to the feed under the author's op-id.
;;;; Reuses g-person + the *apply-policy* (name :safety, else :lww) from the apply suite.

(in-package #:graph-db/test)

(def-suite peer-rehome-suite
  :description "Branch B hub re-home apply (REHOME-AUTHORED-OP)."
  :in graph-db-suite)

(in-suite peer-rehome-suite)

(defparameter *rehome-hub-origin* (id16 4) "The hub (receiver / system-of-record) origin.")

(defmacro with-rehome-hub ((g) &body body)
  "Run BODY with a hub peer-graph (merge-policy *apply-policy*) bound to G and to
*graph*.  Port 0 -> the accept loop binds an ephemeral port and idles; close joins it."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :hub :origin-id *rehome-hub-origin*
                           :replication-port 0 :replication-key "k"
                           :merge-policy *apply-policy* :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(defun feed-op-ids (graph)
  "The op-ids of every authored op in GRAPH's replication logs (the pull feed)."
  (let ((*graph* graph) (ids '()))
    (dolist (log (graph-db::applicable-replication-logs 0 graph) (nreverse ids))
      (ignore-errors
       (with-open-file (in log :element-type '(unsigned-byte 8))
         (loop
           (let ((meta (handler-case (graph-db::read-stream-packet in) (error () nil))))
             (unless meta (return))
             (let ((txh-vec (handler-case (graph-db::read-stream-packet in) (error () nil))))
               (unless txh-vec (return))
               (let ((wc (graph-db::write-count (graph-db::deserialize-tx-header-vector txh-vec))))
                 (dotimes (i wc) (handler-case (graph-db::read-stream-packet in) (error () (return))))
                 (multiple-value-bind (origin op-id lamport op-class)
                     (graph-db::deserialize-peer-meta meta)
                   (declare (ignore origin lamport))
                   (when (= op-class graph-db::+peer-op-authored+)
                     (push op-id ids))))))))))))

(defun id-in (id list) (member id list :test #'equalp))

(test rehome-lww-incoming-newer-lands-and-rejournals
  "A held field bucketed :lww takes the incoming value when the incoming stamp is
newer; the op-id is preserved (a re-push is deduped) and the merged result is
re-journaled to the pull feed under the author's op-id for other devices."
  (with-rehome-hub (g)
    (let ((vid (id (with-transaction () (make-g-person :name "SAFE" :age 5))))) ; hub lamport 1
      (with-transaction ()                                                       ; local edit, stamp age=2
        (let ((v (copy (lookup-vertex vid)))) (setf (slot-value v 'age) 6) (save v)))
      (let* ((op (make-authored-op (lookup-vertex vid)
                                   '((:name . "SAFE") (:age . 5))
                                   '((:name . "SAFE") (:age . 9))
                                   10 *merge-hub-origin*))
             (opid (graph-db::peer-op-op-id op)))
        (is (graph-db::rehome-authored-op g op) "the op re-homes")
        (let ((v (lookup-vertex vid)))
          (is (= 9 (slot-value v 'age)) "incoming (lamport 10) beats local (2)")
          (is (equal "SAFE" (slot-value v 'name)) "the carried field is untouched"))
        (is (= 10 (graph-db::node-field-stamp g vid :age)) ":age stamp advanced to 10")
        (is (>= (graph-db::lamport-counter g) 10) "the hub clock observed the incoming stamp")
        ;; identity preserved: the same op-id is now applied -> a re-push is a no-op.
        (is (graph-db::op-applied-p g opid) "the author's op-id is recorded (preserved)")
        (is (null (graph-db::rehome-authored-op g op)) "re-pushing the same op is deduped")
        ;; re-journaled to the feed under the AUTHOR's op-id (so device E pulls it).
        (is (id-in opid (feed-op-ids g)) "the merged result is re-journaled under the op-id")))))

(test rehome-safety-release-keeps-danger-and-surfaces
  "On the hub, an incoming RELEASE (dangerous -> SAFE) on a :safety field keeps the
dangerous local value and surfaces a conflict -- and still re-journals (the retained
dangerous value must propagate to other devices)."
  (with-rehome-hub (g)
    (let ((vid (id (with-transaction () (make-g-person :name "Confirmed" :age 5)))))
      (let* ((op (make-authored-op (lookup-vertex vid)
                                   '((:name . "Confirmed") (:age . 5))
                                   '((:name . "SAFE") (:age . 5))
                                   10 *merge-hub-origin*))
             (opid (graph-db::peer-op-op-id op)))
        (is (graph-db::rehome-authored-op g op) "the op re-homes")
        (is (equal "Confirmed" (slot-value (lookup-vertex vid) 'name))
            "the release attempt keeps the DANGEROUS local value")
        (let ((cs (graph-db::get-peer-conflicts g)))
          (is (= 1 (length cs)) "one conflict is surfaced")
          (is (equal "Confirmed" (graph-db::peer-conflict-kept-value (first cs))))
          (is (equal "SAFE" (graph-db::peer-conflict-loser-value (first cs)))))
        (is (id-in opid (feed-op-ids g)) "the kept-dangerous result still re-journals")))))

(test rehome-create-of-unheld-node-lands
  "A device authors a brand-new node the hub does not hold: re-home creates it on the
hub with the incoming state and re-journals it under the op-id."
  (with-rehome-hub (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name 'g-person :vertex :graph g)))
           (newid (gen-id))
           (base (make-instance 'g-person :id newid :type-id tid :revision 0))
           (op (progn (setf (graph-db::data base) '((:name . "Fresh") (:age . 3)))
                      (graph-db::make-peer-op
                       :kind :authored :op-id (graph-db::gen-op-id)
                       :origin *merge-hub-origin* :lamport 7 :tx-id 8
                       :writes (list (make-instance 'graph-db::tx-create :node base)))))
           (opid (graph-db::peer-op-op-id op)))
      (is (graph-db::rehome-authored-op g op) "the create re-homes")
      (let ((v (lookup-vertex newid)))
        (is (not (null v)) "the node now exists on the hub")
        (is (equal "Fresh" (slot-value v 'name)))
        (is (= 3 (slot-value v 'age))))
      (is (= 7 (graph-db::node-field-stamp g newid :name)) "create stamps the fields")
      (is (id-in opid (feed-op-ids g)) "the created node re-journals under the op-id"))))
