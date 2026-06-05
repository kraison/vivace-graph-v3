(in-package :graph-db)

(defvar *graphs*
  #+sbcl
  (make-hash-table :test 'equal :synchronized t)
  #+lispworks
  (make-hash-table :test 'equal :single-thread nil)
  #+ccl
  (make-hash-table :test 'equal :shared t)
  #+ecl
  (make-hash-table :test 'equal))

(defclass graph ()
  ((graph-name :accessor graph-name :initarg :graph-name)
   (graph-open-p :accessor graph-open-p :initarg :graph-open-p :initform nil)
   (location :accessor location :initarg :location)
   (txn-log :accessor txn-log :initarg :txn-log)
   (txn-file :accessor txn-file :initarg :txn-file)
   (txn-lock :accessor txn-lock :initarg :txn-lock :initform (make-recursive-lock))
   (transaction-manager :accessor transaction-manager :initarg :transaction-manager)
   (replication-key :accessor replication-key :initarg :replication-key)
   (replication-port :accessor replication-port :initarg :replication-port)
   (vertex-table :accessor vertex-table :initarg :vertex-table)
   (edge-table :accessor edge-table :initarg :edge-table)
   (heap :accessor heap :initarg :heap)
   (indexes :accessor indexes :initarg :indexes)
   (schema :accessor schema :initarg :schema)
   (cache :accessor cache :initarg :cache)
   (ve-index-in :accessor ve-index-in :initarg :ve-index-in)
   (ve-index-out :accessor ve-index-out :initarg :ve-index-out)
   (vev-index :accessor vev-index :initarg :vev-index)
   (vertex-index :accessor vertex-index :initarg :vertex-index)
   (edge-index :accessor edge-index :initarg :edge-index)
   (spatial-index :accessor spatial-index :initarg :spatial-index :initform nil)
   (views-lock :accessor views-lock :initarg :views-lock
               :initform (make-recursive-lock))
   (views :accessor views :initarg :views)
   (write-stats :accessor write-stats :initarg :write-stats
                :initform
                #+ccl (make-hash-table :test 'eq :shared t)
                #+lispworks (make-hash-table :test 'eq :single-thread nil)
                #+ecl (make-hash-table :test 'eq)
                #+sbcl (make-hash-table :test 'eq :synchronized t))
   (read-stats :accessor read-stats :initarg :read-stats
               :initform
               #+ccl (make-hash-table :test 'eq :shared t)
               #+lispworks (make-hash-table :test 'eq :single-thread nil)
               #+ecl (make-hash-table :test 'eq)
               #+sbcl (make-hash-table :test 'eq :synchronized t))))

(defmethod print-object ((graph graph) stream)
  (print-unreadable-object (graph stream :type t :identity t)
    (format stream "~S ~S" (graph-name graph) (location graph))))

;; True while a read pin is held on the current thread (dynamic extent of a
;; WITH-READ-PIN).  Lets a nested LOOKUP-OBJECT skip its own eager byte
;; materialization: the enclosing pinned scope already protects the node, so its
;; data can stay lazy (or be materialized by the scan only when it escapes).
(defvar *read-pinned-p* nil)

;; MVCC read-epoch pin.  Defined here (early) because MAP-VERTICES / MAP-EDGES
;; use it and are compiled before transactions.lisp, which defines the
;; PIN-READ-EPOCH / UNPIN-READ-EPOCH functions this expands to (resolved at
;; runtime).  Holds a pin for BODY's dynamic extent so the reaper retains any
;; version BODY may dereference; a no-op when GRAPH has no transaction-manager
;; yet (open/recovery, when no reaper races).
(defmacro with-read-pin ((graph) &body body)
  (alexandria:with-gensyms (g tm tok)
    ;; A pin nested inside another (e.g. LOOKUP-VERTEX called from a MAP-VERTICES
    ;; scan) is a no-op: the outer pin was taken at an earlier (smaller) epoch, so
    ;; its floor is at least as conservative and already protects this read.  This
    ;; keeps per-node lock traffic off scans -- only the outermost reader pins.
    `(if *read-pinned-p*
         (progn ,@body)
         (let* ((,g ,graph)
                (,tm (and (slot-boundp ,g 'transaction-manager)
                          (transaction-manager ,g))))
           (if ,tm
               (let ((,tok (pin-read-epoch ,tm))
                     (*read-pinned-p* t))
                 (unwind-protect (progn ,@body)
                   (unpin-read-epoch ,tm ,tok)))
               (progn ,@body))))))

(defclass master-graph (graph)
  ((replication-mbox :accessor replication-mbox :initarg :replication-mbox)
   (replication-listener :accessor replication-listener :initarg :replication-listener)
   (stop-replication-p :accessor stop-replication-p :initarg :stop-replication-p :initform nil)
   (slaves :accessor slaves :initarg :slaves :initform ())
   (slaves-lock :accessor slaves-lock :initarg :slaves-lock :initform (make-recursive-lock))))

(defclass slave-graph (graph)
  ((master-host :accessor master-host :initarg :master-host)
   (slave-socket :accessor slave-socket :initarg :slave-socket)
   (stop-replication-p :accessor stop-replication-p :initarg :stop-replication-p :initform nil)
   (slave-thread :accessor slave-thread :initarg :slave-thread :initform nil)
   (master-txn-id :accessor master-txn-id :initarg :master-txn-id)))

(defgeneric graph-p (thing)
  (:method ((graph graph)) graph)
  (:method (thing) nil))

(defgeneric master-graph-p (thing)
  (:method ((graph master-graph)) graph)
  (:method (thing) nil))

(defgeneric slave-graph-p (thing)
  (:method ((graph slave-graph)) graph)
  (:method (thing) nil))

(defgeneric init-schema (graph))
(defgeneric update-schema (graph-or-name))
(defgeneric snapshot (graph &key &allow-other-keys))
(defgeneric scan-for-unindexed-nodes (graph))
(defgeneric start-replication (graph &key package))
(defgeneric stop-replication (graph))

(defun lookup-graph (name)
  (gethash name *graphs*))
