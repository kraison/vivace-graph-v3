(in-package :graph-db)

(defvar *transaction* nil)
(defvar *end-of-transaction-action* '%commit)
(defparameter *maximum-transaction-attempts* 8
  "The number of times a transaction is retried after failing
  validation before it is forced to run within an exclusive lock.")
(defparameter *add-to-indexes-unless-present-p* nil
  "When true, add nodes to the type indexes with a check for
  unqiueness in the index. Needed when potentially recovering from a
  transaction multiple times, e.g. if the recovery crashes and has to
  be restarted.")

;;; Psyching-out set-byte

(defmethod set-byte ((array array) offset byte)
  (setf (aref array offset) byte))

(defmethod get-byte ((array array) offset)
  (aref array offset))

(defmethod serialize-uint64 ((array array) int offset)
  (setf (aref array (+ offset 0)) (ldb (byte 8  0) int))
  (setf (aref array (+ offset 1)) (ldb (byte 8  8) int))
  (setf (aref array (+ offset 2)) (ldb (byte 8 16) int))
  (setf (aref array (+ offset 3)) (ldb (byte 8 24) int))
  (setf (aref array (+ offset 4)) (ldb (byte 8 32) int))
  (setf (aref array (+ offset 5)) (ldb (byte 8 40) int))
  (setf (aref array (+ offset 6)) (ldb (byte 8 48) int))
  (setf (aref array (+ offset 7)) (ldb (byte 8 56) int)))

;;; Object sets keep track of transaction read sets and write sets to
;;; aid in validating transactions.
;;;
;;; Initial implementation as a hash table is for simplicity. Many
;;; other data structures can be used for performance if needed.
(defgeneric make-object-set (initial-contents))
(defgeneric object-set-count (set))
(defgeneric object-set-list (set))
(defgeneric object-set-empty-p (set)
  (:method (set)
    (zerop (object-set-count set))))

(defgeneric add-to-object-set (object set))
(defgeneric object-set-member-p (object set))
(defgeneric call-for-object-set-objects (fun set))

(defmacro do-object-set ((object set) &body body)
  `(block nil
     (call-for-object-set-objects (lambda (,object) ,@body)
                                  ,set)))

(defgeneric object-sets-intersect-p (set1 set2)
  (:method (set1 set2)
    (do-object-set (object set1)
      (when (object-set-member-p object set2)
        (return t)))))

(defclass object-set ()
  ((table
    :initform (make-id-table)
    :reader table)))

(defmethod object-set-list ((set object-set))
  (alexandria:hash-table-values (table set)))

(defmethod object-set-count ((set object-set))
  (hash-table-count (table set)))

(defmethod print-object ((set object-set) stream)
  (print-unreadable-object (set stream :type t)
    (cond ((plusp (object-set-count set))
           (format stream "[~{~A~^ ~}]" (object-set-list set)))
          (t
           (format stream "empty")))))

(defmethod make-object-set (initial-contents)
  (let ((set (make-instance 'object-set)))
    (dolist (object initial-contents set)
      (add-to-object-set object set))))

(defmethod add-to-object-set (object (set object-set))
  (setf (gethash (id object) (table set)) object))

(defmethod object-set-member-p (object (set object-set))
  (nth-value 1 (gethash (id object) (table set))))

(defmethod call-for-object-set-objects (fun (set object-set))
  (maphash (lambda (key object)
             (declare (ignore key))
             (funcall fun object))
           (table set)))

;;; Replication - the bulk is in transaction-streaming.lisp

(defgeneric replicate-transaction (transaction graph))

;;; Transaction conditions
(define-condition validation-conflict (error)
  ((transaction
    :initarg :transaction
    :reader validation-conflict-transaction)))

(define-condition no-transaction-in-progress (error) ())

(define-condition no-transaction-in-progress-warning (warning) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "No transaction in progress; copy cannot be saved"))))

(define-condition modifying-non-copy (error)
  ((node
    :initarg :node
    :reader modifying-non-copy-node))
  (:report (lambda (condition stream)
             (format stream "Modifying ~A without copying first"
                     (modifying-non-copy-node condition)))))

;;; Transaction manager
(defgeneric create-transaction (transaction-manager))
(defgeneric cleanup-transaction (transaction))

(defgeneric graph (object)
  (:documentation
   "Return the associated graph of OBJECT."))

(defgeneric overlapping-transactions (transaction transaction-manager)
  (:documentation
   "Return a list of committed transactions that may affect
   TRANSACTION."))

(defgeneric transaction-lock (transaction))

(defgeneric call-with-transaction-lock (transaction fun)
  (:method (transaction fun)
    ;; TODO: This can have a coarse lock during recovery and fine
    ;; locks during normal use
    (with-write-lock ((transaction-lock transaction))
      (funcall fun))))

(defmacro with-transaction-lock ((transaction) &body body)
  `(call-with-transaction-lock ,transaction
                               (lambda ()
                                 ,@body)))

(defgeneric assign-transaction-id (transaction transaction-manager)
  (:method (transaction transaction-manager)
    (let ((new-id (tx-id-counter transaction-manager)))
      (setf (transaction-id transaction) new-id)
      (incf (tx-id-counter transaction-manager))
      new-id)))

;;; Transactions
(defgeneric transaction-manager (object)
  (:documentation
   "Return the transaction manager of OBJECT."))

(defgeneric state (transaction))
(defgeneric (setf state) (transaction new-value))

(defgeneric sequence-number (transaction))
(defgeneric (setf sequence-number) (transaction new-value))

(defgeneric read-set (transaction))
(defgeneric create-set (transaction))
(defgeneric write-set (transaction))

(defgeneric local-cache (transaction))
(defgeneric graph-cache (transaction))

(defgeneric lookup-object (id table transaction graph)
  (:method (id table (transaction null) graph)
    (lookup-node table id graph))
  (:method (id table transaction (graph t))
    (let ((local-cache (local-cache transaction))
          (graph-cache (graph-cache transaction)))
      (let ((local (gethash id local-cache)))
        (if local
            local
            (let ((value (or (gethash id graph-cache)
                             (lookup-node table id (graph transaction)))))
              (when value
                (add-to-object-set value (read-set transaction))
                (setf (gethash id local-cache) value))))))))

(defgeneric write-object (object transaction))

(defgeneric writes (transaction)
  (:method (transaction)
    (append (object-set-list (create-set transaction))
            (object-set-list (write-set transaction)))))

(defgeneric write-count (transaction)
  (:method (transaction)
    (+ (object-set-count (create-set transaction))
       (object-set-count (write-set transaction)))))

(defgeneric (setf writes) (new-value transaction))

(defgeneric validate (transaction)
  (:method (transaction)
    (let ((write-set (write-set transaction)))
      (or (zerop (object-set-count write-set))
          (loop for other-transaction in (overlapping-transactions
                                          transaction
                                          (transaction-manager transaction))
             never (object-sets-intersect-p write-set
                                            (read-set other-transaction))
             never (object-sets-intersect-p write-set
                                            (write-set other-transaction)))))))

(defgeneric %commit (transaction))
(defgeneric %rollback (transaction))

(defgeneric call-with-transaction (fun transaction-manager)
  (:documentation "Call FUN with *TRANSACTION* bound to a new
  transaction created from TRANSACTION-MANAGER."))

(defmacro with-transaction ((&optional (transaction-manager '(transaction-manager *graph*)))
                            &body body)
  `(call-with-transaction (lambda () ,@body) ,transaction-manager))

(defclass tx ()
  ((read-set
    :initarg :read-set
    :reader read-set)
   (create-set
    :initarg :create-set
    :reader create-set)
   (write-set
    :initarg :write-set
    :reader write-set)
   (transaction-lock
    :initarg :transaction-lock
    :reader transaction-lock)
   (local-cache
    :initarg :local-cache
    :reader local-cache)
   (copies
    :initarg :copies
    :reader copies
    :documentation "A node to be modified must first be copied via
    COPY, which places it in this EQ hash table. UPDATE-NODE will
    refer to this copy when persisting the transaction.")
   (graph-cache
    :initarg :graph-cache
    :reader graph-cache)
   (graph
    :initarg :graph
    :reader graph)
   (transaction-manager
    :initarg :transaction-manager
    :reader transaction-manager)
   (state
    :initarg :state
    :accessor state)
   (sequence-number
    :initarg :sequence-number
    :accessor sequence-number)
   (start-tx-id
    :initarg :start-tx-id
    :reader start-tx-id
    :documentation "The value of the tx-id-counter when this
    transaction was created.")
   (finish-tx-id
    :initarg :finish-tx-id
    :accessor finish-tx-id
    :documentation "The value of the tx-id-counter when this
    transaction is ended.")
   (transaction-id
    :initarg :tx-id
    :accessor transaction-id
    :documentation "A transaction-id is assigned from the transaction
    manager tx-id-counter only after a transaction has been
    validated.")
   (bytes-components
    :initarg :bytes-components
    :initform '()
    :accessor bytes-components
    :documentation "A list of vectors that will be concatenated to
    form BYTES during persisting.")
   (bytes
    :initarg :bytes
    :accessor bytes
    :documentation "BYTES has a serialization of the transaction after
    it has been committed."))
  (:default-initargs
   :read-set (make-object-set nil)
    :create-set (make-object-set nil)
    :write-set (make-object-set nil)
    :transaction-lock (make-rw-lock)
    :local-cache (make-id-table)
    :copies (make-hash-table)
    :state :init))

(defmethod print-object ((transaction tx) stream)
  (print-unreadable-object (transaction stream :type t :identity t)
    (format stream "~D: ~D read~:P, ~D create~:P, ~D write~:P, ~S"
            (sequence-number transaction)
            (object-set-count (read-set transaction))
            (object-set-count (create-set transaction))
            (object-set-count (write-set transaction))
            (state transaction))))


;;; Applying transaction writes to the graph

(defun maybe-initialize-bytes (node)
  "Initialize the BYTES slot of NODE, if necessary."
  (let ((data (data node))
        (bytes (bytes node)))
    (when (and data
               (or (eql bytes :init)
                   (null bytes)))
      (setf (bytes node) (serialize data)))))

(defun maybe-allocate-for-node (node graph)
  "Allocate heap storage and initialize the data pointer for NODE, if
needed."
  (maybe-initialize-bytes node)
  (setf (data-pointer node)
        (if (data node)
            (allocate (heap graph) (length (bytes node)))
            0)))

;;; FIXME: Find a better home for this method
(defmethod set-bytes ((memory memory) vec offset length)
  (declare (type word offset length))
  (dotimes (i length)
    (set-byte memory (+ i offset) (aref vec i)))
  vec)

(defun maybe-write-to-heap (node graph)
  "Write the heap data for NODE to the heap, if necesssary. Nodes with
no data are not written."
  (let ((data-pointer (maybe-allocate-for-node node graph)))
    (unless (zerop data-pointer)
      (let ((bytes (bytes node)))
        (set-bytes (heap graph) bytes data-pointer (length bytes))))))

(defun maybe-free-from-heap (node graph)
  "Free the heap space used by NODE, if necessary."
  (let ((data-pointer (data-pointer node)))
    (unless (zerop data-pointer)
      (handler-case
          (free (heap graph) data-pointer)
        (error (c)
          (log:error "Unable to free ~A (~A): ~A" (string-id node) data-pointer c))))))

(defgeneric add-node-to-indexes (node graph &key unless-present)
  (:method ((node node) graph &key unless-present)
    (add-to-type-index node graph :unless-present unless-present)
    (setf (type-idx-written-p node) t))
  (:method ((node edge) graph &key unless-present)
    (call-next-method)
    (add-to-vev-index node graph :unless-present unless-present)
    (setf (vev-written-p node) t)
    (add-to-ve-index node graph :unless-present unless-present)
    (setf (ve-written-p node) t)))

;;; tx-writes have enough information to update the graph database and
;;; its views.

(defclass tx-write ()
  ((node
    :initarg :node
    :reader node)))

(defmethod id ((write tx-write))
  (id (node write)))

(defmethod print-object ((write tx-write) stream)
  (print-unreadable-object (write stream :type t)
    (format stream "for ~A ~A"
            (class-name (class-of (node write)))
            (string-id (id write)))))

(defclass tx-create (tx-write) ())

(defclass tx-update (tx-write)
  ((old-node
    :initarg :old-node
    :reader old-node)))

(defclass tx-delete (tx-update) ())

(defgeneric tx-write-table (object graph)
  (:method ((edge edge) graph)
    (edge-table graph))
  (:method ((vertex vertex) graph)
    (vertex-table graph))
  (:method ((write tx-write) graph)
    (tx-write-table (node write) graph)))

(defgeneric apply-tx-write (tx-write graph))

(defmethod apply-tx-write :after ((write tx-write) graph)
  (let ((node (node write)))
    (setf (gethash (id node) (cache graph)) node)))

(defmethod apply-tx-write ((write tx-create) graph)
  (let ((table (tx-write-table write graph))
        (node (node write)))
    (setf (revision node) 0)
    (maybe-write-to-heap node graph)
    (add-node-to-indexes node graph
                         :unless-present *add-to-indexes-unless-present-p*)
    (handler-case
        (lhash-insert table (id node) node)
      (duplicate-key-error (condition)
        (declare (ignore condition))
        (lhash-update table (id node) node)))
    (finalize-node node table graph))
  write)

(defmethod apply-tx-write ((write tx-update) graph)
  (let ((new-node (node write))
        (old-node (old-node write))
        (table (tx-write-table write graph)))
    (setf (revision new-node)
          (ldb (byte 32 0) (1+ (revision old-node))))
    (setf (bytes new-node)
          (serialize (data new-node)))
    (maybe-write-to-heap new-node graph)
    (lhash-update table (id new-node) new-node)
    ;; KTR: moved to post-view generation
    ;;(maybe-free-from-heap old-node graph)
    )
  write)


;;; Applying transaction view updates

(defgeneric call-for-applicable-views (fun graph node)
  (:method (fun graph (node node))
    (loop
       for view in (lookup-views graph node)
       when view do (funcall fun view))))

(defmacro do-applicable-views ((view graph node) &body body)
  `(call-for-applicable-views (lambda (,view) ,@body)
                              ,graph ,node))

(defgeneric applicable-views (node graph)
  (:method (node graph)
    (let ((result '()))
      (do-applicable-views (view graph node)
        (push view result))
      (nreverse result))))


(defgeneric apply-tx-write-to-views (write graph))

(defmethod apply-tx-write-to-views ((write tx-create) graph)
  (let ((node (node write)))
;;    (do-applicable-views (view graph node)
;;      (add-to-view graph view node))))
    (log:debug "Apply ~A to views for ~A" write (type-of node))
    (add-to-views graph node)))

(defmethod apply-tx-write-to-views ((write tx-update) graph)
  (let ((new-node (node write))
        (old-node (old-node write)))
;;    (do-applicable-views (view graph new-node)
;;      (remove-from-view graph view old-node)
;;      (add-to-view graph view new-node))))
    (log:debug "Apply ~A to views for ~A" write (type-of old-node))
    (update-in-views graph new-node old-node)))

(defmethod apply-tx-write-to-views ((write tx-delete) graph)
  (let ((node (node write)))
;;    (do-applicable-views (view graph node)
;;      (remove-from-view graph view node))))
    (remove-from-views graph node)))


;;; Applying the transaction

(defvar *highest-transaction-id-lock*
  (make-recursive-lock "transaction id file"))

(defgeneric highest-transaction-id-file (graph)
  (:method (graph)
    (make-pathname :name "transaction-id"
                   :type "dat"
                   :defaults (location graph))))

(defgeneric persist-highest-transaction-id (transaction-id graph)
  (:method (transaction-id graph)
    (let ((persist-file (highest-transaction-id-file graph))
          (serialized (make-byte-vector 8)))
      (serialize-uint64 serialized transaction-id 0)
      (with-open-file (stream persist-file
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-does-not-exist :create
                              :if-exists :overwrite)
        (with-recursive-lock-held (*highest-transaction-id-lock*)
          (write-sequence serialized stream)))
      transaction-id)))

(defgeneric load-highest-transaction-id (graph)
  (:method (graph)
    (let ((persist-file (highest-transaction-id-file graph))
          (serialized (make-byte-vector 8)))
      (with-recursive-lock-held (*highest-transaction-id-lock*)
        (if (probe-file persist-file)
            (with-open-file (stream persist-file
                                    :direction :input
                                    :element-type '(unsigned-byte 8))
              (let ((offset (read-sequence serialized stream)))
                (unless (= offset (length serialized))
                  (error "Bad read-sequence from transaction id file"))
                (deserialize-uint64 serialized 0)))
            0)))))

(defgeneric apply-tx-writes (writes graph)
  (:method (writes graph)
    (dolist (write writes)
      (apply-tx-write write graph))))

(defgeneric apply-tx-writes-to-views (writes graph)
  (:method (writes graph)
    (dolist (write writes)
      (apply-tx-write-to-views write graph))))

(defgeneric garbage-collect-heap (writes graph)
  (:method (writes graph)
    (let ((old-nodes (delete-duplicates
                      (mapcar 'old-node
                              (remove-if-not (lambda (write)
                                               (typep write 'tx-update))
                                             writes)))))
      (dolist (old-node old-nodes)
        (maybe-free-from-heap old-node graph)))))

(defgeneric apply-transaction (transaction graph)
  (:method (transaction graph)
    (with-transaction-lock (transaction)
      (let ((writes (writes transaction)))
        (apply-tx-writes writes graph)
        (apply-tx-writes-to-views writes graph)
        (garbage-collect-heap writes graph)
        (persist-highest-transaction-id (transaction-id transaction) graph)))))

(defmethod apply-transaction :after (transaction (graph master-graph))
  (replicate-transaction transaction graph))

;;;
;;; Serializing a transaction
;;;
;;; A transaction is serialized as a header chunk followed by a number
;;; of tx-write chunks.
;;;
;;; The transaction header is as follows:
;;;
;;;   - 8 bytes for the header size (fixed)
;;;
;;;   - 1 byte for flags; currently unused
;;;
;;;   - 1 byte for type (#\x72)
;;;
;;;   - 8 bytes for the transaction id
;;;
;;;   - 8 bytes for the tx-write count
;;;
;;;   - 8 bytes for the total size of following tx-writes
;;;

(defclass tx-header ()
  ((transaction-id
    :initarg :transaction-id
    :reader transaction-id)
   (write-count
    :initarg :write-count
    :accessor write-count)
   (write-size
    :initarg :write-size
    :accessor write-size
    :documentation "The number of bytes of serialized tx-write data
    following the tx-header.")
   (writes
    :initarg :writes
    :accessor writes)
   (graph
    :initarg :graph
    :accessor graph)))

(alexandria:define-constant +tx-header-size+ (+ 8 1 1 8 8 8))
(alexandria:define-constant +tx-header-type-code+ (char-code #\t))

(defun make-tx-header-vector ()
  (make-byte-vector +tx-header-size+))

(defun serialize-tx-header (tx vector offset)
  (serialize-uint64 vector +tx-header-size+ offset)
  (incf offset 8)
  ;; Skip flags
  (incf offset)
  (setf (aref vector offset) +tx-header-type-code+)
  (incf offset)
  (serialize-uint64 vector (transaction-id tx) offset)
  (incf offset 8)
  (let ((writes (writes tx)))
    (serialize-uint64 vector (length writes) offset)
    (incf offset 8)
    (let ((total-size (reduce #'+ writes :key 'tx-write-vector-size)))
      (serialize-uint64 vector total-size offset)
      (incf offset 8)
      (values vector offset))))

(defun deserialize-tx-header-vector (vector)
  ;; Skip the size and flags and type
  (let ((offset 10))
    (let ((transaction-id (deserialize-uint64 vector offset))
          (write-count (deserialize-uint64 vector (+ offset 8)))
          (write-size (deserialize-uint64 vector (+ offset 16))))
      (make-instance 'tx-header
                     :transaction-id transaction-id
                     :write-count write-count
                     :write-size write-size))))

(defun tx-header-vector (tx)
  (let ((vector (make-tx-header-vector)))
    (values (serialize-tx-header tx vector 0))))

(defun read-uint64-sized-vector (stream)
  (let ((size-vector (make-byte-vector 8)))
    (let ((last-position (read-sequence size-vector stream)))
      (when (= 0 last-position)
        (return-from read-uint64-sized-vector nil))
      (unless (= 8 last-position)
        (error "Could not read size information from ~A" stream))
      (let* ((size (deserialize-uint64 size-vector 0))
             (vector (make-byte-vector size)))
        (setf last-position (read-sequence vector stream :start 8))
        (unless (= last-position size)
          (error "Could not read to ~D  (got ~D) bytes from ~A"
                 size last-position stream))
        (replace vector size-vector)
        vector))))


;;; Serializing tx-writes
;;;
;;; A tx-write consists of a header followed by one or two serialized
;;; nodes.
;;;
;;; tx-write header:
;;;
;;;   - 8 bytes for total tx-write size
;;;
;;;   - 1 byte for flags; currently unused
;;;
;;;   - 1 byte for type (#x63 for create, #x75 for update, #x64 for delete)
;;;
;;;   - 1 byte for node count
;;;
;;; A node:
;;;
;;;   - 8 bytes for size
;;;
;;;   - 1 byte flags; currently unused
;;;
;;;   - 1 byte for type (#x65 for edge, #x76 for vertex)
;;;
;;;   - 16 bytes for uuid
;;;
;;;   - 1 bytes for node header size
;;;
;;;   - N bytes for node header
;;;
;;;   - M bytes for node heap value
;;;



;;; Serialize a single node

(alexandria:define-constant +transaction-node-base-header-size+
    (+ 8 1 16 1 1))
(alexandria:define-constant +transaction-node-edge-code+ (char-code #\e))
(alexandria:define-constant +transaction-node-vertex-code+ (char-code #\v))

(defun transaction-node-header-size (node)
  (etypecase node
    (edge +edge-header-size+)
    (vertex +node-header-size+)))

(defun transaction-node-vector-size (node)
  (+ +transaction-node-base-header-size+
     (transaction-node-header-size node)
     (if (typep (bytes node) 'sequence)
         (length (bytes node))
         0)))

(defun transaction-node-type-code (node)
  (etypecase node
    (edge +transaction-node-edge-code+)
    (vertex +transaction-node-vertex-code+)))

(defun serialize-transaction-node-header (node vector offset)
  (etypecase node
    (edge
     (serialize-edge-head vector node offset))
    (vertex
     (serialize-node-head vector node offset))))

(defun serialize-transaction-uuid (uuid vector offset)
  (replace vector uuid :start1 offset))

(defun deserialize-transaction-uuid (vector offset)
  (subseq vector offset (+ offset 16)))

(defun serialize-transaction-node (node vector offset)
  (let* ((size (transaction-node-vector-size node))
         (type-code (transaction-node-type-code node))
         (header-size (transaction-node-header-size node))
         (bytes (bytes node))
         (flags 0))
    ;; 8 byte size
    (serialize-uint64 vector size offset)
    (incf offset 8)
    ;; 1 byte (unused) flags
    (set-byte vector offset flags)
    (incf offset)
    ;; 1 byte type
    (set-byte vector offset type-code)
    (incf offset)
    ;; 16 byte uuid
    (serialize-transaction-uuid (id node) vector offset )
    (incf offset 16)
    ;; 1 byte node header size
    (set-byte vector offset header-size)
    (incf offset)
    ;; header-size bytes of node header
    (serialize-transaction-node-header node vector offset)
    (incf offset header-size)
    ;; (length bytes) of node bytes
    (when (typep bytes 'sequence)
      (replace vector bytes :start1 offset)
      (incf offset (length bytes)))
    offset))

(defun transaction-node-vector (node)
  (let* ((size (transaction-node-vector-size node))
         (vector (make-byte-vector size))
         (offset 0))
    (serialize-transaction-node node vector offset)
    vector))

(defun deserialize-edge-transaction-node-vector (vector id
                                                 header-offset
                                                 data-offset
                                                 end)
  (let* ((edge (deserialize-edge-head vector header-offset))
         (bytes (subseq vector data-offset end)))
    (setf (id edge) id)
    (if (> (length bytes) 0)
        (progn
          (setf (data edge) (deserialize bytes))
          (setf (bytes edge) bytes))
        (setf (data edge) nil))
    edge))

(defun deserialize-vertex-transaction-node-vector (vector id
                                                   header-offset
                                                   data-offset
                                                   end)
  (let ((vertex (deserialize-vertex-head vector header-offset))
        (bytes (subseq vector data-offset end)))
    (setf (id vertex) id)
    (if (> (length bytes) 0)
        (progn
          (setf (data vertex) (deserialize bytes))
          (setf (bytes vertex) bytes))
        (setf (data vertex) nil))
    vertex))

(defun deserialize-transaction-node-vector (vector &optional (offset 0))
  "Return the edge or vertex represented by VECTOR."
  (let (size uuid type header-size end)
    (setf size (deserialize-uint64 vector offset))
    (setf end (+ offset size))
    (incf offset 8)
    ;; Skip flags
    (incf offset)
    (setf type (get-byte vector offset))
    (incf offset)
    (setf uuid (deserialize-transaction-uuid vector offset))
    (incf offset 16)
    (setf header-size (get-byte vector offset))
    (incf offset)
    (let* ((header-offset offset)
           (data-offset (+ offset header-size))
           (node
            (cond ((eql type +transaction-node-edge-code+)
                   (deserialize-edge-transaction-node-vector vector
                                                             uuid
                                                             header-offset
                                                             data-offset
                                                             end))
                  ((eql type +transaction-node-vertex-code+)
                   (deserialize-vertex-transaction-node-vector vector
                                                               uuid
                                                               header-offset
                                                               data-offset
                                                               end))
                  (t
                   (error "Unknown transaction node type ~S" type)))))
      (values node end))))

;;; Serialize a tx-write

(alexandria:define-constant +tx-write-header-size+ (+ 8 1 1 1))
(alexandria:define-constant +tx-write-create-code+ (char-code #\c))
(alexandria:define-constant +tx-write-update-code+ (char-code #\u))
(alexandria:define-constant +tx-write-delete-code+ (char-code #\d))

(defun tx-write-vector-size (tx-write)
  (+ +tx-write-header-size+
     (transaction-node-vector-size (node tx-write))
     (if (typep tx-write 'tx-update)
         (transaction-node-vector-size (old-node tx-write))
         0)))

(defgeneric tx-write-vector-code (tx-write)
  (:method ((write tx-create))
    +tx-write-create-code+)
  (:method ((write tx-update))
    +tx-write-update-code+)
  (:method ((write tx-delete))
    +tx-write-delete-code+))

(defgeneric tx-write-node-count (tx-write)
  (:method ((write tx-write))
    1)
  (:method ((write tx-update))
    2))

(defun tx-write-vector (tx-write)
  "Serialize TX-WRITE to a byte vector and return the vector."
  (let* ((size (tx-write-vector-size tx-write))
         (vector (make-byte-vector size))
         (node-count (tx-write-node-count tx-write))
         (offset 0))
    ;; 8 byte size
    (serialize-uint64 vector size offset)
    (incf offset 8)
    ;; 1 byte flag (unused)
    (incf offset 1)
    ;; 1 byte type code
    (set-byte vector offset (tx-write-vector-code tx-write))
    (incf offset 1)
    ;; 1 byte count
    (set-byte vector offset node-count)
    (incf offset 1)
    ;; Nodes
    (setf offset (serialize-transaction-node (node tx-write)
                                             vector
                                             offset))
    (when (= 2 node-count)
      (setf offset (serialize-transaction-node (old-node tx-write)
                                               vector
                                               offset)))
    (values vector offset)))

(defun tx-write-class (type-code)
  "Return the class name designated by TYPE-CODE."
  (cond ((eql type-code +tx-write-create-code+)
         'tx-create)
        ((eql type-code +tx-write-update-code+)
         'tx-update)
        ((eql type-code +tx-write-delete-code+)
         'tx-delete)
        (t
         (error "Unknown type code ~S" type-code))))

(defun deserialize-tx-write-vector (vector)
  ;; Skip the size and flags
  (let* ((offset 9)
         type-code count node class)
    (setf type-code (get-byte vector offset))
    (setf class (tx-write-class type-code))
    (incf offset)
    (setf count (get-byte vector offset))
    (incf offset)
    (setf (values node offset)
          (deserialize-transaction-node-vector vector offset))
    (if (= count 2)
        (let ((old-node (deserialize-transaction-node-vector vector offset)))
          ;; get local data-pointer to replace the one read from the txn file
          (log:debug "FINDING PROPER DATA-POINTER FOR ~A (~A)"
                     (id old-node) (data-pointer old-node))
          (let ((local-old-node (if (vertex-p old-node)
                                    (lookup-vertex (id old-node))
                                    (lookup-edge (id old-node)))))
            (log:debug "SETTING DATA-POINTER OF ~A TO ~A"
                       (id old-node)
                       (data-pointer local-old-node))
            (setf (data-pointer old-node) (data-pointer local-old-node)))
          (make-instance class
                         :node node
                         :old-node old-node))
        (make-instance class :node node))))

;;; Saving tx-writes to a file

(defun save-bytes-component-vector (vector tx)
  (push vector (bytes-components tx)))

(defun write-tx-writes-to-stream (tx stream)
  (dolist (write (writes tx))
    (let ((v (tx-write-vector write)))
      (save-bytes-component-vector v tx)
      (write-sequence (tx-write-vector write) stream))))

(defun write-tx-header-to-stream (tx stream)
  (let ((v (tx-header-vector tx)))
    (save-bytes-component-vector v tx)
    (write-sequence v stream)))

(defun initialize-bytes-from-components (tx)
  (let* ((components (bytes-components tx))
         (length (reduce #'+ components :key 'length))
         (bytes (make-byte-vector length))
         (offset 0))
    (dolist (component (reverse components))
      (replace bytes component :start1 offset)
      (incf offset (length component)))
    (setf (bytes-components tx) nil)
    (setf (bytes tx) bytes)))

(defgeneric persist-tx (transaction initial-file final-file)
  (:documentation
   "Persist TRANSACTION to INITIAL-FILE. After the transaction has
   been completely written and INITIAL-FILE is closed, INITIAL-FILE is
   reanamed to FINAL-FILE. The intent is to make it atomically clear
   that the transaction file has a complete complete set of related
   changes. After transaction has persisted, its BYTES slot contains a
   serialization of the transaction.")
  (:method (transaction initial-file final-file)
    (with-open-file (stream initial-file :direction :output
                            :if-exists :error
                            :element-type '(unsigned-byte 8))
      (write-tx-header-to-stream transaction stream)
      (write-tx-writes-to-stream transaction stream))
    (initialize-bytes-from-components transaction)
    (rename-file initial-file final-file)
    final-file))

(defun load-tx-header (stream)
  (let ((vector (read-uint64-sized-vector stream)))
    (when vector
      (deserialize-tx-header-vector vector))))

(defun load-one-tx-write (stream)
  "Load a single write node from STREAM. Returns NIL if no writes are
left in the stream."
  (let ((vector (read-uint64-sized-vector stream)))
    (when vector
      (deserialize-tx-write-vector vector))))

(defun load-tx-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((tx-header (load-tx-header stream)))
      (setf (writes tx-header)
            (loop for i from 0
               repeat (write-count tx-header)
               for node = (load-one-tx-write stream)
               unless node do (error "Too few writes in ~A: expected ~A, ended at ~A"
                                     file
                                     (write-count tx-header)
                                     i)
               collect node))
      tx-header)))

(defgeneric persistent-transaction-directory (graph)
  (:method (graph)
    (merge-pathnames "tx/" (location graph))))

(defgeneric transaction-pathname (transaction)
  (:method (transaction)
    (make-pathname :name (format nil "~16,'0X" (transaction-id transaction))
                   :type "txn"
                   :defaults (persistent-transaction-directory
                              (graph transaction)))))

(defgeneric transaction-temporary-pathname (transaction)
  (:method (transaction)
    (make-pathname :type "txn-tmp"
                   :defaults (transaction-pathname transaction))))

(defgeneric persist-transaction (transaction)
  (:method (transaction)
    (persist-tx transaction
                (transaction-temporary-pathname transaction)
                (transaction-pathname transaction))
    (let* ((transaction-manager (transaction-manager transaction))
           (stream (replication-log transaction-manager))
           (lock (replication-log-lock transaction-manager)))
      (with-recursive-lock-held (lock)
        (write-sequence (bytes transaction)
                        (replication-log (transaction-manager transaction)))
        (finish-output stream)))))

;;; Locking for object sets

(defun ordered-bucket-locks (lhash keys)
  "Return a fresh list of locks for LHASH in ascending bucket order."
  (let* ((level (read-lhash-level lhash))
         (buckets (mapcar (lambda (key)
                            (hash lhash level key))
                          keys)))
    (remove-duplicates
     (mapcar (lambda (bucket)
               (lookup-lhash-lock lhash bucket))
             (sort buckets #'<)))))


(defun tx-writes-locks (writes graph)
  (let ((vertexes '())
        (edges '()))
    (loop for write in writes
       for node = (node write)
       do
         (if (vertex-p node)
             (push (id node) vertexes)
             (push (id node) edges)))
    (let ((vertex-locks (ordered-bucket-locks (vertex-table graph) vertexes))
          (edge-locks (ordered-bucket-locks (edge-table graph) edges)))
      (nconc vertex-locks edge-locks))))

(defun call-with-locks (locks fun)
  (if (endp locks)
      (funcall fun)
      (with-lock ((first locks))
        (call-with-locks (cdr locks) fun))))




(defmacro ensure-transaction ((transaction-manager) &body body)
  `(if *transaction*
       (call-next-method)
       (with-transaction (,transaction-manager) ,@body )))

(defgeneric %create-node (node graph transaction)
  (:method (node graph transaction)
    (add-to-object-set (make-instance 'tx-create :node node)
                       (create-set *transaction*))
    node))

(defgeneric create-node (node graph)
  (:method (node graph)
    (%create-node node graph *transaction*)))

(defmethod create-node :around (node graph)
  (ensure-transaction ((transaction-manager graph))
    (call-next-method)))

(defgeneric copy-node (node)
  (:method ((node node))
    (maybe-init-node-data node)
    (let ((new-node (make-instance (type-of node)
                                   :id (slot-value node 'id)
                                   :type-id (slot-value node 'type-id)
                                   :revision (slot-value node 'revision)
                                   :deleted-p (slot-value node 'deleted-p)
                                   :written-p (slot-value node 'written-p)
                                   :data-pointer (slot-value node 'data-pointer))))
      (setf (data new-node) (copy-tree (slot-value node 'data)))
      (if *transaction*
          (setf (gethash new-node (copies *transaction*)) node)
          (warn 'no-transaction-in-progress-warning))
      new-node)))

(defgeneric update-node (new-node graph)
  (:method (new-node graph)
    ;; This does not automatically ensure a transaction, because you
    ;; have to COPY any node you want to modify within a transaction
    ;; anyway. That compound action inhibits auto-wrapping.
    (unless *transaction*
      (error 'no-transaction-in-progress))
    (let ((old-node (gethash new-node (copies *transaction*))))
      (unless old-node
        (error 'modifying-non-copy
               :node new-node))
      (add-to-object-set (make-instance 'tx-update
                                        :node new-node
                                        :old-node old-node)
                         (write-set *transaction*))
      new-node)))

(defgeneric delete-node (node graph)
  (:method (node graph)
    (let ((old-node node)
          (new-node (copy node)))
      (setf (bytes new-node) (bytes old-node))
      (setf (deleted-p new-node) t)
      (add-to-object-set (make-instance 'tx-delete
                                        :old-node old-node
                                        :node new-node)
                         (write-set *transaction*)))))

(defmethod delete-node :around (node graph)
  (ensure-transaction ((transaction-manager graph))
    (call-next-method)))


(defclass transaction-manager ()
  ((sequence-number
    :initarg :sequence-number
    :accessor sequence-number
    :initform 0)
   (tx-id-counter
    :initarg :tx-id-counter
    :accessor tx-id-counter
    :initform 0)
   (transactions
    :initarg :transactions
    :reader transactions
    :initform (make-hash-table))
   (lock
    :initarg :lock
    :reader lock
    :initform (make-recursive-lock "transaction manager lock"))
   (replication-log-file
    :initarg :replication-log-file
    :accessor replication-log-file
    :initform nil)
   (replication-log
    :initarg :replication-log
    :accessor replication-log
    :documentation "An open stream appending to the replication log
    file."
    :initform nil)
   (replication-log-lock
    :initarg :replication-log-lock
    :reader replication-log-lock
    :initform (make-recursive-lock "replication log lock"))
   (graph
    :initarg :graph
    :reader graph
    :initform *graph*)))

(defmethod print-object ((transaction-manager transaction-manager) stream)
  (print-unreadable-object (transaction-manager stream :type t)
    (format stream "~D transaction~:P, sequence number ~D"
            (hash-table-count (transactions transaction-manager))
            (sequence-number transaction-manager))))

(defmethod initialize-instance :after ((instance transaction-manager)
                                       &key &allow-other-keys)
  (let* ((graph (graph instance))
         (tx-id-counter (1+ (load-highest-transaction-id graph))))
    (setf (tx-id-counter instance) tx-id-counter)
    (setf (replication-log-file instance)
          (make-pathname :name (format nil "replication-~16,'0X"
                                       tx-id-counter)
                         :type "log"
                         :defaults (persistent-transaction-directory graph)))))

(defmethod replication-log-file ((graph graph))
  (replication-log-file (transaction-manager graph)))

(defgeneric init-replication-log (graph)
  (:method (graph)
    (let* ((transaction-manager (transaction-manager graph))
           (file (replication-log-file transaction-manager))
           (stream (open file
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :append
                         :if-does-not-exist :create)))
      (setf (replication-log (transaction-manager graph)) stream))))

(defgeneric close-replication-log (graph)
  (:method (graph)
    (let* ((transaction-manager (transaction-manager graph))
           (stream (replication-log transaction-manager)))
      (when (and (streamp stream)
                 (open-stream-p stream))
        (close stream)
        (setf (replication-log transaction-manager) nil)))))

(defgeneric call-with-transaction-manager-lock (fun transaction-manager)
  (:method (fun transaction-manager)
    (with-recursive-lock-held ((lock transaction-manager))
      (funcall fun))))

(defmacro with-transaction-manager-lock ((transaction-manager) &body body)
  `(call-with-transaction-manager-lock (lambda () ,@body)
                                       ,transaction-manager))

(defmethod call-with-transaction (fun transaction-manager)
  (let ((completed nil)
        (attempt-count 0))
    (flet ((call-transaction-fun ()
             (let ((*transaction* (create-transaction transaction-manager)))
               (unwind-protect
                    (prog1
                        (funcall fun)
                      (setf completed t))
                 (when completed
                   (funcall *end-of-transaction-action* *transaction*))
                 (cleanup-transaction *transaction*)))))
      (loop
         (when (<= *maximum-transaction-attempts* attempt-count)
           (with-transaction-manager-lock (transaction-manager)
             (return (call-transaction-fun))))
         (handler-case
             (return (call-transaction-fun))
           (validation-conflict ()
             (incf attempt-count)))))))

(defgeneric add-transaction (transaction transaction-manager)
  (:method (transaction transaction-manager)
    (setf (gethash (sequence-number transaction)
                   (transactions transaction-manager))
          transaction)))

(defgeneric call-for-transactions (fun transaction-manager)
  (:method (fun (transaction-manager transaction-manager))
    (maphash (lambda (sequence-number tx)
               (declare (ignore sequence-number))
               (funcall fun tx))
             (transactions transaction-manager))))

(defmacro do-transactions ((transaction transaction-manager) &body body)
  `(call-for-transactions (lambda (,transaction) ,@body)
                          ,transaction-manager))

(defmacro do-committed-transactions ((transaction transaction-manager)
                                     &body body)
  `(call-for-transactions (lambda (,transaction)
                            (when (transaction-id ,transaction)
                              ,@body))
                          ,transaction-manager))

(defmacro do-active-transactions ((transaction transaction-manager)
                                  &body body)
  `(call-for-transactions (lambda (,transaction)
                            (when (eql (state ,transaction) :active)
                              ,@body))
                          ,transaction-manager))

(defmethod overlapping-transactions (transaction transaction-manager)
  (let ((start (start-tx-id transaction))
        (finish (finish-tx-id transaction))
        (result '()))
    (do-committed-transactions (tx transaction-manager)
      (when (<= start (transaction-id tx) finish)
        (push tx result)))
    result))

(defun minimum-start-transaction-id (transaction-manager)
  (let (min)
    (do-active-transactions (tx transaction-manager)
      (let ((start (start-tx-id tx)))
        (if min
            (setf min (min start min))
            (setf min start))))
    min))

(defun prune-committed-transactions (transaction-manager)
  (let ((min-id (minimum-start-transaction-id transaction-manager)))
    (do-committed-transactions (tx transaction-manager)
      (when (or (not min-id)
                (< (transaction-id tx) min-id))
        (remove-transaction tx transaction-manager)))))

(defgeneric remove-transaction (transaction transaction-manager)
  (:method (transaction transaction-manager)
    (remhash (sequence-number transaction)
             (transactions transaction-manager))))

(defgeneric next-sequence-number (transaction-manager)
  (:method (transaction-manager)
    (incf (sequence-number transaction-manager))))

(defmethod create-transaction (transaction-manager)
  (with-recursive-lock-held ((lock transaction-manager))
    (let* ((sequence-number (next-sequence-number transaction-manager))
           (graph (graph transaction-manager))
           (cache (cache graph))
           (tx (make-instance 'tx
                              :sequence-number sequence-number
                              :start-tx-id (tx-id-counter transaction-manager)
                              :finish-tx-id nil
                              :tx-id nil
                              :transaction-manager transaction-manager
                              :graph graph
                              :graph-cache cache)))
      (add-transaction tx transaction-manager)
      (setf (state tx) :active)
      tx)))

;;; Commit sequence

(defvar *delete-committed-transaction-files* t)

(defun mark-as-committed (file)
  (if *delete-committed-transaction-files*
      (delete-file file)
      (let ((committed (make-pathname :type "committed" :defaults file)))
        (rename-file file committed))))

(defmethod %rollback ((tx tx))
  (unless (eql (state tx) :active)
    (error "Transaction ~A is not active" tx))
  (setf (state tx) :aborted))

(defmethod %commit ((tx tx))
  (when (eql (state tx) :active)
    (let ((tm (transaction-manager tx)))
      (setf (state tx) :committing)
      (setf (finish-tx-id tx) (tx-id-counter tm))
      (with-transaction-manager-lock (tm)
        (unless (validate tx)
          (error 'validation-conflict :transaction tx))
        (setf (transaction-id tx) (tx-id-counter tm))
        (incf (tx-id-counter tm))
        (prune-committed-transactions tm)
        (persist-transaction tx)))
    (apply-transaction tx (graph tx))
    (setf (state tx) :committed)))

(defmethod cleanup-transaction ((tx tx))
  (let ((transaction-manager (transaction-manager tx)))
    (if (eql (state tx) :committed)
        (mark-as-committed (transaction-pathname tx))
        (remove-transaction tx transaction-manager))))


(defun commit (&optional (transaction *transaction*))
  (unless *transaction*
    (error 'no-transaction-in-progress))
  (%commit transaction))

(defun rollback (&optional (transaction *transaction*))
  (unless *transaction*
    (error 'no-transaction-in-progress))
  (%rollback transaction))


;;; Recovering/restoring transactions

(defclass recovery-transaction ()
  ((transaction-id
    :initarg :transaction-id
    :accessor transaction-id)
   (writes
    :initarg :writes
    :accessor writes)))

(defun load-recovery-transaction (file)
  (let ((tx-header (load-tx-file file)))
    (make-instance 'recovery-transaction
                   :transaction-id (transaction-id tx-header)
                   :writes (writes tx-header))))

(defmethod call-with-transaction-lock ((transaction recovery-transaction) fun)
  ;; No locking during recovery
  (funcall fun))

(defgeneric recovery-transaction-files (graph)
  (:method (graph)
    (let* ((directory (persistent-transaction-directory graph))
           (files (directory
                   (make-pathname :name :wild
                                  :type "txn"
                                  :defaults directory))))
      (sort files #'string< :key 'pathname-name))))

(defgeneric recovery-transactions (graph)
  (:method (graph)
    (mapcar 'load-recovery-transaction
            (recovery-transaction-files graph))))

(defgeneric recover-transactions (graph)
  (:method (graph)
    (dolist (transaction-file (recovery-transaction-files graph))
      (let ((transaction (load-recovery-transaction transaction-file))
            (*add-to-indexes-unless-present-p* t))
        (apply-transaction transaction graph)
        (mark-as-committed transaction-file)))))

(defclass restore-transaction (recovery-transaction) ()
  (:default-initargs
   :writes nil))

(defmethod %create-node (node graph (transaction recovery-transaction))
  (push (make-instance 'tx-create :node node) (writes transaction)))

(defclass replicated-transaction (recovery-transaction)
  ((graph
    :initarg :graph
    :reader graph))
  (:default-initargs
   :writes nil))
