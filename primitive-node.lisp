(in-package :graph-db)

(alexandria:define-constant +node-header-size+ 15)

(defgeneric node-p (thing)
  (:method ((thing node)) t)
  (:method (thing) nil))

(defmethod print-object ((node node) stream)
  (format stream "#<~A ~S REV ~S>"
          (type-of node) (uuid:byte-array-to-uuid (id node))
          (revision node)))

(defmethod flags-as-int ((n node))
  (let ((flags 0))
    (when (slot-value n 'deleted-p)
      (setq flags (dpb 1 (byte 1 0) flags)))
    (when (slot-value n 'written-p)
      (setq flags (dpb 1 (byte 1 1) flags)))
    (when (slot-value n 'heap-written-p)
      (setq flags (dpb 1 (byte 1 2) flags)))
    (when (slot-value n 'type-idx-written-p)
      (setq flags (dpb 1 (byte 1 3) flags)))
    (when (slot-value n 'views-written-p)
      (setq flags (dpb 1 (byte 1 4) flags)))
    (when (slot-value n 've-written-p)
      (setq flags (dpb 1 (byte 1 5) flags)))
    (when (slot-value n 'vev-written-p)
      (setq flags (dpb 1 (byte 1 6) flags)))
    flags))

(defmethod save-node-flags ((table lhash) (node node))
  (lhash-custom-update table
                       (lambda (mf offset)
                         (let ((flags (flags-as-int node)))
                           (set-byte mf offset flags)))
                       (id node)))

(defun serialize-node-head (mf n offset)
  ;; flags: deleted-p, written-p
  (let ((flags (flags-as-int n)))
    (set-byte mf offset flags))
  ;; type-id
  (dotimes (i 2)
    (set-byte mf (incf offset) (ldb (byte 8 (* i 8)) (type-id n))))
  ;; revision
  (dotimes (i 4)
    (set-byte mf (incf offset) (ldb (byte 8 (* i 8)) (revision n))))
  ;; data-pointer
  (dotimes (i 8)
    (set-byte mf (incf offset) (ldb (byte 8 (* i 8)) (data-pointer n))))
  offset)

(defun deserialize-node-head (mf offset)
  (let ((flags (get-byte mf offset)))
    (values
     (ldb-test (byte 1 0) flags) ;; deleted-p
     (ldb-test (byte 1 1) flags) ;; written-p
     (ldb-test (byte 1 2) flags) ;; heap-written-p
     (ldb-test (byte 1 3) flags) ;; type-idx-written-p
     (ldb-test (byte 1 4) flags) ;; views-written-p
     (ldb-test (byte 1 5) flags) ;; ve-written-p
     (ldb-test (byte 1 6) flags) ;; vev-written-p
     (let ((int 0)) ;; type-id
       (declare (type (integer 0 65535) int))
       (dotimes (i 2)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; revision
       (declare (type (integer 0 4294967295) int))
       (dotimes (i 4)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     (let ((int 0)) ;; data-pointer
       #+sbcl (declare (type sb-ext:word int))
       (dotimes (i 8)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     offset)))

(defun finalize-node (node table graph)
  (setf (written-p node) t)
  (save-node-flags table node)
  (setf (gethash (id node) (cache graph)) node))

;;; FIXME: reenable when deallocation bug is fixed
;;; (declaim (inline maybe-init-node-data))
(defun maybe-init-node-data (node &key (graph *graph*))
  (when (> (data-pointer node) 0)
    (when (or (eq (bytes node) :init) (null (bytes node)))
      (let ((bytes (read-bytes (make-mpointer
                                :mmap (memory-mmap (heap graph))
                                :loc (data-pointer node)))))
        (when bytes
          (setf (data node)
                (deserialize bytes)
                (bytes node)
                bytes)))))
  node)

(defmethod lookup-node ((table lhash) (key array) (graph graph))
  (or (and *cache-enabled*
           (let ((node (gethash key (cache graph))))
             (when node
               (record-graph-read)
               node)))
      (let ((node (lhash-get table key)))
        (when (node-p node)
          (setf (id node) key)
          ;; we should wait to do this until we need the data.
          ;;(maybe-init-node-data node :graph graph)
          (when *cache-enabled*
            (setf (gethash key (cache graph)) node))
          (record-graph-read)
          node))))

(defun save-node (node table &key (graph *graph*))
  ;; you must copy the node before writing to its slots,
  ;; in case others are reading it!
  ;;(log:info "SAVING ~A" (string-id node))
  (let ((old-node nil))
    (when (plusp (data-pointer node))
      (if (data node)
          (setf (bytes node) (serialize (data node)))
          (maybe-init-node-data node :graph graph))
      (let ((addr (allocate (heap graph) (length (bytes node)))))
        (dotimes (i (length (bytes node)))
          (set-byte (heap graph)
                    (+ i addr) (aref (bytes node) i)))
        (setf (data-pointer node) addr)))
    (with-locked-hash-key (table (id node))
      ;; Assure that the old node is available in cache for readers
      (setq old-node (%lhash-get table (id node)))
      (setf (id old-node) (id node))
      ;;(log:info "~A OLD ADDRESS IS ~A" (string-id node) (data-pointer old-node))
      ;;(log:info "~A NEW ADDRESS IS ~A" (string-id node) (data-pointer node))
      (maybe-init-node-data old-node :graph graph)
      (setf (gethash (id old-node) (cache graph)) old-node)
      ;;(log:info "OLD REV OF ~A CACHED" (string-id old-node))
      ;; Make sure we are modifying the latest revision or bail
      (unless (= (revision node) (revision old-node))
        ;; Undo data slot saving
        (log:error "REVISION ERROR ~A: ~A /= ~A"
                   (string-id node) (revision node) (revision old-node))
        (when (/= 0 (data-pointer node))
          (free (heap graph) (data-pointer node)))
        (error 'stale-revision-error
               :current-revision (revision old-node)
               :instance (string-id node)))
      ;; Save our changes
      ;; Don't overflow the revision (32 bit int)
      (if (>= (revision node) 4294967295)
          (setf (revision node) 0)
          (incf (revision node)))
      ;;(log:info "UPDATING LHASH FOR ~A" (string-id node))
      (%lhash-update table (id node) node)
      ;;(log:info "RECORDING WRITE FOR ~A" (string-id node))
      (record-graph-write)
      ;;(log:info "CACHING NEW REV OF ~A" (string-id node))
      (setf (gethash (id node) (cache graph)) node))
    (when (/= 0 (data-pointer old-node))
      (let ((pointer (data-pointer old-node))
            ;;(old-id (string-id old-node))
            (heap (heap graph)))
        ;;(log:info "~A FREEING ~A IN ~A" old-id pointer heap)
        (free heap pointer)
        ;; Remove from heap upon gc
        ;;(sb-ext:finalize old-node
        ;;                 (lambda ()
        ;;                   (let ((*package* (find-package :graph-db)))
        ;;                     (log:debug "~A FREEING ~A IN ~A" old-id pointer heap)
        ;;                     (free heap pointer))))
        ))
    ;;(log:info "SAVED ~A" (string-id node))
    (values node old-node)))

(defmethod string-id ((id array))
  (with-output-to-string (out)
    (map nil
         (lambda (byte)
           (format out "~(~2,'0X~)" byte))
         id)))

(defmethod string-id ((node node))
  (with-output-to-string (out)
    (map nil
         (lambda (byte)
           (format out "~(~2,'0X~)" byte))
         (id node))))

(defun node-to-alist (node &key slots)
  (maybe-init-node-data node)
  (remove-if (lambda (pair)
               (member (car pair) slots))
             (append (list (cons :id (string-id node)))
                     (copy-alist (data node)))))

(defun node-slot-value (node key &key (graph *graph*))
  ;;(log:info "GETTING ~A FOR ~A" key (string-id node))
  (maybe-init-node-data node :graph graph)
  (when (consp (data node))
    (cdr (assoc (if (keywordp key)
                    key
                    (intern (symbol-name key) :keyword))
                (data node)))))

(defsetf node-slot-value (node key) (value)
  (with-gensyms (pair keyword)
    `(if (typep ,value 'graph-db::node)
         (error "Cannot set ~A slots to objects with parent type NODE" (type-of ,node))
         (let ((,keyword (if (keywordp ,key)
                             ,key
                             (intern (symbol-name ,key) :keyword))))
           (maybe-init-node-data ,node)
           (cond ((null (data ,node))
                  (push (cons ,keyword ,value) (data ,node)))
                 ((consp (data ,node))
                  (let ((,pair (assoc ,keyword (data ,node))))
                    (if ,pair
                        (setf (cdr ,pair) ,value)
                        (push (cons ,keyword ,value) (data ,node)))))
                 (t
                  (error "Cannot set slot value when data slot is of type ~A"
                         (type-of (data ,node)))))))))

(defmethod slot-value-using-class :around ((class node-class) instance slot)
  "Around method that is alternate-version aware and will show values for the current,
   working private version of instance."
  ;;(log:debug "slot-value-using-class~%  '~A'~%  '~A'" class
  ;;(sb-pcl:slot-definition-name slot))
  (let* ((slot-name (slot-definition-name slot))
         (slot-keyword-name (intern (symbol-name slot-name) :keyword)))
    (cond ((member slot-name (persistent-slot-names class))
           ;; FIXME: Check for txn and give current revision's value
           (node-slot-value instance slot-keyword-name))
          ((member slot-name (ephemeral-slot-names class))
           ;; FIXME: Check for txn and give current revision's value
           (call-next-method))
          ((member slot-name (meta-slot-names class))
           (call-next-method))
          (t
           (call-next-method)))))

(defmethod (setf slot-value-using-class) :around
    (new-value (class node-class) instance slot)
  "Is alternate-version aware and will update values for the current, working private
   version of instance."
  ;;(log:debug "setf slot-value-using-class~%  '~A'~%  '~A'~%  '~A'" new-value class
  ;;(sb-pcl:slot-definition-name slot))
  (let* ((slot-name (slot-definition-name slot))
         (slot-keyword-name (intern (symbol-name slot-name) :keyword)))
    (cond ((member slot-name (persistent-slot-names class))
           ;; FIXME: Check for txn and handle
           (setf (node-slot-value instance slot-keyword-name) new-value))
          ((member slot-name (ephemeral-slot-names class))
           ;; FIXME: Check for txn and handle
           (call-next-method))
          ((member slot-name (meta-slot-names class))
           (call-next-method))
          (t
           (call-next-method)))))

(defgeneric node-equal (x y)
  (:method ((x node) (y node))
    (equalp (id x) (id y)))
  (:method (x y)
    nil))

(defmethod %hash ((key #.(class-of (make-array 16 :element-type '(unsigned-byte 8)))))
  ;;(declare (type (array (unsigned-byte 8) (16)) key))
  (let ((hash 5381))
    (dotimes (i 16)
      (let ((item (elt key i)))
        (setf hash (+ (+ hash (ash hash -5)) item))))
    hash))

(defun sxhash-node (node) (%hash (id node)))
#+sbcl (sb-ext:define-hash-table-test node-equal sxhash-node)
(defun make-node-table (&key weakness synchronized)
  #+ccl
  (make-hash-table :test 'node-equal
                   :hash-function 'sxhash-node
                   :weakness weakness
                   :shared synchronized)
  #+sbcl
  (make-hash-table :test 'node-equal
                   :weakness weakness
                   :synchronized synchronized))

(defun id-equal (x y) (equalp x y))
(defun sxhash-id-array (id) (%hash id))
#+sbcl (sb-ext:define-hash-table-test id-equal sxhash-id-array)
(defun make-id-table (&key weakness synchronized)
  #+ccl
  (make-hash-table :test 'id-equal
                   :hash-function 'sxhash-id-array
                   :weak weakness
                   :shared synchronized)
  #+sbcl
  (make-hash-table :test 'id-equal
                   :weakness weakness
                   :synchronized synchronized))
