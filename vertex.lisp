(in-package :graph-db)

(defclass vertex (node)
  ()
  (:metaclass node-class))

(defmethod print-object ((node vertex) stream)
  (format stream "#<~A ~S REV ~S>"
          (type-of node) (string-id (id node))
          (revision node)))

(defgeneric vertex-p (thing)
  (:method ((thing vertex)) t)
  (:method (thing) nil))

(defun %make-vertex (&key id type-id revision deleted-p data-pointer data bytes
                     written-p heap-written-p type-idx-written-p views-written-p)
  (let ((vertex (get-vertex-buffer)))
    (cond (id
           (setf (id vertex) id))
          ((equalp +null-key+ (id vertex))
           (setf (id vertex) (gen-vertex-id))))
    (when type-id (setf (type-id vertex) type-id))
    (when revision (setf (revision vertex) revision))

    (when deleted-p (setf (deleted-p vertex) deleted-p))
    (when written-p (setf (written-p vertex) written-p))
    (when heap-written-p (setf (heap-written-p vertex) heap-written-p))
    (when type-idx-written-p (setf (type-idx-written-p vertex) type-idx-written-p))
    ;; For edges only
    ;;(when ve-written-p (setf (ve-written-p vertex) ve-written-p))
    ;;(when vev-written-p (setf (vev-written-p vertex) vev-written-p))
    (when views-written-p (setf (views-written-p vertex) views-written-p))

    (when data-pointer (setf (data-pointer vertex) data-pointer))
    (when data (setf (data vertex) data))
    (when bytes (setf (bytes vertex) bytes))
    vertex))

(defun serialize-vertex-head (mf v offset)
  (serialize-node-head mf v offset))

(defun deserialize-vertex-head (mf offset)
  (multiple-value-bind
        (deleted-p written-p heap-written-p type-idx-written-p views-written-p
                   ve-written-p vev-written-p type-id revision pointer)
      (deserialize-node-head mf offset)
    (declare (ignore ve-written-p vev-written-p))
    (let* ((subclass (if (eq type-id 0)
                         'vertex
                         (let ((type-meta (lookup-node-type-by-id
                                           type-id :vertex)))
                           (node-type-name type-meta))))
           (v (%make-vertex :deleted-p deleted-p
                            :written-p written-p
                            :heap-written-p heap-written-p
                            :type-idx-written-p type-idx-written-p
                            :views-written-p views-written-p
                            :type-id type-id
                            :revision revision
                            :data-pointer pointer)))
      (change-class v subclass))))

(defun make-vertex-table (location &key (key-test 'uuid-array-equal)
                                     (base-buckets (expt 2 17)))
  (let ((table
         (make-lhash :test key-test
                     :location location
                     :value-bytes +node-header-size+
                     :bucket-size 24
                     :buckets base-buckets
                     :key-serializer 'serialize-key
                     :key-deserializer 'deserialize-key
                     :value-serializer 'serialize-vertex-head
                     :value-deserializer 'deserialize-vertex-head)))
    table))

(defmethod lookup-vertex ((id string) &key (graph *graph*))
  (lookup-vertex (read-id-array-from-string id) :graph graph))

(defmethod lookup-vertex ((id array) &key (graph *graph*))
  (lookup-object id (vertex-table graph) *transaction* graph))

(defmethod add-to-type-index ((vertex vertex) (graph graph)
                              &key unless-present)
;;  (let ((skip-list (type-index-skip-list (vertex-index graph))))
;;    (add-to-skip-list skip-list (type-id vertex) (id vertex))))
  (type-index-push (id vertex) (type-id vertex) (vertex-index graph)
                   :unless-present unless-present))

(defmethod remove-from-type-index ((vertex vertex) (graph graph))
;;  (let ((skip-list (type-index-skip-list (vertex-index graph))))
;;    (remove-from-skip-list skip-list (type-id vertex) (id vertex))))
  (type-index-remove (id vertex) (type-id vertex) (vertex-index graph)))

(defun make-vertex (type-id data &key id deleted-p revision retry-p (graph *graph*))
  (let ((type-meta (or (and (eq type-id :generic) :generic)
                       (and (eq 0 type-id) :generic)
                       (and (integerp type-id)
                            (lookup-node-type-by-id type-id :vertex :graph graph))
                       (lookup-node-type-by-name type-id :vertex :graph graph))))
    (when (stringp id)
      (setq id (read-id-array-from-string id)))
    (if type-meta
        (let* ((subclass (if (eq type-meta :generic)
                             'vertex
                             (node-type-name type-meta)))
               (bytes (when data (serialize data)))
               (v (%make-vertex :id id ;; (or id (gen-vertex-id))
                                :type-id (if (eq type-meta :generic)
                                             0
                                             (node-type-id type-meta))
                                :revision (or revision 0)
                                :deleted-p deleted-p
                                :written-p nil
                                :bytes bytes
                                :data data)))
          (change-class v subclass)
          (setf (bytes v) bytes)
          (handler-case
              (create-node v graph)
            (duplicate-key-error (c)
              (if retry-p
                  (let ((*print-pretty* nil))
                    (log:error "VERTEX: Duplicate key error: ~A. Retrying MAKE-VERTEX" (id v))
                    (make-vertex type-id data
                                 :id (gen-vertex-id)
                                 :revision revision
                                 :deleted-p deleted-p :graph graph))
                  (error c)))))
        (error "Unknown vertex type ~A" type-id))))

(defun copy-vertex (vertex)
  (copy-node vertex))

(defmethod delete-vertex ((vertex vertex) &key (graph *graph*))
  (when (deleted-p vertex)
    (error 'vertex-already-deleted-error
           :node vertex))
  (delete-node vertex graph))

(defun map-vertices (fn graph &key collect-p vertex-type include-deleted-p (include-subclasses-p t))
  (let ((result nil))
    (flet ((map-it (vertex-type)
             (let* ((type-meta (or (and (integerp vertex-type)
                                        (lookup-node-type-by-id vertex-type :vertex))
                                   (lookup-node-type-by-name vertex-type :vertex)))
                    (vertex-type-id (node-type-id type-meta)))
               (when vertex-type-id
                 (let ((index-list (get-type-index-list (vertex-index graph) vertex-type-id)))
                   (map-index-list (lambda (id)
                                     (let ((vertex (lookup-vertex id :graph graph)))
                                       (when (and (written-p vertex)
                                                  (or include-deleted-p
                                                      (not (deleted-p vertex))))
                                         (if collect-p
                                             (push (funcall fn vertex) result)
                                             (funcall fn vertex)))))
                                   index-list))))))
      (cond ((and vertex-type include-subclasses-p)
             (let ((vertex-class
                    (find-class
                     (if (integerp vertex-type)
                         (let ((node-type (lookup-node-type-by-id vertex-type :vertex)))
                           (if node-type
                               (node-type-name node-type)
                               (error "Unknown node-type ~A" vertex-type)))
                         vertex-type))))
               (if vertex-class
                   (let ((all-classes (nconc (list vertex-type)
                                             (find-all-subclass-names vertex-class))))
                     (mapcan #'map-it all-classes))
                   (error "Unable to find-class for vertex-type ~A" vertex-type))))
            (vertex-type
             (map-it vertex-type))
            (t
             (map-lhash #'(lambda (pair)
                            (let ((vertex (cdr pair)))
                              (when (and (written-p vertex)
                                         (or include-deleted-p
                                             (not (deleted-p vertex))))
                                (setf (id vertex) (car pair))
                                (if collect-p
                                    (push (funcall fn vertex) result)
                                    (funcall fn vertex)))))
                        (vertex-table *graph*)))))
    (when collect-p (nreverse result))))

(defmethod compact-vertices ((graph graph))
  (map-edges (lambda (vertex)
               (when (deleted-p vertex)
                 (remove-from-type-index vertex graph)))
             *graph*
             :include-deleted-p t))
