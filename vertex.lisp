(in-package :graph-db)

(defclass vertex (node) () (:metaclass node-class))

(defmethod print-object ((node vertex) stream)
  (format stream "#<~A ~S REV ~S>"
          (type-of node) (uuid:byte-array-to-uuid (id node))
          (revision node)))

(defgeneric vertex-p (thing)
  (:method ((thing vertex)) t)
  (:method (thing) nil))

(let ((random-states (list (make-random-state)
                           (progn (sleep 1) (make-random-state)))))
  (defun gen-vertex-id ()
    (let* ((now (format nil "~,6F~6D~' :@/graph-db::print-byte-array/"
                        (coerce (gettimeofday) 'double-float)
                        (random 1000000 (nth (random 2) random-states))
                        (get-random-bytes 16)))
           (id (uuid:uuid-to-byte-array
                (uuid:make-v5-uuid *vertex-namespace* now))))
      id)))

(defun %make-vertex (&key id type-id revision deleted-p data-pointer data bytes
                     written-p heap-written-p type-idx-written-p views-written-p)
  (let ((vertex (get-vertex-buffer)))
    (when id (setf (id vertex) id))
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

(defun make-vertex-table (location &key (key-test 'uuid-array-equal))
  (let ((table
         (make-lhash :test key-test
                     :location location
                     :value-bytes +node-header-size+
                     :bucket-size 24
                     :buckets (expt 2 17)
                     :key-serializer 'serialize-key
                     :key-deserializer 'deserialize-key
                     :value-serializer 'serialize-vertex-head
                     :value-deserializer 'deserialize-vertex-head)))
    table))

(defmethod lookup-vertex ((id string) &key (graph *graph*))
  (lookup-node (vertex-table graph) (read-id-array-from-string id) graph))

(defmethod lookup-vertex ((id array) &key (graph *graph*))
  (lookup-node (vertex-table graph) id graph))

(defmethod add-to-type-index ((vertex vertex) (graph graph))
;;  (let ((skip-list (type-index-skip-list (vertex-index graph))))
;;    (add-to-skip-list skip-list (type-id vertex) (id vertex))))
  (type-index-push (id vertex) (type-id vertex) (vertex-index graph)))

(defmethod remove-from-type-index ((vertex vertex) (graph graph))
;;  (let ((skip-list (type-index-skip-list (vertex-index graph))))
;;    (remove-from-skip-list skip-list (type-id vertex) (id vertex))))
  (type-index-remove (id vertex) (type-id vertex) (vertex-index graph)))

(defmethod rollback-vertex ((graph graph) (v vertex))
  (when (views-written-p v)
    (log:error "VERTEX: Removing ~A from views" v)
    (remove-from-views graph v)
    (setf (views-written-p v) nil)
    (save-node-flags (vertex-table graph) v))
  (when (type-idx-written-p v)
    (log:error "VERTEX: Removing ~A from type-index" v)
    (remove-from-type-index v graph)
    (setf (type-idx-written-p v) nil)
    (save-node-flags (vertex-table graph) v))
  (when (and (heap-written-p v) (/= 0 (data-pointer v)))
    (log:error "VERTEX: Removing ~A from heap" v)
    (free (heap graph) (data-pointer v))
    (setf (heap-written-p v) nil)
    (save-node-flags (vertex-table graph) v))
  (log:error "VERTEX: Removing ~A from lhash" v)
  (lhash-remove (vertex-table graph) (id v)))

(defmethod write-vertex ((v vertex) (graph graph))
  (let ((addr (when (data v) (allocate (heap graph) (length (bytes v))))))
    (setf (data-pointer v) (or addr 0))
    (handler-case
        (progn
          (lhash-insert (vertex-table graph) (id v) v)
          (when (/= 0 (data-pointer v))
            (dotimes (i (length (bytes v)))
              (set-byte (heap graph)
                        (+ i (data-pointer v))
                        (aref (bytes v) i))
              (setf (heap-written-p v) t)
              (save-node-flags (vertex-table graph) v)))
          (add-to-type-index v graph)
          (setf (type-idx-written-p v) t)
          (save-node-flags (vertex-table graph) v)
          (let ((class-name (class-name (class-of v))))
            (if (lookup-view-group class-name graph)
                (with-write-locked-view-group (class-name graph)
                  ;; This lock keeps the consistency of the graph in check:
                  ;; no one can see the view change until the node is written
                  (handler-case
                      (progn
                        (%add-to-views graph v class-name)
                        (setf (views-written-p v) t)
                        (save-node-flags (vertex-table graph) v)
                        (finalize-node v (vertex-table graph) graph))
                    (error (c)
                      (log:error "VERTEX: Error finalizing ~A: ~A" v c)
                      (when (views-written-p v)
                        (log:error "VERTEX: Removing ~A from views" v)
                        (%remove-from-views graph v class-name)
                        (setf (views-written-p v) nil)
                        (save-node-flags (vertex-table graph) v)))))
                (finalize-node v (vertex-table graph) graph)))
          (log-txn graph :add v))
      (error (c)
        (dbg "Problem creating vertex: ~S" c)
        (log:error "VERTEX: Problem creating vertex: ~S" c)
        (rollback-vertex graph v)
        (error c)))
    (record-graph-write)
    v))

(defun make-vertex (type-id data &key id deleted-p revision (graph *graph*))
  (let ((type-meta (or (and (eq type-id :generic) :generic)
                       (and (eq 0 type-id) :generic)
                       (and (integerp type-id)
                            (lookup-node-type-by-id type-id :vertex :graph graph))
                       (lookup-node-type-by-name type-id :vertex :graph graph))))
    (if type-meta
        (let* ((subclass (if (eq type-meta :generic)
                             'vertex
                             (node-type-name type-meta)))
               (bytes (when data (serialize data)))
               (v (%make-vertex :id (or id (gen-vertex-id))
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
              (write-vertex v graph)
            (duplicate-key-error (c)
              (declare (ignore c))
              (let ((*print-pretty* nil))
                (log:error "VERTEX: Duplicate key error: ~A. Retrying MAKE-VERTEX" (id v)))
              (make-vertex type-id data
                           :id (gen-vertex-id)
                           :revision revision
                           :deleted-p deleted-p :graph graph))))
        (error "Unknown vertex type ~A" type-id))))

(defun copy-vertex (vertex)
  (copy-node vertex))

(defmethod save-vertex ((vertex vertex) &key (graph *graph*))
  "you must copy the vertex before writing to its slots,
  in case others are reading it!"
  (let ((class-name (class-name (class-of vertex))))
    (if (lookup-view-group class-name graph)
        (with-write-locked-view-group (class-name graph)
          (multiple-value-bind (new old)
              (save-node vertex (vertex-table graph) :graph graph)
            ;;(log:info "UPDATING VIEWS FOR ~A" (string-id new))
            (%update-in-views graph new old class-name)
            ;;(log:info "LOGGING MODIFY TXN FOR ~A" (string-id new))
            (log-txn graph :modify new)
            new))
        (let ((new (save-node vertex (vertex-table graph) :graph graph)))
          ;;(log:info "LOGGING MODIFY TXN FOR ~A" (string-id new))
          (log-txn graph :modify new)
          new))))

(defmethod delete-vertex ((vertex vertex) &key (graph *graph*))
  (let ((v (copy-vertex vertex)))
    (setf (deleted-p v) t)
    (let ((class-name (class-name (class-of vertex))))
      (if (lookup-view-group class-name graph)
          (with-write-locked-view-group (class-name graph)
            (save-node v (vertex-table graph) :graph graph)
            ;; see compact-vertex for index deletion
            ;; View deletion must happen here because of the loose coupling of
            ;; views
            (%remove-from-views graph v class-name))
          (save-node v (vertex-table graph) :graph graph))
      (log-txn graph :delete v)
      nil)))

(defun map-vertices (fn graph &key collect-p vertex-type include-deleted-p)
  (let ((result nil))
    (if vertex-type
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
                              index-list))))
        (map-lhash #'(lambda (pair)
                       (let ((vertex (cdr pair)))
                         (when (and (written-p vertex)
                                    (or include-deleted-p
                                        (not (deleted-p vertex))))
                           (setf (id vertex) (car pair))
                           (if collect-p
                               (push (funcall fn vertex) result)
                               (funcall fn vertex)))))
                   (vertex-table *graph*)))
    (when collect-p (nreverse result))))

(defmethod compact-vertices ((graph graph))
  (map-edges (lambda (vertex)
               (when (deleted-p vertex)
                 (remove-from-type-index vertex graph)))
             *graph*
             :include-deleted-p t))
