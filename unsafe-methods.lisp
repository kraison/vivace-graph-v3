(in-package :graph-db)

(defun %%unsafe-rehash-bucket (lhash bucket)
  (let ((pairs (read-bucket lhash
                            (%lhash-table lhash)
                            (bucket-offset lhash bucket))))
    (clear-bucket lhash (%lhash-table lhash) (bucket-offset lhash bucket))
    (dolist (pair pairs)
      (let* ((level (1+ (read-lhash-level lhash)))
             (new-bucket (hash0 lhash level (car pair))))
        (add-to-bucket lhash
                       (%lhash-table lhash)
                       (bucket-offset lhash new-bucket)
                       (car pair)
                       (cdr pair))))))

(defun %%unsafe-split-lhash (lhash)
  (let ((*rehashing-bucket* t))
    (let ((bucket (read-lhash-next-split lhash)))
      (unless (>= (mapped-file-length (%lhash-table lhash))
                  (1+ (* (%lhash-bucket-bytes lhash)
                         (1+ (bucket-count lhash)))))
        (setf (%lhash-table lhash)
              (extend-mapped-file (%lhash-table lhash)
                                  +data-extent-size+)))
      (%%unsafe-rehash-bucket lhash bucket)
      (incf-lhash-next-split lhash)
      (when (= (read-lhash-next-split lhash)
               (* (%lhash-base-buckets lhash)
                  (expt 2 (read-lhash-level lhash))))
        (set-lhash-next-split lhash 0)
        (incf-lhash-level lhash))))
  lhash)

(defun %%unsafe-lhash-get (lhash key)
  (let ((bucket (hash lhash (read-lhash-level lhash) key)))
    (read-from-bucket lhash (%lhash-table lhash)
                      (bucket-offset lhash bucket) key)))

(defun %%unsafe-lhash-insert (lhash key val)
  (let ((bucket (hash lhash (read-lhash-level lhash) key)) (split-p nil))
    (setq split-p
          (add-to-bucket lhash (%lhash-table lhash)
                         (bucket-offset lhash bucket) key val))
    (when (or split-p (> (load-factor lhash) .75))
      (%%unsafe-split-lhash lhash))))

(defun %%unsafe-lhash-update (lhash key val)
  (let ((bucket (hash lhash (read-lhash-level lhash) key)))
    (update-in-bucket lhash (%lhash-table lhash)
                      (bucket-offset lhash bucket) key val)))

(defun %%unsafe-lhash-custom-update (lhash fn key)
  (let ((bucket (hash lhash (read-lhash-level lhash) key)))
    (custom-update-in-bucket lhash (%lhash-table lhash)
                             (bucket-offset lhash bucket)
                             fn key)))

(defmethod %%unsafe-save-node-flags ((table lhash) (node node))
  (%%unsafe-lhash-custom-update
   table
   (lambda (mf offset)
     (let ((flags (flags-as-int node)))
       (set-byte mf offset flags)))
   (id node)))

(defun %%unsafe-finalize-node (node table graph)
  (setf (written-p node) t)
  (%%unsafe-save-node-flags table node)
  (setf (gethash (id node) (cache graph)) node))

(defun %%unsafe-save-node (node table &key (graph *graph*))
  (let ((old-node (%%unsafe-lhash-get table (id node))))
    (setf (id old-node) (id node))
    (when (data node)
      (setf (bytes node) (serialize (data node)))
      (let ((addr (allocate (heap graph) (length (bytes node)))))
        (dotimes (i (length (bytes node)))
          (set-byte (heap graph)
                    (+ i addr) (aref (bytes node) i)))
        (setf (data-pointer node) addr)))
    ;; Don't overflow the revision (32 bit int)
    (if (>= (revision node) 4294967295)
        (setf (revision node) 0)
        (incf (revision node)))
    (%%unsafe-lhash-update table (id node) node)
    (setf (gethash (id node) (cache graph)) node)
    (when (/= 0 (data-pointer old-node))
      (let ((pointer (data-pointer old-node))
            (heap (heap graph)))
        (free heap pointer)))
    (values node old-node)))

(defmethod %%unsafe-type-index-push ((uuid array) (type-id integer) (idx type-index))
  (let ((il (gethash type-id (type-index-cache idx))))
    (index-list-push uuid il)
    (serialize-index-list (type-index-table idx)
                          il
                          (* type-id +index-list-bytes+))
    il))

(defmethod %%unsafe-add-to-type-index ((vertex vertex) (graph graph))
  (%%unsafe-type-index-push (id vertex) (type-id vertex) (vertex-index graph)))

(defmethod %%unsafe-add-to-type-index ((edge edge) (graph graph))
  (%%unsafe-type-index-push (id edge) (type-id edge) (edge-index graph)))

(defmethod %%unsafe-ve-index-push ((idx ve-index) (key ve-key) (id array))
  (let ((table (ve-index-table idx)))
    (let ((index-list (%%unsafe-lhash-get table key)))
      (if index-list
          (progn
            (index-list-push id index-list)
            (%%unsafe-lhash-update table key index-list))
          (progn
            (setq index-list
                  (make-index-list (heap *graph*) id))
            (%%unsafe-lhash-insert table key index-list)))
      (cache-index-list idx key index-list))))

(defmethod %%unsafe-add-to-ve-index ((edge edge) (graph graph))
  (let ((in-ve-key (make-ve-key :id (to edge) :type-id (type-id edge)))
        (out-ve-key (make-ve-key :id (from edge) :type-id (type-id edge))))
    (%%unsafe-ve-index-push (ve-index-in graph) in-ve-key (id edge))
    (%%unsafe-ve-index-push (ve-index-out graph) out-ve-key (id edge))))

(defmethod %%unsafe-add-to-vev-index ((edge edge) (graph graph))
  (let ((vev-key (make-vev-key :in-id (to edge) :out-id (from edge) :type-id (type-id edge)))
        (table (vev-index-table (vev-index graph))))
    (let ((index-list (%%unsafe-lhash-get table vev-key)))
      (if index-list
          (progn
            ;;(dbg "add-to-vev-index: Got ~A" index-list)
            (index-list-push (id edge) index-list)
            (%%unsafe-lhash-update table vev-key index-list))
          (progn
            (setq index-list
                  (make-index-list (heap graph) (id edge)))
            (%%unsafe-lhash-insert table vev-key index-list)))
      (cache-index-list (vev-index graph) vev-key index-list))))

(defmethod %%unsafe-write-edge ((e edge) (graph graph))
  (let ((*print-pretty* nil))
    (let ((addr (when (bytes e) (allocate (heap graph) (length (bytes e))))))
      (setf (data-pointer e) (or addr 0))
      (%%unsafe-lhash-insert (edge-table graph) (id e) e)
      (when (/= 0 (data-pointer e))
        (dotimes (i (length (bytes e)))
          (set-byte (heap graph)
                    (+ i (data-pointer e))
                    (aref (bytes e) i))))
      (setf (heap-written-p e) t)
      (%%unsafe-add-to-type-index e graph)
      (setf (type-idx-written-p e) t)
      (%%unsafe-add-to-ve-index e graph)
      (setf (ve-written-p e) t)
      (%%unsafe-add-to-vev-index e graph)
      (setf (vev-written-p e) t)
      (setf (views-written-p e) t)
      (%%unsafe-finalize-node e (edge-table graph) graph)
      e)))

(defun %%unsafe-make-edge (type from to weight data &key id revision deleted-p
                  (graph *graph*))
  (when (stringp id)
    (setq id (read-id-array-from-string id)))
  (typecase from
    (string (setq from (read-id-array-from-string from)))
    (vertex (setq from (id from))))
  (typecase to
    (string (setq to (read-id-array-from-string to)))
    (vertex (setq to (id to))))
  (let ((type-meta (or (and (eq type :generic) :generic)
                       (and (eq 0 type) :generic)
                       (and (integerp type)
                            (lookup-node-type-by-id type :edge :graph graph))
                       (lookup-node-type-by-name type :edge :graph graph))))
    (if type-meta
        (let* ((subclass (if (eq type-meta :generic)
                             'edge
                             (node-type-name type-meta)))
               (bytes (when data (serialize data)))
               (e (%make-edge
                   :id (or id (gen-edge-id))
                   :type-id (if (eq type-meta :generic)
                                0
                                (node-type-id type-meta))
                   :revision (or revision 0)
                   :deleted-p deleted-p
                   :written-p nil
                   :from from
                   :to to
                   :weight weight
                   :bytes bytes
                   :data data)))
          (change-class e subclass)
          (setf (bytes e) bytes)
          (%%unsafe-write-edge e graph))
        (error "Unknown edge type ~A" type))))

(defmethod %%unsafe-save-edge ((edge edge) &key (graph *graph*))
  (%%unsafe-save-node edge (edge-table graph) :graph graph)
  edge)

(defmethod %%unsafe-delete-edge ((edge edge) &key (graph *graph*))
  (let ((e (copy-edge edge)))
    (setf (deleted-p e) t)
    (%%unsafe-save-node e (edge-table graph) :graph graph)
    nil))

(defmethod %%unsafe-write-vertex ((v vertex) (graph graph))
  (let ((addr (when (data v) (allocate (heap graph) (length (bytes v))))))
    (setf (data-pointer v) (or addr 0))
    (%%unsafe-lhash-insert (vertex-table graph) (id v) v)
    (when (/= 0 (data-pointer v))
      (dotimes (i (length (bytes v)))
        (set-byte (heap graph)
                  (+ i (data-pointer v))
                  (aref (bytes v) i))))
    (setf (heap-written-p v) t)
    (%%unsafe-add-to-type-index v graph)
    (setf (type-idx-written-p v) t)
    (setf (views-written-p v) t)
    (%%unsafe-finalize-node v (vertex-table graph) graph)
    v))

(defun %%unsafe-make-vertex (type-id data &key id deleted-p revision (graph *graph*))
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
          (%%unsafe-write-vertex v graph))
        (error "Unknown vertex type ~A" type-id))))

(defmethod %%unsafe-save-vertex ((vertex vertex) &key (graph *graph*))
  (%%unsafe-save-node vertex (vertex-table graph) :graph graph)
  vertex)

(defmethod %%unsafe-delete-vertex ((vertex vertex) &key (graph *graph*))
  (let ((v (copy-vertex vertex)))
    (setf (deleted-p v) t)
    (%%unsafe-save-node v (vertex-table graph) :graph graph)
    nil))

(defun %%unsafe-execute-tx-action (action tx)
  (let* ((type (first tx))
         (subtype (cond ((subtypep type 'vertex) :vertex)
                        ((subtypep type 'edge) :edge)
                        (t (error "Unknown graph type ~A" type)))))
    (case subtype
      (:vertex
       (destructuring-bind (type data id revision deleted-p)
           tx
         (setf id (transform-to-byte-vector id))
         (case action
           (:add
            (%%unsafe-make-vertex type data
                                  :id id
                                  :revision revision
                                  :deleted-p deleted-p))
           (:delete
            (let ((vertex (lookup-vertex id)))
              (if vertex
                  (%%unsafe-delete-vertex vertex)
                  (log:error "DELETE on unknown vertex ~A" id))))
           (:modify
            (let ((vertex (lookup-vertex id)))
              (if vertex
                  (let ((new-vertex (copy vertex)))
                    (setf (data new-vertex) data)
                    (%%unsafe-save-vertex new-vertex))
                  (log:error "MODIFY on unknown vertex ~A" id))))
           (otherwise
            (dbg "Unknown input: ~S" tx)
            (log:error "Unknown input: ~S" tx)
            (error "Unknown input: ~S" tx)))))
      (:edge
       (destructuring-bind (type from to weight data id revision deleted-p)
           tx
         (setf id (transform-to-byte-vector id))
         (case action
           (:add
            (setf from (transform-to-byte-vector from))
            (setf to (transform-to-byte-vector to))
            (%%unsafe-make-edge type from to weight data
                                :id id
                                :revision revision
                                :deleted-p deleted-p))
           (:delete
            (let ((edge (lookup-edge id)))
              (if edge
                  (%%unsafe-delete-edge edge)
                  (log:error "DELETE on unknown edge ~A" id))))
           (:modify
            (let ((edge (lookup-edge id)))
              (if edge
                  (let ((new-edge (copy edge)))
                    (setf (weight new-edge) weight)
                    (setf (data new-edge) data)
                    (%%unsafe-save-edge new-edge))
                  (log:error "MODIFY on unknown edge ~A" id))))
           (otherwise
            (dbg "Unknown input: ~S" tx)
            (log:error "Unknown input: ~S" tx)
            (error "Unknown input: ~S" tx))))))))

