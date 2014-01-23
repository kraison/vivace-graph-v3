(in-package :graph-db)

(defun make-graph (name location &key master-p slave-p master-host replication-port
                   replication-key package)
  (let* ((path (first (directory (ensure-directories-exist location))))
         (dirty-file (format nil "~A/.dirty" location)))
    (unless (probe-file path)
      (error "Unable to open graph location ~A" path))
    (init-buffer-pool)
    (let* ((heap (create-memory
                  (format nil "~A/heap.dat" path)
                  (* 1024 1024 1000)))
           (graph
            (make-instance
             (cond (slave-p 'slave-graph)
                   (master-p 'master-graph)
                   (t 'graph))
             :graph-name name
             :location path
             :views (make-hash-table :synchronized t)
             :cache
             (make-id-table :synchronized t :weakness :value)
             ;;(make-hash-table :synchronized t :test 'equalp :weakness :value)
             :replication-key replication-key
             :replication-port replication-port
             :vertex-table (make-vertex-table
                            (format nil "~A/vertex/" path))
             :edge-table (make-edge-table
                          (format nil "~A/edge/" path))
             :heap heap
             :indexes (create-memory
                       (format nil "~A/indexes.dat" path)
                       (* 1024 1024 1000))
             :ve-index-in (make-ve-index
                           (format nil "~A/ve-index-in/" path))
             :ve-index-out (make-ve-index
                            (format nil "~A/ve-index-out/" path))
             :vev-index (make-vev-index
                         (format nil "~A/vev-index/" path)))))
      (setf (vertex-index graph)
            (make-type-index
             (format nil "~A/vertex-index.dat" path) heap))
      (setf (edge-index graph)
            (make-type-index
             (format nil "~A/edge-index.dat" path) heap))
      (let ((*graph* graph))
        (init-schema graph)
        (update-schema graph)
        (init-txn-log graph)
        (with-open-file (out dirty-file :direction :output)
          (format out "~S" (get-universal-time)))
        (setf (gethash name *graphs*) graph))
      (when slave-p
        (setf (master-host graph) master-host))
      (start-replication graph :package package)
      graph)))

(defun open-graph (name location &key master-p slave-p master-host replication-port
                   replication-key package)
  (let ((path (first (directory (ensure-directories-exist location))))
        (dirty-file (format nil "~A/.dirty" location))
        (schema-file (format nil "~A/schema.dat" location)))
    (unless (probe-file path)
      (error "Unable to open graph location ~A" path))
    (when (probe-file dirty-file)
      (error "~A exists;  graph not closed properly.  Run recovery." dirty-file))
    (log:info "Initilizing buffer pool.")
    (init-buffer-pool)
    (log:info "Opening graph.")
    (let* ((heap (open-memory (format nil "~A/heap.dat" path)))
           (graph
            (make-instance
             (cond (slave-p 'slave-graph)
                   (master-p 'master-graph)
                   (t 'graph))
             :graph-name name
             :location path
             :views (make-hash-table :synchronized t)
             :cache
             (make-id-table :synchronized t :weakness :value)
             ;;(make-hash-table :synchronized t :test 'equalp :weakness :value)
             :replication-key replication-key
             :replication-port replication-port
             :vertex-table (open-lhash
                            (format nil "~A/vertex/" path))
             :edge-table (open-lhash
                          (format nil "~A/edge/" path))
             :heap heap
             :indexes (open-memory
                       (format nil "~A/indexes.dat" path))
             :ve-index-in (open-ve-index
                           (format nil "~A/ve-index-in/" path))
             :ve-index-out (open-ve-index
                            (format nil "~A/ve-index-out/" path))
             :vev-index (open-vev-index
                         (format nil "~A/vev-index/" path)))))
      (let ((*graph* graph))
        (setf (vertex-index graph)
              (open-type-index (format nil "~A/vertex-index.dat" path) heap))
        (setf (edge-index graph)
              (open-type-index (format nil "~A/edge-index.dat" path) heap))
        (if (probe-file schema-file)
            (setf (schema graph)
                  (cl-store:restore schema-file))
            (init-schema graph))
        (setf (schema-lock (schema graph)) (make-recursive-lock))
        (update-schema graph)
        (restore-views graph)
        (init-txn-log graph)
        (with-open-file (out dirty-file :direction :output)
          (format out "~S" (get-universal-time)))
        (setf (gethash name *graphs*) graph))
      (when slave-p
        (setf (master-host graph) master-host))
      (start-replication graph :package package)
      graph)))

(defmethod close-graph ((graph graph) &key (snapshot-p t))
  (stop-replication graph)
  (remhash (graph-name graph) *graphs*)
  (when snapshot-p
    (dbg "Snapshotting ~A" graph)
    (snapshot graph))
  (close-txn-log graph)
  (when (type-index-p (vertex-index graph))
    (dbg "Closing ~A" (vertex-index graph))
    (close-type-index (vertex-index graph)))
  (when (type-index-p (edge-index graph))
    (dbg "Closing ~A" (edge-index graph))
    (close-type-index (edge-index graph)))
  (when (vev-index-p (vev-index graph))
    (dbg "Closing ~A" (vev-index graph))
    (close-vev-index (vev-index graph)))
  (when (ve-index-p (ve-index-in graph))
    (dbg "Closing ~A" (ve-index-in graph))
    (close-ve-index (ve-index-in graph)))
  (when (ve-index-p (ve-index-out graph))
    (dbg "Closing ~A" (ve-index-out graph))
    (close-ve-index (ve-index-out graph)))
  (when (lhash-p (vertex-table graph))
    (dbg "Closing ~A" (vertex-table graph))
    (close-lhash (vertex-table graph)))
  (when (lhash-p (edge-table graph))
    (dbg "Closing ~A" (edge-table graph))
    (close-lhash (edge-table graph)))
  (when (memory-p (indexes graph))
    (dbg "Closing ~A" (indexes graph))
    (close-memory (indexes graph)))
  (when (memory-p (heap graph))
    (dbg "Closing ~A" (heap graph))
    (close-memory (heap graph)))
  (stop-buffer-pool)
  (setf (heap graph) nil
        (vertex-table graph) nil
        (edge-table graph) nil)
  (let ((dirty-file (format nil "~A/.dirty" (location graph))))
    (delete-file dirty-file))
  graph)

