(in-package :graph-db)

(defgeneric backup (object location &key include-deleted-p))

(defmethod backup :around ((node node) location &key include-deleted-p)
  (when (or include-deleted-p (not (deleted-p node)))
    (call-next-method)))

(defmethod backup ((v vertex) (stream stream) &key include-deleted-p)
  (declare (ignore include-deleted-p))
  (let ((plist
         (list :v
               (type-of v)
               (when (slot-boundp v 'data)
                 (data v))
               :id (id v)
               :revision (revision v)
               :deleted-p (deleted-p v))))
    (let ((*print-pretty* nil))
      (format stream "~S~%" plist))))

(defmethod backup ((e edge) (stream stream) &key include-deleted-p)
  (declare (ignore include-deleted-p))
  (let ((plist
         (list :e
               (type-of e)
               (from e)
               (to e)
               (weight e)
               (when (slot-boundp e 'data)
                 (data e))
               :id (id e)
               :revision (revision e)
               :deleted-p (deleted-p e))))
    (let ((*print-pretty* nil))
      (format stream "~S~%" plist))))

(defmethod backup ((graph graph) location &key include-deleted-p)
  (ensure-directories-exist location)
  (let ((count 0))
    (with-open-file (out location :direction :output)
      (map-vertices (lambda (v)
                      (maybe-init-node-data v :graph graph)
                      (incf count)
                      (backup v out))
                    graph :include-deleted-p include-deleted-p)
      (map-edges (lambda (e)
                   (maybe-init-node-data e :graph graph)
                   (incf count)
                   (backup e out))
                 graph :include-deleted-p include-deleted-p)
      (values count location))))

(defmethod check-data-integrity ((graph graph) &key include-deleted-p)
  (let ((*cache-enabled* nil))
    (let ((problems nil) (count 0))
      (map-vertices (lambda (v)
                      (incf count)
                      (when (= 0 (mod count 1000))
                        (format t ".")
                        (force-output))
                      (handler-case
                          (maybe-init-node-data v :graph graph)
                        (error (c)
                          (log:error "data integrity ~A: ~A" (string-id v) c)
                          (push (cons (string-id v) c) problems))))
                    graph :include-deleted-p include-deleted-p)
      (map-edges (lambda (e)
                      (incf count)
                      (when (= 0 (mod count 1000))
                        (format t ".")
                        (force-output))
                   (handler-case
                       (maybe-init-node-data e :graph graph)
                     (error (c)
                       (log:error "data integrity ~A: ~A" (string-id e) c)
                       (push (cons (string-id e) c) problems))))
                 graph :include-deleted-p include-deleted-p)
      (terpri)
      problems)))

;;; ---------------------------------------------------------------------------
;;; v1 -> v2 migration (MVCC head growth, storage-version 1 -> 2)
;;;
;;; v2 grew the node head 15 -> 31 bytes (commit-epoch + prev-pointer), so v2
;;; code cannot open a v1 graph directly.  MIGRATE-GRAPH does a format-agnostic
;;; LOGICAL snapshot + replay: open the v1 graph read-only with a 15-byte head
;;; shim, BACKUP every live node to a pointer-free plist file, then MAKE-GRAPH a
;;; fresh v2 graph and RECREATE-GRAPH (replay) into it.  Precedent: the
;;; pre-58f87d6 UUID/hash change was migrated the same way (snapshot + replay).
;;; ---------------------------------------------------------------------------

(defun migrate-graph (name old-location new-location
                      &key (package :graph-db) include-deleted-p
                           (delete-snapshot-p t)
                           (snapshot-file
                            (format nil "~A/migrate-~A.snapshot"
                                    (or #+sbcl (sb-ext:native-namestring
                                                (uiop:temporary-directory))
                                        "/tmp/")
                                    name)))
  "Migrate a pre-MVCC (v1) graph at OLD-LOCATION to the current (v2) on-disk
format at NEW-LOCATION, returning the new, open graph.

Migration is a logical snapshot + replay: the v1 graph is opened read-only with
a 15-byte head shim, every live node is written to a format-independent snapshot
file, then a fresh v2 graph is created and the snapshot replayed through the
normal MAKE-VERTEX / MAKE-EDGE path.  The v1 graph's schema (its type-id
registry) is copied to the new graph, so type-ids are preserved.

OLD-LOCATION is left byte-for-byte untouched; NEW-LOCATION must not already hold
a graph.  The CLOS classes for the graph's node types must already be defined in
this image (load your DEF-VERTEX / DEF-EDGE forms first).  :INCLUDE-DELETED-P
carries tombstoned nodes across too; :DELETE-SNAPSHOT-P (default T) removes the
intermediate snapshot file when done."
  (when (equal (namestring (truename (ensure-directories-exist
                                      (merge-pathnames "" old-location))))
               (ignore-errors
                (namestring (truename (merge-pathnames "" new-location)))))
    (error "MIGRATE-GRAPH: old and new locations must differ (~A)" old-location))
  (let ((old-schema nil))
    ;; 1. Open the v1 graph read-only (15-byte heads) and snapshot it logically.
    (let ((*node-head-reader* 'deserialize-node-head-v1))
      (let ((old (open-graph name old-location
                             ;; tolerate v1 AND v2 so a re-run is harmless
                             :accept-versions (list 1 +storage-version+)
                             :gc-heap-p nil :buffer-pool-p t)))
        (unwind-protect
             (let ((*graph* old)) ;; map-vertices' all-types branch reads *graph*
               (setq old-schema (schema old))
               (log:info "MIGRATE-GRAPH: snapshotting v1 graph ~A -> ~A"
                         old-location snapshot-file)
               (backup old snapshot-file :include-deleted-p include-deleted-p))
          (close-graph old :snapshot-p nil))))
    ;; 2. Create the v2 graph, adopt the v1 schema (preserving type-ids), replay.
    (let ((new (make-graph name new-location)))
      (handler-case
          (progn
            (setf (schema new) old-schema)
            (restore-schema-locks (schema new))
            (setf (schema-lock (schema new)) (make-recursive-lock))
            (save-schema (schema new) new)
            (log:info "MIGRATE-GRAPH: replaying snapshot into v2 graph ~A"
                      new-location)
            (recreate-graph new snapshot-file :package-name package)
            (when delete-snapshot-p
              (ignore-errors (delete-file snapshot-file)))
            new)
        (error (c)
          (close-graph new)
          (error c))))))
