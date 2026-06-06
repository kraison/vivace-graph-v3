;;;; End-to-end test of the write-path hook: transactions auto-maintain the
;;;; spatial index for geometry-bearing nodes (transactions.lisp
;;;; apply-tx-writes-to-spatial-index), via the NODE-GEOMETRY protocol.

(in-package #:graph-db/test)

;; A geometry-bearing vertex type in the shared integration schema, plus the
;; NODE-GEOMETRY method that opts it into spatial indexing.  (LOC holds a
;; geometry value, which serializes via the +geometry+ codec.)
(def-vertex geo-place ()
  ((loc))
  :graph-db-integration-test)

(defmethod node-geometry ((p geo-place))
  (slot-value p 'loc))

(def-suite spatial-hook-suite
  :description "Transactions auto-maintain the spatial index (create/update/delete)."
  :in graph-db-suite)

(in-suite spatial-hook-suite)

(defparameter *kharkiv-box* '(37.16d0 49.19d0 37.19d0 49.21d0))
(defparameter *lviv-box*    '(23.70d0 50.00d0 23.75d0 50.05d0))

(defun in-box-p (g id box)
  (member id (apply #'spatial-index-query-bbox (spatial-index g) box) :test 'equalp))

(test create-indexes-node
  "Committing a geometry-bearing node indexes it automatically."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
      (is (in-box-p g id *kharkiv-box*))
      (is (not (in-box-p g id *lviv-box*))))))

(test update-reindexes-node
  "Updating the geometry moves the node in the index (old cell out, new cell in)."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
      (is (in-box-p g id *kharkiv-box*))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'loc) (make-point 23.7183d0 50.0263d0))
          (save v)))
      (is (not (in-box-p g id *kharkiv-box*)) "old location de-indexed")
      (is (in-box-p g id *lviv-box*) "new location indexed"))))

(test delete-removes-from-index
  "Deleting a node removes it from the spatial index."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
      (is (in-box-p g id *kharkiv-box*))
      (with-transaction ()
        (mark-deleted (lookup-vertex id)))
      (is (not (in-box-p g id *kharkiv-box*))))))

(test nodes-without-geometry-are-ignored
  "A node whose NODE-GEOMETRY is NIL (the default) is not indexed, and commits
fine."
  (with-test-graph (g)
    (let (id)
      (with-transaction ()
        (setq id (id (make-g-person :name "No geometry" :age 1))))
      (is (lookup-vertex id))
      ;; querying a wide box returns no g-person (only geo-place nodes index)
      (is (not (in-box-p g id *kharkiv-box*))))))

(test index-survives-reopen-with-real-nodes
  "Nodes indexed via the write-path hook are still queryable after reopen."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (id nil))
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))))
          (close-graph g :snapshot-p nil)))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is (in-box-p g2 id *kharkiv-box*)))
          (close-graph g2 :snapshot-p nil)
          (collect-garbage))))))
