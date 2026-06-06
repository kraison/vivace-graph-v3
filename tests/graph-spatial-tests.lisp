;;;; Tests for the graph-lifecycle integration of the spatial index
;;;; (make-graph / open-graph / close-graph wiring in graph.lisp).

(in-package #:graph-db/test)

(def-suite graph-spatial-suite
  :description "A graph owns a spatial index that survives close/reopen."
  :in graph-db-suite)

(in-suite graph-spatial-suite)

;; 16-byte node-id stand-ins (the index stores ids as opaque byte vectors).
(defun gs-bid (n)
  (let ((a (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref a 0) n) a))

(test new-graph-has-spatial-index
  "make-graph creates and attaches a usable spatial index."
  (with-test-graph (g)
    (is (spatial-index-p (spatial-index g)))
    (spatial-index-insert (spatial-index g) (gs-bid 1) (make-point 37.1724d0 49.2020d0))
    (is (member (gs-bid 1)
                (spatial-index-query-bbox (spatial-index g) 37.16d0 49.19d0 37.19d0 49.21d0)
                :test 'equalp))))

(test spatial-index-survives-reopen
  "Entries indexed before close-graph are queryable after open-graph, via the
persisted root sidecar."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (spatial-index-insert (spatial-index g) (gs-bid 1) (make-point 37.1724d0 49.2020d0))
          (spatial-index-insert (spatial-index g) (gs-bid 2) (make-point 23.7183d0 50.0263d0))
          (close-graph g :snapshot-p nil)))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is (spatial-index-p (spatial-index g2)))
               (let ((cands (spatial-index-query-bbox (spatial-index g2)
                                                      37.16d0 49.19d0 37.19d0 49.21d0)))
                 (is (member (gs-bid 1) cands :test 'equalp) "Kharkiv point persisted")
                 (is (not (member (gs-bid 2) cands :test 'equalp))
                     "Lviv point is outside the query window")))
          (close-graph g2 :snapshot-p nil)
          (collect-garbage))))))
