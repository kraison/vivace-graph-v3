;;;; Native (Mode B) shortest-path tests: Dijkstra, A*, single-source.

(in-package #:graph-db/algorithms-test)

(def-suite shortest-path-suite
  :description "Native Dijkstra / A* / single-source shortest paths."
  :in graph-db-algorithms-suite)

(in-suite shortest-path-suite)

;; A weighted DAG:  A->B (1)  B->C (1)  A->C (5)  C->D (1)
;; Weighted A->D = 3 (A-B-C-D); the direct A->C(5) is never on a shortest path.
(defparameter +diamond-nodes+ '("A" "B" "C" "D"))
(defparameter +diamond-edges+ '(("A" "B" 1.0) ("B" "C" 1.0) ("A" "C" 5.0) ("C" "D" 1.0)))

(test dijkstra-weighted-path-and-cost
  "Weighted shortest path prefers A-B-C-D (cost 3) over the heavy direct edge."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (multiple-value-bind (path cost)
        (shortest-path (gethash "A" h) (gethash "D" h) :graph g)
      (is (equal '("A" "B" "C" "D") (path-names path)))
      (is (= 3.0 cost)))))

(test dijkstra-unweighted-hop-count
  "Unweighted, the direct A->C edge gives a 2-hop A-C-D path to D."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (multiple-value-bind (path cost)
        (shortest-path (gethash "A" h) (gethash "D" h) :graph g :unweighted t)
      (is (equal '("A" "C" "D") (path-names path)))
      (is (= 2 cost)))))

(test shortest-path-same-node
  "FROM = TO yields the singleton path at cost 0."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (multiple-value-bind (path cost)
        (shortest-path (gethash "A" h) (gethash "A" h) :graph g)
      (is (equal '("A") (path-names path)))
      (is (= 0 cost)))))

(test shortest-path-unreachable
  "No directed path B->A in the DAG: returns (values nil nil)."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (multiple-value-bind (path cost)
        (shortest-path (gethash "B" h) (gethash "A" h) :graph g)
      (is (null path))
      (is (null cost)))))

(test shortest-path-direction-both-undirected
  "Treating edges as undirected (:direction :both) makes A reachable from D."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (multiple-value-bind (path cost)
        (shortest-path (gethash "D" h) (gethash "A" h) :graph g :direction :both)
      (is (equal '("D" "C" "B" "A") (path-names path)))
      (is (= 3.0 cost)))))

(test a-star-zero-heuristic-equals-dijkstra
  "A* with an admissible zero heuristic returns the same path/cost as Dijkstra."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (multiple-value-bind (path cost)
        (a-star (gethash "A" h) (gethash "D" h)
                (lambda (v target) (declare (ignore v target)) 0)
                :graph g)
      (is (equal '("A" "B" "C" "D") (path-names path)))
      (is (= 3.0 cost)))))

(test single-source-distances
  "single-source-shortest-paths returns every reachable node by ascending dist."
  (with-populated-graph (g h +diamond-nodes+ +diamond-edges+)
    (let* ((cells (single-source-shortest-paths (gethash "A" h) :graph g))
           (by-name (mapcar (lambda (c) (cons (slot-value (car c) 'name) (cdr c)))
                            cells)))
      (is (= 4 (length cells)))
      (is (= 0   (cdr (assoc "A" by-name :test #'string=))))
      (is (= 1.0 (cdr (assoc "B" by-name :test #'string=))))
      (is (= 2.0 (cdr (assoc "C" by-name :test #'string=))))
      (is (= 3.0 (cdr (assoc "D" by-name :test #'string=))))
      ;; sorted ascending
      (is (equal '(0 1.0 2.0 3.0) (mapcar #'cdr cells))))))
