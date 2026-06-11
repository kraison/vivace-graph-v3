;;;; Tests for the Lisp-method traversal API (traverse.lisp): the breadth-first
;;;; TRAVERSE method, its :direction / :edge-type / :max-depth / :return-paths
;;;; options, and the traversal accessors (TRAVERSAL-PATH, END-VERTEX).
;;;;
;;;; Notes on TRAVERSE's current semantics (asserted, not judged):
;;;;   - It only collects a vertex when the connecting edge is (typep edge
;;;;     EDGE-TYPE), so an EDGE-TYPE must be supplied to get any results.
;;;;   - :MAX-DEPTH gates EXPANSION of a node, so max-depth 0 expands only the
;;;;     start vertex and thus yields its direct neighbours.
;;;;
;;;; Reuses the g-person / g-knows / g-likes schema from graph-tests.lisp.

(in-package #:graph-db/test)

(def-suite traverse-suite
  :description "Breadth-first TRAVERSE method and traversal accessors."
  :in graph-db-suite)

(in-suite traverse-suite)

(defun names-of (vertices)
  (sort (mapcar (lambda (v) (slot-value v 'name)) vertices) #'string<))

;;; Build:  A -knows-> B -knows-> C ,  A -knows-> D ,  A -likes-> E
(defmacro with-diamond-graph ((g) &body body)
  `(with-test-graph (,g)
     (let (a b c d e)
       (declare (ignorable a b c d e))
       (with-transaction ()
         (setq a (make-g-person :name "A") b (make-g-person :name "B")
               c (make-g-person :name "C") d (make-g-person :name "D")
               e (make-g-person :name "E"))
         (make-g-knows :from a :to b)
         (make-g-knows :from b :to c)
         (make-g-knows :from a :to d)
         (make-g-likes :from a :to e))
       ,@body)))

(test traverse-out-collects-reachable-of-type
  "Outbound traverse following g-knows reaches B, C (via B) and D; the g-likes
neighbour E is excluded by the edge-type filter."
  (with-diamond-graph (g)
    (is (equal '("B" "C" "D")
               (names-of (traverse a :graph g :direction :out
                                     :edge-type 'g-knows))))))

(test traverse-edge-type-filters
  "Following g-likes from A reaches only E, not the g-knows neighbours."
  (with-diamond-graph (g)
    (is (equal '("E")
               (names-of (traverse a :graph g :direction :out
                                     :edge-type 'g-likes))))))

(test traverse-max-depth-0-is-direct-neighbours
  "max-depth 0 expands only the start vertex, so it yields A's direct g-knows
neighbours (B, D) but not the two-hop C."
  (with-diamond-graph (g)
    (is (equal '("B" "D")
               (names-of (traverse a :graph g :direction :out
                                     :edge-type 'g-knows :max-depth 0))))))

(test traverse-direction-in-walks-backwards
  "Inbound traverse from C reaches B then A along the g-knows chain."
  (with-diamond-graph (g)
    (is (equal '("A" "B")
               (names-of (traverse c :graph g :direction :in
                                     :edge-type 'g-knows))))))

(test traverse-direction-both
  "Bidirectional traverse from B reaches the whole g-knows component: C (out),
A (in), D (via A), and B itself (re-reached along the back-edge under global
uniqueness).  E is g-likes-only and excluded."
  (with-diamond-graph (g)
    (is (equal '("A" "B" "C" "D")
               (names-of (traverse b :graph g :direction :both
                                     :edge-type 'g-knows))))))

(test traverse-return-paths-gives-traversals
  "With :return-paths, traverse returns traversal objects; the one ending at C
has a two-edge path (A->B->C)."
  (with-diamond-graph (g)
    (let* ((paths (traverse a :graph g :direction :out
                              :edge-type 'g-knows :return-paths t))
           (to-c (find (id c) paths
                       :key (lambda (tr) (id (end-vertex tr)))
                       :test #'equalp)))
      (is (= 3 (length paths)) "expected traversals to B, C, D")
      (is-true to-c "a path ending at C should be present")
      (when to-c
        (is (= 2 (length (traversal-path to-c)))
            "A->B->C is two edges")
        (is (every (lambda (e) (typep e 'g-knows)) (traversal-path to-c)))))))

(test traverse-without-edge-type-returns-nil
  "With no edge-type, TRAVERSE collects nothing (typep edge nil is always
false) -- documents the current contract."
  (with-diamond-graph (g)
    (is (null (traverse a :graph g :direction :out)))))
