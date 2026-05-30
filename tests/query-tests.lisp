;;;; Tests for the embedded Prolog query language (prologc.lisp,
;;;; prolog-functors.lisp): select / select-flat / select-one over a graph,
;;;; using the is-a/2 built-in and the generated edge functors.
;;;;
;;;; Reuses the schema (g-person, g-knows) from graph-tests.lisp.  Queries run
;;;; against the dynamically-bound *graph* from WITH-TEST-GRAPH.

(in-package #:graph-db/test)

(def-suite query-suite
  :description "Prolog select queries over a graph."
  :in graph-db-suite)

(in-suite query-suite)

(test select-flat-is-a
  "select-flat with is-a returns every vertex of a type."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-person :name "C"))
    (let ((people (select-flat (?p) (is-a ?p g-person))))
      (is (= 3 (length people)))
      (is (every (lambda (p) (typep p 'g-person)) people)))))

(test select-one-returns-single
  (with-test-graph (g)
    (with-transaction () (make-g-person :name "Solo"))
    (let ((p (select-one (?p) (is-a ?p g-person))))
      (is-true (typep p 'g-person))
      (is (string= "Solo" (slot-value p 'name))))))

(test select-no-matches-is-nil
  (with-test-graph (g)
    (is (null (select-flat (?p) (is-a ?p g-person))))))

(test select-edge-functor-pairs
  "The generated <edge>/2 functor yields connected (from to) vertex pairs."
  (with-test-graph (g)
    (let (aid bid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B")))
          (setq aid (id a) bid (id b))
          (make-g-knows :from a :to b)))
      (let ((pairs (select (:flat nil) (?a ?b) (g-knows ?a ?b))))
        (is (= 1 (length pairs)))
        (destructuring-bind (a b) (first pairs)
          (is (equalp aid (id a)))
          (is (equalp bid (id b))))))))

(test select-join-multiple-goals
  "A multi-goal query joins is-a and the edge functor: all (person, known)
pairs.  With A->B and A->C, both pairs are sourced from A."
  (with-test-graph (g)
    (let (aid bid cid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B"))
              (c (make-g-person :name "C")))
          (setq aid (id a) bid (id b) cid (id c))
          (make-g-knows :from a :to b)
          (make-g-knows :from a :to c)))
      (let ((pairs (select (:flat nil) (?a ?b)
                           (is-a ?a g-person)
                           (g-knows ?a ?b))))
        (is (= 2 (length pairs)))
        (is (every (lambda (pair) (equalp aid (id (first pair)))) pairs))
        (let ((targets (mapcar (lambda (pair) (id (second pair))) pairs)))
          (is-true (member bid targets :test #'equalp))
          (is-true (member cid targets :test #'equalp)))))))
