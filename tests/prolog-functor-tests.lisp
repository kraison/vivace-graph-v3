;;;; Tests for the built-in Prolog functors (prolog-functors.lisp): unification
;;;; and (in)equality (=, ==, /=), numeric comparison (<, >, <=, >=), arithmetic
;;;; (is), type checks (numberp, atom, var), regex-match, the lisp escape, and
;;;; the control functors call/1, if/2, if/3, plus unique/1 deduplication.
;;;;
;;;; These predicates are mostly pure logic, but a query still needs a live
;;;; *graph*, so we wrap each in with-test-graph.

(in-package #:graph-db/test)

(def-suite prolog-functor-suite
  :description "Built-in Prolog functors: =, comparison, is, type checks, control."
  :in graph-db-suite)

(in-suite prolog-functor-suite)

(test eq-unifies-and-can-fail
  "=/2 unifies a var with a value; unifying two incompatible bound values fails."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(42) (select-flat (?x) (= ?x 42))))
    (is (null (select-flat (?x) (= ?x 1) (= ?x 2))))))

(test equality-and-inequality-functors
  "==/2 succeeds on equal derefs, /=/2 on unequal."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (== ?x 5))))
    (is (null         (select-flat (?x) (= ?x 5) (== ?x 6))))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (/= ?x 6))))
    (is (null         (select-flat (?x) (= ?x 5) (/= ?x 5))))))

(test numeric-comparison-functors
  "</2 >/2 <=/2 >=/2 compare bound numbers."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (< ?x 10))))
    (is (null         (select-flat (?x) (= ?x 5) (< ?x 3))))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (> ?x 4))))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (>= ?x 5) (<= ?x 5))))
    (is (null         (select-flat (?x) (= ?x 5) (> ?x 5))))))

(test is-evaluates-arithmetic
  "is/2 unifies a var with the value of a Lisp arithmetic expression."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(42) (select-flat (?x) (is ?x (+ 40 2)))))
    (is (equal '(6)  (select-flat (?x) (is ?x (* 3 (+ 1 1))))))))

(test type-check-functors
  "numberp/1, atom/1 and var/1 gate on the term's type/binding."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (= ?x 5) (numberp ?x))))
    (is (null         (select-flat (?x) (= ?x foo) (numberp ?x))))
    (is (equal '(foo) (select-flat (?x) (= ?x foo) (atom ?x))))
    ;; var/1: succeeds while unbound, fails once bound
    (is (equal '(7) (select-flat (?x) (var ?x) (= ?x 7))))
    (is (null         (select-flat (?x) (= ?x 7) (var ?x))))))

(test regex-match-functor
  "regex-match/2 scans the second string for the first (a regex)."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '("hello world")
               (select-flat (?x) (= ?x "hello world") (regex-match "wor" ?x))))
    (is (null (select-flat (?x) (= ?x "hello world") (regex-match "^xyz" ?x))))))

(test lisp-escape-functor
  "lisp/2 unifies a var with the result of evaluating a Lisp form."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(5) (select-flat (?x) (lisp ?x (+ 2 3)))))
    (is (equal '((1 2 3)) (select-flat (?x) (lisp ?x (list 1 2 3)))))))

(test call-functor-invokes-goal
  "call/1 invokes a (possibly computed) goal."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(7) (select-flat (?x) (= ?x 7) (call (< ?x 10)))))
    (is (null         (select-flat (?x) (= ?x 7) (call (> ?x 10)))))))

(test if-3-chooses-branch
  "if/3 runs the THEN branch when the test succeeds, else the ELSE branch."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(:yes) (select-flat (?r) (if (< 1 2) (= ?r :yes) (= ?r :no)))))
    (is (equal '(:no)  (select-flat (?r) (if (< 2 1) (= ?r :yes) (= ?r :no)))))))

(test if-2-runs-then-on-success-else-fails
  "if/2 runs THEN when the test succeeds; when the test fails the goal fails
(standard Prolog Cond -> Then with no else), so the whole branch yields nothing."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '(99) (select-flat (?x) (if (< 1 2) (= ?x 99)))))
    ;; test false -> continuation never invoked -> branch fails -> no solutions
    (is (null (select-flat (?x) (= ?x 5) (if (< 2 1) (= ?x 99)))))))

(test unique-deduplicates-nodes
  "unique/1 collapses repeated bindings of the same node across solutions."
  (with-test-graph (g)
    (let (a)
      (with-transaction ()
        (setq a (make-g-person :name "A"))
        (make-g-knows :from a :to (make-g-person :name "B"))
        (make-g-knows :from a :to (make-g-person :name "C")))
      ;; A is the source of two edges -> two solutions for ?a without unique
      (is (= 2 (length (select-flat (?a) (g-knows ?a ?b)))))
      ;; unique/1 collapses them to one
      (is (= 1 (length (select-flat (?a) (g-knows ?a ?b) (unique ?a))))))))
