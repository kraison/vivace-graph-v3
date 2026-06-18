;;;; Deep / stress tests for the Prolog query engine (prologc.lisp,
;;;; prolog-functors.lisp): user-defined recursive rules via <-, multi-goal
;;;; joins, cut, state isolation across queries, and rule redefinition.
;;;;
;;;; SAFETY: a recursive rule over CYCLIC graph data exhausts the control
;;;; stack and fatally crashes SBCL (uncatchable), so every scenario here uses
;;;; ACYCLIC data, and each query runs under WITH-QUERY-TIMEOUT so an
;;;; unexpected non-termination fails the test instead of freezing the suite.
;;;;
;;;; Reuses the g-person / g-knows schema from graph-tests.lisp.

(in-package #:graph-db/test)

(def-suite prolog-stress-suite
  :description "Recursive rules, joins, cut, and engine state robustness."
  :in graph-db-suite)

(in-suite prolog-stress-suite)

#+sbcl
(defmacro with-query-timeout ((&optional (seconds 30)) &body body)
  `(handler-case (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout ()
       (error "Prolog query did not terminate within ~D s (possible hang)"
              ,seconds))))
#-sbcl
(defmacro with-query-timeout ((&optional seconds) &body body)
  (declare (ignore seconds))
  `(progn ,@body))

(defun build-knows-chain (n)
  "Create N g-person vertices named n0..n(N-1), linked in an acyclic chain
n0 -> n1 -> ... by g-knows.  Must run with *graph* bound to a test graph."
  (with-transaction ()
    (let ((nodes (loop for i below n
                       collect (make-g-person :name (format nil "n~d" i)))))
      (loop for (a b) on nodes while b do (make-g-knows :from a :to b)))))

(defun drop-rule (name arity)
  "Remove a user-defined Prolog rule (functor NAME/ARITY) from the global
table, if present.  Used to keep rule definitions from leaking across tests."
  (let ((f (lookup-functor (make-functor-symbol name arity))))
    (when f (delete-functor f))))

(defmacro in-test-package (&body body)
  "Evaluate BODY with *package* bound to GRAPH-DB/TEST.  The meta-call
functors (not / bagof / setof / if, via call/1) resolve the inner goal's
functor by interning its name in the *runtime* *package*, so it must be the
package where the functors are accessible."
  `(let ((*package* (find-package '#:graph-db/test)))
     ,@body))

(defmacro defining-reach (&body body)
  "Define the recursive rule reach/2 over g-knows (transitive closure) for the
extent of BODY, then remove it.  Cleans up first too, so a leak from an
earlier failure can't corrupt this run."
  `(progn
     (drop-rule 'reach 2)
     (<- (reach ?x ?y) (g-knows ?x ?y))
     (<- (reach ?x ?y) (g-knows ?x ?z) (reach ?z ?y))
     (unwind-protect (progn ,@body)
       (drop-rule 'reach 2))))

(test recursive-rule-definition-terminates
  "Regression for the prolog-compile-help hang: defining a recursive <- rule
must simply complete (it used to spin in the compiler forever)."
  (with-test-graph (g)
    (build-knows-chain 3)
    (finishes
      (with-query-timeout (15)
        (drop-rule 'reach 2)
        (<- (reach ?x ?y) (g-knows ?x ?y))
        (<- (reach ?x ?y) (g-knows ?x ?z) (reach ?z ?y))
        (drop-rule 'reach 2)))))

(test transitive-closure-over-dag
  "reach/2 over an acyclic 5-chain yields exactly the 10 reachable pairs."
  (with-test-graph (g)
    (build-knows-chain 5)
    (defining-reach
      (let ((pairs (with-query-timeout (20)
                     (select (:flat nil) (?x ?y) (reach ?x ?y)))))
        (is (= 10 (length pairs)))))))

(test reachability-from-a-source
  "Multi-goal join: everything reachable from n0 (recursion + node-slot-value
as both a generator constraint and a projection)."
  (with-test-graph (g)
    (build-knows-chain 5)
    (defining-reach
      (let ((names (with-query-timeout (20)
                     (select-flat (?yname)
                       (reach ?x ?y)
                       (node-slot-value ?x name "n0")
                       (node-slot-value ?y name ?yname)))))
        (is (= 4 (length names)))
        (is (equal '("n1" "n2" "n3" "n4") (sort (copy-list names) #'string<)))))))

(test deep-recursion-bounded
  "A long acyclic chain still terminates with the full closure from the head."
  (with-test-graph (g)
    (build-knows-chain 40)
    (defining-reach
      (let ((names (with-query-timeout (30)
                     (select-flat (?yname)
                       (reach ?x ?y)
                       (node-slot-value ?x name "n0")
                       (node-slot-value ?y name ?yname)))))
        (is (= 39 (length names)))))))

(test cut-stops-after-first-solution
  "Cut (!) prunes backtracking: is-a + ! yields exactly one solution."
  (with-test-graph (g)
    (build-knows-chain 5)
    (let ((one (with-query-timeout (15)
                 (select-flat (?p) (is-a ?p g-person) !))))
      (is (= 1 (length one))))))

(test state-isolation-after-recursive-query
  "After a recursive query, a plain query must still return correct results --
i.e. the engine's global state (*trail*, var counter, functor table) is not
left corrupted."
  (with-test-graph (g)
    (build-knows-chain 5)
    (defining-reach
      (with-query-timeout (20)
        (select (:flat nil) (?x ?y) (reach ?x ?y))))
    ;; reach is gone; a simple query over the same graph still works
    (is (= 5 (length (with-query-timeout (15)
                       (select-flat (?p) (is-a ?p g-person))))))))

(test repeated-queries-are-stable
  "Running the same recursive query many times yields the same result every
time (no global-state accumulation, no slowdown to a hang)."
  (with-test-graph (g)
    (build-knows-chain 5)
    (defining-reach
      (with-query-timeout (30)
        (dotimes (i 25)
          (is (= 10 (length (select (:flat nil) (?x ?y) (reach ?x ?y))))))))))

(test rule-redefinition-after-delete
  "Defining reach, deleting it, and redefining it gives the same answers --
clauses must not accumulate across define/delete cycles."
  (with-test-graph (g)
    (build-knows-chain 5)
    (defining-reach
      (is (= 10 (length (with-query-timeout (20)
                          (select (:flat nil) (?x ?y) (reach ?x ?y)))))))
    ;; defining-reach dropped it; do a second independent cycle
    (defining-reach
      (is (= 10 (length (with-query-timeout (20)
                          (select (:flat nil) (?x ?y) (reach ?x ?y)))))))))

;;; ---------------------------------------------------------------------------
;;; Control / aggregation functors: or, not, bagof, setof.
;;;
;;; or and = are CL symbols whose Prolog compiler macros resolve from any
;;; package.  not / bagof / setof go through call/1, whose runtime functor
;;; resolution is *package*-sensitive, so those queries run inside
;;; IN-TEST-PACKAGE.
;;; ---------------------------------------------------------------------------

(defun make-people (n)
  "Create N g-person vertices named n0..n(N-1)."
  (with-transaction ()
    (dotimes (i n) (make-g-person :name (format nil "n~d" i)))))

(test query-or
  "(or g1 g2) succeeds when either branch does."
  (with-test-graph (g)
    (make-people 4)
    (let ((names (with-query-timeout (15)
                   (select-flat (?n)
                     (is-a ?x g-person)
                     (node-slot-value ?x name ?n)
                     (or (= ?n "n0") (= ?n "n2"))))))
      (is (equal '("n0" "n2") (sort (copy-list names) #'string<))))))

(test query-not
  "(not g) succeeds when g fails -- here, every person except n0."
  (with-test-graph (g)
    (make-people 4)
    (let ((names (in-test-package
                   (with-query-timeout (15)
                     (select-flat (?n)
                       (is-a ?x g-person)
                       (node-slot-value ?x name ?n)
                       (not (node-slot-value ?x name "n0")))))))
      (is (= 3 (length names)))
      (is-false (member "n0" names :test #'string=)))))

(test query-bagof-collects-all-solutions
  "bagof binds its result to the list of all solutions for the template."
  (with-test-graph (g)
    (make-people 4)
    (let ((result (in-test-package
                    (with-query-timeout (15)
                      (select (:flat nil) (?bag)
                              (bagof ?x (is-a ?x g-person) ?bag))))))
      ;; one solution, whose single value is the 4-element bag
      (is (= 1 (length result)))
      (is (= 4 (length (first (first result))))))))

(test bagof-vs-setof-over-user-rule
  "bagof keeps duplicates, setof removes them.  Also a regression for the
call/1 fix: meta-call (bagof/setof) over a user-defined (<-) predicate.
Edges n0->n2, n1->n2, n0->n3 make \"n2\" a duplicate target name."
  (with-test-graph (g)
    (let (h)
      (setq h (make-hash-table :test 'equal))
      (with-transaction ()
        (dolist (nm '("n0" "n1" "n2" "n3"))
          (setf (gethash nm h) (make-g-person :name nm)))
        (make-g-knows :from (gethash "n0" h) :to (gethash "n2" h))
        (make-g-knows :from (gethash "n1" h) :to (gethash "n2" h))
        (make-g-knows :from (gethash "n0" h) :to (gethash "n3" h))))
    (drop-rule 'target-name 1)
    (<- (target-name ?n) (g-knows ?x ?y) (node-slot-value ?y name ?n))
    (unwind-protect
         (in-test-package
           (let ((bag (first (first (with-query-timeout (15)
                                      (select (:flat nil) (?b)
                                              (bagof ?n (target-name ?n) ?b))))))
                 (set (first (first (with-query-timeout (15)
                                      (select (:flat nil) (?s)
                                              (setof ?n (target-name ?n) ?s)))))))
             (is (= 3 (length bag)))                       ; n2 n2 n3
             (is (= 2 (length set)))                       ; n2 n3
             (is (equal '("n2" "n3") (sort (copy-list set) #'string<)))))
      (drop-rule 'target-name 1))))

;;; ---------------------------------------------------------------------------
;;; All-solutions aggregation (#45 Phase 0.3): findall, ^, free-var grouping,
;;; sorted setof.
;;; ---------------------------------------------------------------------------

(test findall-always-succeeds-empty-on-no-solutions
  "findall/3 binds the empty list (and still succeeds) when the goal has no
solutions -- unlike bagof/setof, which fail."
  (with-test-graph (g)
    (declare (ignore g))
    (is (equal '((()))
               (in-test-package
                 (with-query-timeout (15)
                   (select (:flat nil) (?l)
                           (findall ?x (is-a ?x g-person) ?l))))))
    ;; bagof over the same empty goal fails -> no solutions
    (is (null (in-test-package
                (with-query-timeout (15)
                  (select (:flat nil) (?l)
                          (bagof ?x (is-a ?x g-person) ?l))))))))

(test findall-collects-in-order-with-duplicates
  "findall keeps every solution, in order, including duplicates."
  (with-test-graph (g)
    (make-people 3)
    (let ((lst (first (first
                       (in-test-package
                         (with-query-timeout (15)
                           (select (:flat nil) (?l)
                                   (findall ?n
                                            (and (is-a ?x g-person)
                                                 (node-slot-value ?x name ?n))
                                            ?l))))))))
      (is (= 3 (length lst)))
      (is (equal '("n0" "n1" "n2") (sort (copy-list lst) #'string<))))))

(test bagof-groups-by-free-variable
  "bagof groups by the goal's free (witness) variable: one solution per source
vertex, each bag holding that source's targets."
  (with-test-graph (g)
    (declare (ignore g))
    ;; n0 -> {n1, n2}, n1 -> {n2}
    (with-transaction ()
      (let ((n0 (make-g-person :name "n0"))
            (n1 (make-g-person :name "n1"))
            (n2 (make-g-person :name "n2")))
        (make-g-knows :from n0 :to n1)
        (make-g-knows :from n0 :to n2)
        (make-g-knows :from n1 :to n2)))
    ;; ?x is a free var (witness): one group per source -> two solutions
    (let ((groups (in-test-package
                    (with-query-timeout (15)
                      (select (:flat nil) (?bag)
                              (bagof ?y (g-knows ?x ?y) ?bag))))))
      (is (= 2 (length groups)))
      (is (equal '(1 2) (sort (mapcar (lambda (row) (length (first row))) groups)
                              #'<))))))

(test bagof-existential-collapses-groups
  "^-quantifying the free variable removes it as a witness, collapsing all
solutions into a single bag."
  (with-test-graph (g)
    (declare (ignore g))
    (with-transaction ()
      (let ((n0 (make-g-person :name "n0"))
            (n1 (make-g-person :name "n1"))
            (n2 (make-g-person :name "n2")))
        (make-g-knows :from n0 :to n1)
        (make-g-knows :from n0 :to n2)
        (make-g-knows :from n1 :to n2)))
    ;; (^ ?x ...) makes ?x existential -> one group of all three targets
    (let ((groups (in-test-package
                    (with-query-timeout (15)
                      (select (:flat nil) (?bag)
                              (bagof ?y (^ ?x (g-knows ?x ?y)) ?bag))))))
      (is (= 1 (length groups)))
      (is (= 3 (length (first (first groups))))))))

(test setof-sorts-and-dedups-each-group
  "setof returns each group's list sorted by standard order with duplicates
removed; bagof over the same goal keeps the duplicate."
  (with-test-graph (g)
    (declare (ignore g))
    ;; numbers offered out of order, with a duplicate
    (let ((set (first (first
                       (in-test-package
                         (with-query-timeout (15)
                           (select (:flat nil) (?s)
                                   (setof ?n
                                          (or (= ?n 3) (= ?n 1) (= ?n 3) (= ?n 2))
                                          ?s)))))))
          (bag (first (first
                       (in-test-package
                         (with-query-timeout (15)
                           (select (:flat nil) (?b)
                                   (bagof ?n
                                          (or (= ?n 3) (= ?n 1) (= ?n 3) (= ?n 2))
                                          ?b))))))))
      (is (equal '(1 2 3) set))          ; sorted, de-duplicated
      (is (= 4 (length bag))))))         ; 3 1 3 2 -- order/dups preserved

;;; ---------------------------------------------------------------------------
;;; Query resource bounds (#45 Phase 0.4): inference budget + wall-clock timeout
;;; turn runaway / cyclic recursion into a catchable prolog-resource-error
;;; instead of a hang or an (uncatchable) control-stack crash.
;;; ---------------------------------------------------------------------------

(test inference-budget-bounds-recursion
  "A recursive query that exceeds its inference budget aborts with a catchable
prolog-resource-error; the same query with an ample budget succeeds normally."
  (with-test-graph (g)
    (build-knows-chain 6)               ; acyclic -> 15 reachable pairs
    (defining-reach
      (signals graph-db:prolog-resource-error
        (with-query-timeout (15)
          (select (:max-inferences 5) (?x ?y) (reach ?x ?y))))
      (is (= 15 (length
                 (with-query-timeout (15)
                   (select (:max-inferences 1000000) (?x ?y) (reach ?x ?y)))))))))

(test cyclic-recursion-fails-gracefully
  "Recursion over CYCLIC data -- which would otherwise overflow the control
stack -- fails with a catchable prolog-resource-error once a budget is set."
  (with-test-graph (g)
    (declare (ignore g))
    (with-transaction ()
      (let ((a (make-g-person :name "a"))
            (b (make-g-person :name "b")))
        (make-g-knows :from a :to b)
        (make-g-knows :from b :to a)))   ; a <-> b cycle
    (defining-reach
      (signals graph-db:prolog-resource-error
        (with-query-timeout (20)
          (select (:max-inferences 1000) (?x ?y) (reach ?x ?y)))))))

(test inference-budget-breaks-spin-loop
  "An inference budget breaks an otherwise-infinite (repeat ... fail) loop,
which never re-enters compile-call."
  (with-test-graph (g)
    (declare (ignore g))
    (signals graph-db:prolog-resource-error
      (in-test-package
        (with-query-timeout (15)
          (select (:max-inferences 1000) () (repeat) (fail)))))))

(test query-timeout-breaks-spin-loop
  "A wall-clock timeout (here 0 s -> the deadline is already past) breaks a
spin loop with a catchable prolog-resource-error."
  (with-test-graph (g)
    (declare (ignore g))
    (signals graph-db:prolog-resource-error
      (in-test-package
        (with-query-timeout (15)
          (select (:timeout 0) () (repeat) (fail)))))))

(test unbounded-query-still-runs-by-default
  "With no bound (the default), an ordinary query is unaffected."
  (with-test-graph (g)
    (build-knows-chain 5)
    (is (= 5 (length (select-flat (?p) (is-a ?p g-person)))))))
