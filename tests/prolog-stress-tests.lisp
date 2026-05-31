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
