;;;; CONCURRENT-PROLOG-SUITE
;;;;
;;;; Tests Prolog query engine and functor state under concurrent access.
;;;;
;;;; *user-functors* is a global hash table shared by all threads.  The three
;;;; tests check:
;;;;   1. Concurrent queries do not interfere (no shared mutable per-query state)
;;;;   2. Concurrent clause additions via add-functor-clause are safe (the fix
;;;;      from functor.lisp is exercised directly)
;;;;   3. Readers querying while writers add clauses do not deadlock or error

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-prolog-suite
  :description "Prolog query engine thread-safety."
  :in concurrency-suite)

(in-suite concurrent-prolog-suite)

;;; ---------------------------------------------------------------------------
;;; Helper: generate a fresh functor symbol unique to this test invocation.
;;; Using a unique name prevents leftover state from prior runs or from the
;;; sequential test suite polluting these tests.
;;; ---------------------------------------------------------------------------

(defun fresh-functor-name (base arity)
  "Return a fresh intern'd symbol like BASE/ARITY-<random> in this package."
  (intern (format nil "~A/~A-~36R" base arity (random (expt 36 8)))))

;;; ---------------------------------------------------------------------------
;;; Test 1: concurrent queries produce consistent results
;;;
;;; N threads each run the same select-flat query against the same data.
;;; All N result counts must be equal (no shared mutable query state that
;;; corrupts results across threads).
;;; ---------------------------------------------------------------------------

(test concurrent-queries-no-interference
  "N threads running the same query simultaneously must all get the same count."
  (with-conc-graph (g)
    (with-transaction ()
      (dotimes (i 12)
        (make-c-item :value i)))
    (let ((counts (make-array *thread-count* :initial-element -1)))
      (run-threads *thread-count*
                   (lambda (i)
                     (setf (aref counts i)
                           (length (select-flat (?x)
                                     (is-a ?x c-item))))))
      ;; All threads must agree on the count.
      (let ((expected (aref counts 0)))
        (is (> expected 0) "Expected at least 1 result; got ~D" expected)
        (dotimes (i *thread-count*)
          (is (= expected (aref counts i))
              "Thread ~D got ~D; expected ~D" i (aref counts i) expected))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: concurrent clause additions are safe
;;;
;;; Pre-create a functor with one seed clause; then N threads each call
;;; add-functor-clause once under contention.  Final clause count = N + 1.
;;; This directly exercises the lock+nconc fix in functor.lisp.
;;; ---------------------------------------------------------------------------

(test concurrent-rule-addition
  "N concurrent add-functor-clause calls must all take effect."
  (let* ((fname  (fresh-functor-name 'c-conc-rule 1))
         (seed   (list fname '?x))
         (f      (make-functor :name fname :clauses (list (list seed)))))
    (unwind-protect
         (progn
           (run-threads *thread-count*
                        (lambda (i)
                          (declare (ignore i))
                          (add-functor-clause f (list (list fname (gensym "ARG"))))))
           (is (= (1+ *thread-count*)
                  (length (functor-clauses f)))
               "Expected ~D clauses; got ~D"
               (1+ *thread-count*)
               (length (functor-clauses f))))
      ;; Clean up: remove from global functor table.
      (when (lookup-functor fname)
        (delete-functor (lookup-functor fname))))))

;;; ---------------------------------------------------------------------------
;;; Test 3: queries while clauses are being added
;;;
;;; N/2 writer threads add clauses to a rule while N/2 reader threads run
;;; select-flat against existing data.  No thread should error or deadlock.
;;; ---------------------------------------------------------------------------

(test concurrent-query-during-rule-addition
  "Queries running while clauses are added must not error or deadlock."
  (with-conc-graph (g)
    (with-transaction ()
      (dotimes (i 8) (make-c-item :value i)))
    (let* ((n        *thread-count*)
           (writers  (max 1 (floor n 2)))
           (readers  (- n writers))
           (fname    (fresh-functor-name 'c-mixed-rule 1))
           (seed     (list (list fname '?x)))
           (f        (make-functor :name fname :clauses (list (list (list fname '?x))))))
      (unwind-protect
           (progn
             (run-threads n
                          (lambda (i)
                            (if (< i writers)
                                ;; Writer: add clauses
                                (dotimes (_ 5)
                                  (add-functor-clause
                                   f
                                   (list (list fname (gensym "A")))))
                                ;; Reader: run a query against c-item data
                                (dotimes (_ 5)
                                  (select-flat (?x)
                                    (is-a ?x c-item))))))
             (pass))
        (declare (ignore seed readers))
        (when (lookup-functor fname)
          (delete-functor (lookup-functor fname)))))))
