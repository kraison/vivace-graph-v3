;;;; tests/layer1/test-package.lisp
;;;; Package definition tests (minimal — mostly compile-time checks)

(in-package :cl-user)

(def-suite l1.package
  :description "Layer 1: Package definition tests"
  :in nil)

(in-suite l1.package)

(test test-l1.package.graph-db-exists
  "Verify :graph-db package is defined."
  (is (find-package :graph-db)))

(test test-l1.package.exports-graph-lifecycle
  "Verify graph lifecycle functions are exported."
  (let ((pkg (find-package :graph-db)))
    (is (find-symbol "MAKE-GRAPH" pkg))
    (is (find-symbol "OPEN-GRAPH" pkg))
    (is (find-symbol "CLOSE-GRAPH" pkg))
    (is (find-symbol "LOOKUP-GRAPH" pkg))))

(test test-l1.package.exports-transactions
  "Verify transaction functions are exported."
  (let ((pkg (find-package :graph-db)))
    (is (find-symbol "WITH-TRANSACTION" pkg))
    (is (find-symbol "EXECUTE-TX" pkg))
    (is (find-symbol "COMMIT" pkg))
    (is (find-symbol "ROLLBACK" pkg))))

(test test-l1.package.exports-node-operations
  "Verify node operations are exported."
  (let ((pkg (find-package :graph-db)))
    (is (find-symbol "DEF-VERTEX" pkg))
    (is (find-symbol "DEF-EDGE" pkg))
    (is (find-symbol "MAKE-VERTEX" pkg))
    (is (find-symbol "MAKE-EDGE" pkg))))

(test test-l1.package.exports-views
  "Verify view operations are exported."
  (let ((pkg (find-package :graph-db)))
    (is (find-symbol "DEF-VIEW" pkg))
    (is (find-symbol "MAP-VIEW" pkg))))

(test test-l1.package.exports-prolog
  "Verify Prolog operations are exported."
  (let ((pkg (find-package :graph-db)))
    (is (find-symbol "DEF-GLOBAL-PROLOG-FUNCTOR" pkg))
    (is (find-symbol "UNIFY" pkg))
    (is (find-symbol "SELECT" pkg))
    (is (find-symbol "?" pkg))))

(test test-l1.package.exports-replication
  "Verify replication operations are exported."
  (let ((pkg (find-package :graph-db)))
    (is (find-symbol "START-REPLICATION" pkg))
    (is (find-symbol "STOP-REPLICATION" pkg))))

(test test-l1.package.exports-rest
  "Verify REST operations are exported."
  (let ((pkg (find-package :graph-db)))
    (is (find-symbol "START-REST" pkg))
    (is (find-symbol "STOP-REST" pkg))))

(defun run-l1-package-tests ()
  "Run all Layer 1 Package tests."
  (let* ((results (run! 'l1.package))
         (passed (fiveam:results-passed results))
         (failed (fiveam:results-failed results)))
    (format t "~&Layer 1 Package Tests: ~D passed, ~D failed~%" passed failed)
    (zerop failed)))
