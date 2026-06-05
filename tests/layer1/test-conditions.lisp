;;;; -*- Mode: Lisp; Package: :vg-tests -*-
;;;; tests/layer1/test-conditions.lisp
;;;;
;;;; Comprehensive test suite for src/conditions.lisp
;;;; Tests all 12 exception classes and their behavior

(in-package :vg-tests)

(def-suite l1.conditions
  :description "Layer 1: Exception classes and condition handling"
  :in nil)

(in-suite l1.conditions)

;;;; ============================================================================
;;;; SLAVE-AUTH-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.slave-auth-error-instantiate
  "Can create slave-auth-error with reason and host."
  (let ((error (make-condition 'graph-db:slave-auth-error
                              :reason "Invalid key"
                              :host "192.168.1.1")))
    (is (slot-value error 'graph-db::reason))
    (is (slot-value error 'graph-db::host))))

(test test-l1.conditions.slave-auth-error-slots
  "Slave-auth-error slots are accessible."
  (let ((error (make-condition 'graph-db:slave-auth-error
                              :reason "Wrong credentials"
                              :host "slave.example.com")))
    (is (equal (graph-db:slave-auth-reason error) "Wrong credentials"))
    (is (equal (graph-db:slave-auth-host error) "slave.example.com"))))

(test test-l1.conditions.slave-auth-error-report
  "Slave-auth-error :report produces correct message."
  (let ((error (make-condition 'graph-db:slave-auth-error
                              :reason "Key mismatch"
                              :host "192.168.1.100")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Slave auth error" msg))
      (is (search "192.168.1.100" msg))
      (is (search "Key mismatch" msg)))))

;;;; ============================================================================
;;;; TRANSACTION-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.transaction-error-instantiate
  "Can create transaction-error with reason."
  (let ((error (make-condition 'graph-db:transaction-error
                              :reason "Constraint violation")))
    (is (slot-value error 'graph-db::reason))))

(test test-l1.conditions.transaction-error-slot
  "Transaction-error reason is accessible."
  (let ((error (make-condition 'graph-db:transaction-error
                              :reason "Deadlock detected")))
    (is (equal (graph-db:transaction-error-reason error) "Deadlock detected"))))

(test test-l1.conditions.transaction-error-report
  "Transaction-error :report produces correct message."
  (let ((error (make-condition 'graph-db:transaction-error
                              :reason "Lock timeout")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Transaction error" msg))
      (is (search "Lock timeout" msg)))))

;;;; ============================================================================
;;;; SERIALIZATION-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.serialization-error-instantiate
  "Can create serialization-error with instance and reason."
  (let ((error (make-condition 'graph-db:serialization-error
                              :instance #(1 2 3)
                              :reason "Unknown type")))
    (is (slot-value error 'graph-db::instance))
    (is (slot-value error 'graph-db::reason))))

(test test-l1.conditions.serialization-error-slots
  "Serialization-error slots are accessible."
  (let ((obj "test-object")
        (reason "Unsupported type"))
    (let ((error (make-condition 'graph-db:serialization-error
                                :instance obj
                                :reason reason)))
      (is (equal (graph-db:serialization-error-instance error) obj))
      (is (equal (graph-db:serialization-error-reason error) reason)))))

(test test-l1.conditions.serialization-error-report
  "Serialization-error :report includes instance and reason."
  (let ((error (make-condition 'graph-db:serialization-error
                              :instance "my-vertex"
                              :reason "Bad slot type")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Serialization failed" msg))
      (is (search "my-vertex" msg))
      (is (search "Bad slot type" msg)))))

;;;; ============================================================================
;;;; DESERIALIZATION-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.deserialization-error-instantiate
  "Can create deserialization-error with instance and reason."
  (let ((error (make-condition 'graph-db:deserialization-error
                              :instance #(0 1 2)
                              :reason "Bad type code")))
    (is (slot-value error 'graph-db::instance))
    (is (slot-value error 'graph-db::reason))))

(test test-l1.conditions.deserialization-error-slots
  "Deserialization-error slots are accessible."
  (let ((data "corrupted-bytes")
        (reason "Invalid format"))
    (let ((error (make-condition 'graph-db:deserialization-error
                                :instance data
                                :reason reason)))
      (is (equal (graph-db:deserialization-error-instance error) data))
      (is (equal (graph-db:deserialization-error-reason error) reason)))))

(test test-l1.conditions.deserialization-error-report
  "Deserialization-error :report includes instance and reason."
  (let ((error (make-condition 'graph-db:deserialization-error
                              :instance "bad-bytes"
                              :reason "Type code 255")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Deserialization failed" msg))
      (is (search "bad-bytes" msg))
      (is (search "Type code 255" msg)))))

;;;; ============================================================================
;;;; STALE-REVISION-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.stale-revision-error-instantiate
  "Can create stale-revision-error with instance and revision."
  (let ((error (make-condition 'graph-db:stale-revision-error
                              :instance "vertex-123"
                              :current-revision 5)))
    (is (slot-value error 'graph-db::instance))
    (is (slot-value error 'graph-db::current-revision))))

(test test-l1.conditions.stale-revision-error-slots
  "Stale-revision-error slots are accessible."
  (let ((error (make-condition 'graph-db:stale-revision-error
                              :instance "my-node"
                              :current-revision 10)))
    (is (equal (graph-db:stale-revision-error-instance error) "my-node"))
    (is (= (graph-db:stale-revision-error-revision error) 10))))

(test test-l1.conditions.stale-revision-error-report
  "Stale-revision-error :report includes instance and revision."
  (let ((error (make-condition 'graph-db:stale-revision-error
                              :instance "vertex"
                              :current-revision 3)))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Attempt to update stale revision" msg))
      (is (search "vertex" msg))
      (is (search "3" msg)))))

;;;; ============================================================================
;;;; DUPLICATE-KEY-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.duplicate-key-error-instantiate
  "Can create duplicate-key-error with instance and key."
  (let ((error (make-condition 'graph-db:duplicate-key-error
                              :instance "index"
                              :key #(1 2 3))))
    (is (slot-value error 'graph-db::instance))
    (is (slot-value error 'graph-db::key))))

(test test-l1.conditions.duplicate-key-error-slots
  "Duplicate-key-error slots are accessible."
  (let ((key #(255 254 253)))
    (let ((error (make-condition 'graph-db:duplicate-key-error
                                :instance "type-index"
                                :key key)))
      (is (equal (graph-db:duplicate-key-error-instance error) "type-index"))
      (is (equalp (graph-db:duplicate-key-error-key error) key)))))

(test test-l1.conditions.duplicate-key-error-report
  "Duplicate-key-error :report includes instance and key."
  (let ((error (make-condition 'graph-db:duplicate-key-error
                              :instance "ve-index"
                              :key "key-value")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Duplicate key" msg))
      (is (search "key-value" msg))
      (is (search "ve-index" msg)))))

;;;; ============================================================================
;;;; NONEXISTENT-KEY-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.nonexistent-key-error-instantiate
  "Can create nonexistent-key-error with instance and key."
  (let ((error (make-condition 'graph-db:nonexistent-key-error
                              :instance "index"
                              :key #(1 2 3))))
    (is (slot-value error 'graph-db::instance))
    (is (slot-value error 'graph-db::key))))

(test test-l1.conditions.nonexistent-key-error-slots
  "Nonexistent-key-error slots are accessible."
  (let ((key #(0 0 0)))
    (let ((error (make-condition 'graph-db:nonexistent-key-error
                                :instance "skip-list"
                                :key key)))
      (is (equal (graph-db:nonexistent-key-error-instance error) "skip-list"))
      (is (equalp (graph-db:nonexistent-key-error-key error) key)))))

(test test-l1.conditions.nonexistent-key-error-report
  "Nonexistent-key-error :report includes instance and key."
  (let ((error (make-condition 'graph-db:nonexistent-key-error
                              :instance "lhash"
                              :key "missing")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Nonexistent key" msg))
      (is (search "missing" msg))
      (is (search "lhash" msg)))))

;;;; ============================================================================
;;;; NODE-ALREADY-DELETED-ERROR TESTS (BASE CLASS)
;;;; ============================================================================

(test test-l1.conditions.node-already-deleted-error-instantiate
  "Can create node-already-deleted-error with node."
  (let ((error (make-condition 'graph-db:node-already-deleted-error
                              :node "vertex-123")))
    (is (slot-value error 'graph-db::node))))

(test test-l1.conditions.node-already-deleted-error-slot
  "Node-already-deleted-error node is accessible."
  (let ((error (make-condition 'graph-db:node-already-deleted-error
                              :node "my-node")))
    (is (equal (graph-db:node-already-deleted-node error) "my-node"))))

(test test-l1.conditions.node-already-deleted-error-report
  "Node-already-deleted-error :report includes node."
  (let ((error (make-condition 'graph-db:node-already-deleted-error
                              :node "vertex-456")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "Node" msg))
      (is (search "vertex-456" msg))
      (is (search "already deleted" msg)))))

;;;; ============================================================================
;;;; VERTEX-ALREADY-DELETED-ERROR TESTS (SUBCLASS)
;;;; ============================================================================

(test test-l1.conditions.vertex-already-deleted-error-instantiate
  "Can create vertex-already-deleted-error with node."
  (let ((error (make-condition 'graph-db:vertex-already-deleted-error
                              :node "vertex")))
    (is (slot-value error 'graph-db::node))))

(test test-l1.conditions.vertex-already-deleted-error-slot
  "Vertex-already-deleted-error inherits node slot."
  (let ((error (make-condition 'graph-db:vertex-already-deleted-error
                              :node "my-vertex")))
    (is (equal (graph-db:node-already-deleted-node error) "my-vertex"))))

(test test-l1.conditions.vertex-already-deleted-error-is-node-error
  "Vertex-already-deleted-error is subclass of node-already-deleted-error."
  (let ((error (make-condition 'graph-db:vertex-already-deleted-error
                              :node "v")))
    (is (typep error 'graph-db:node-already-deleted-error))))

(test test-l1.conditions.vertex-already-deleted-error-catch-specific
  "Can catch vertex-already-deleted-error specifically."
  (let ((caught nil))
    (handler-case
      (error 'graph-db:vertex-already-deleted-error :node "v")
      (graph-db:vertex-already-deleted-error () (setf caught t)))
    (is caught)))

(test test-l1.conditions.vertex-already-deleted-error-catch-parent
  "Vertex-already-deleted-error caught by parent type."
  (let ((caught nil))
    (handler-case
      (error 'graph-db:vertex-already-deleted-error :node "v")
      (graph-db:node-already-deleted-error () (setf caught t)))
    (is caught)))

;;;; ============================================================================
;;;; EDGE-ALREADY-DELETED-ERROR TESTS (SUBCLASS)
;;;; ============================================================================

(test test-l1.conditions.edge-already-deleted-error-instantiate
  "Can create edge-already-deleted-error with node."
  (let ((error (make-condition 'graph-db:edge-already-deleted-error
                              :node "edge")))
    (is (slot-value error 'graph-db::node))))

(test test-l1.conditions.edge-already-deleted-error-slot
  "Edge-already-deleted-error inherits node slot."
  (let ((error (make-condition 'graph-db:edge-already-deleted-error
                              :node "my-edge")))
    (is (equal (graph-db:node-already-deleted-node error) "my-edge"))))

(test test-l1.conditions.edge-already-deleted-error-is-node-error
  "Edge-already-deleted-error is subclass of node-already-deleted-error."
  (let ((error (make-condition 'graph-db:edge-already-deleted-error
                              :node "e")))
    (is (typep error 'graph-db:node-already-deleted-error))))

(test test-l1.conditions.edge-already-deleted-error-catch-specific
  "Can catch edge-already-deleted-error specifically."
  (let ((caught nil))
    (handler-case
      (error 'graph-db:edge-already-deleted-error :node "e")
      (graph-db:edge-already-deleted-error () (setf caught t)))
    (is caught)))

(test test-l1.conditions.edge-already-deleted-error-catch-parent
  "Edge-already-deleted-error caught by parent type."
  (let ((caught nil))
    (handler-case
      (error 'graph-db:edge-already-deleted-error :node "e")
      (graph-db:node-already-deleted-error () (setf caught t)))
    (is caught)))

;;;; ============================================================================
;;;; INVALID-VIEW-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.invalid-view-error-instantiate
  "Can create invalid-view-error with class and view names."
  (let ((error (make-condition 'graph-db:invalid-view-error
                              :class-name "PERSON"
                              :view-name "AGE_DIST")))
    (is (slot-value error 'graph-db::class-name))
    (is (slot-value error 'graph-db::view-name))))

(test test-l1.conditions.invalid-view-error-slots
  "Invalid-view-error slots are accessible."
  (let ((error (make-condition 'graph-db:invalid-view-error
                              :class-name "USER"
                              :view-name "STATS")))
    (is (equal (graph-db:invalid-view-error-class error) "USER"))
    (is (equal (graph-db:invalid-view-error-view error) "STATS"))))

(test test-l1.conditions.invalid-view-error-report
  "Invalid-view-error :report includes class and view names."
  (let ((error (make-condition 'graph-db:invalid-view-error
                              :class-name "PERSON"
                              :view-name "MISSING")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "No such graph view" msg))
      (is (search "PERSON" msg))
      (is (search "MISSING" msg))
      (is (search "/" msg)))))

;;;; ============================================================================
;;;; VIEW-LOCK-ERROR TESTS
;;;; ============================================================================

(test test-l1.conditions.view-lock-error-instantiate
  "Can create view-lock-error with message."
  (let ((error (make-condition 'graph-db:view-lock-error
                              :message "Deadlock")))
    (is (slot-value error 'graph-db::message))))

(test test-l1.conditions.view-lock-error-slot
  "View-lock-error message is accessible."
  (let ((error (make-condition 'graph-db:view-lock-error
                              :message "Timeout")))
    (is (equal (graph-db:view-lock-error-message error) "Timeout"))))

(test test-l1.conditions.view-lock-error-report
  "View-lock-error :report includes message."
  (let ((error (make-condition 'graph-db:view-lock-error
                              :message "Lock conflict")))
    (let ((msg (with-output-to-string (s)
                 (print-condition error s))))
      (is (search "View locking error" msg))
      (is (search "Lock conflict" msg)))))

;;;; ============================================================================
;;;; HIERARCHY TESTS
;;;; ============================================================================

(test test-l1.conditions.deletion-error-hierarchy
  "Vertex and edge deletion errors are subclasses of node error."
  (is (subtypep 'graph-db:vertex-already-deleted-error 'graph-db:node-already-deleted-error))
  (is (subtypep 'graph-db:edge-already-deleted-error 'graph-db:node-already-deleted-error)))

(test test-l1.conditions.all-errors-subclass-of-error
  "All conditions are subclasses of standard error."
  (is (subtypep 'graph-db:slave-auth-error 'error))
  (is (subtypep 'graph-db:transaction-error 'error))
  (is (subtypep 'graph-db:serialization-error 'error))
  (is (subtypep 'graph-db:deserialization-error 'error))
  (is (subtypep 'graph-db:stale-revision-error 'error))
  (is (subtypep 'graph-db:duplicate-key-error 'error))
  (is (subtypep 'graph-db:nonexistent-key-error 'error))
  (is (subtypep 'graph-db:node-already-deleted-error 'error))
  (is (subtypep 'graph-db:invalid-view-error 'error))
  (is (subtypep 'graph-db:view-lock-error 'error)))

;;;; ============================================================================
;;;; INTEGRATION TESTS
;;;; ============================================================================

(test test-l1.conditions.error-raise-catch
  "Can raise and catch exceptions."
  (let ((caught nil))
    (handler-case
      (error 'graph-db:transaction-error :reason "test")
      (graph-db:transaction-error () (setf caught t)))
    (is caught)))

(test test-l1.conditions.multiple-handlers
  "Multiple handler cases work correctly."
  (let ((caught-specific nil) (caught-generic nil))
    (handler-case
      (error 'graph-db:duplicate-key-error :instance "i" :key "k")
      (graph-db:duplicate-key-error () (setf caught-specific t))
      (error () (setf caught-generic t)))
    (is (and caught-specific (not caught-generic)))))

(test test-l1.conditions.handler-case-rethrow
  "Can re-raise condition with additional context."
  (let ((caught nil))
    (handler-case
      (handler-case
        (error 'graph-db:serialization-error
               :instance "obj"
               :reason "type")
        (graph-db:serialization-error (e)
          (error 'graph-db:transaction-error
                 :reason (format nil "Serialization failed: ~a"
                               (graph-db:serialization-error-reason e)))))
      (graph-db:transaction-error () (setf caught t)))
    (is caught)))

(test test-l1.conditions.all-conditions-have-readers
  "All exception classes have slot reader functions."
  (is (fboundp 'graph-db:slave-auth-reason))
  (is (fboundp 'graph-db:slave-auth-host))
  (is (fboundp 'graph-db:transaction-error-reason))
  (is (fboundp 'graph-db:serialization-error-instance))
  (is (fboundp 'graph-db:serialization-error-reason))
  (is (fboundp 'graph-db:deserialization-error-instance))
  (is (fboundp 'graph-db:deserialization-error-reason))
  (is (fboundp 'graph-db:stale-revision-error-instance))
  (is (fboundp 'graph-db:stale-revision-error-revision))
  (is (fboundp 'graph-db:duplicate-key-error-instance))
  (is (fboundp 'graph-db:duplicate-key-error-key))
  (is (fboundp 'graph-db:nonexistent-key-error-instance))
  (is (fboundp 'graph-db:nonexistent-key-error-key))
  (is (fboundp 'graph-db:node-already-deleted-node))
  (is (fboundp 'graph-db:invalid-view-error-class))
  (is (fboundp 'graph-db:invalid-view-error-view))
  (is (fboundp 'graph-db:view-lock-error-message)))

;;;; ============================================================================
;;;; TEST RUNNER
;;;; ============================================================================

(defun run-l1-conditions-tests ()
  "Run all Layer 1 Conditions tests."
  (let* ((results (run! 'l1.conditions))
         (passed (fiveam:results-passed results))
         (failed (fiveam:results-failed results))
         (skipped (fiveam:results-skipped results)))
    (format t "~&Layer 1 Conditions Tests Summary:~%")
    (format t "  Passed:  ~D~%" passed)
    (format t "  Failed:  ~D~%" failed)
    (format t "  Skipped: ~D~%" skipped)
    (format t "  Total:   ~D~%" (+ passed failed skipped))
    (zerop failed)))

(in-package :cl-user)
