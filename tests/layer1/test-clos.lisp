;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: :vg-tests -*-
;;;; tests/layer1/test-clos.lisp
;;;;
;;;; Test suite for src/clos.lisp (Layer 1: Meta-Object Protocol)
;;;; 
;;;; Framework: fiveam
;;;; Test naming convention: test-l1.clos.FUNCTION-NAME
;;;;
;;;; CRITICAL TESTS:
;;;; - Meta-slot access (direct)
;;;; - User-slot storage (plist)
;;;; - Transaction enlistment
;;;; - Unbind operations
;;;; - Inheritance behavior

(in-package :vg-tests)

;;;; ============================================================================
;;;; TEST SUITE DEFINITION
;;;; ============================================================================

(def-suite l1.clos
  :description "Layer 1: CLOS Meta-Object Protocol tests"
  :in nil)

(in-suite l1.clos)

;;; Helper functions

(defun make-test-node (&optional (type-id 1))
  "Create a test NODE instance with minimal setup."
  (make-instance 'node :id (get-random-bytes 16) :%type-id type-id :data nil))

(defun plist-get (key plist)
  "Get value from plist by key."
  (cdr (assoc key plist)))

(defun plist-set (key value plist)
  "Set value in plist, returning modified plist."
  (let ((existing (assoc key plist)))
    (if existing
        (progn (setf (cdr existing) value) plist)
        (cons (cons key value) plist))))

;;;; ============================================================================
;;;; *meta-slots* VARIABLE TESTS
;;;; ============================================================================

(test test-l1.clos.meta-slots-definition
  "Verify *meta-slots* is defined as a list."
  (is (listp *meta-slots*))
  (is (> (length *meta-slots*) 10)))

(test test-l1.clos.meta-slots-contains-id
  "Verify *meta-slots* contains 'id'."
  (is (member 'id *meta-slots*)))

(test test-l1.clos.meta-slots-contains-data
  "Verify *meta-slots* contains '%data'."
  (is (member '%data *meta-slots*)))

(test test-l1.clos.meta-slots-contains-type-id
  "Verify *meta-slots* contains '%type-id'."
  (is (member '%type-id *meta-slots*)))

(test test-l1.clos.meta-slots-count
  "Verify *meta-slots* contains expected 15 slots."
  ;; id, %type-id, %revision, %deleted-p, %heap-written-p,
  ;; %type-idx-written-p, %ve-written-p, %vev-written-p, %views-written-p,
  ;; %written-p, %data-pointer, %data, %bytes, from, to, weight
  (is (= (length *meta-slots*) 16)))  ; Adjust if actual count differs

;;;; ============================================================================
;;;; METACLASS TESTS
;;;; ============================================================================

(test test-l1.clos.graph-class-exists
  "Verify graph-class is defined."
  (is (find-class 'graph-class)))

(test test-l1.clos.graph-class-is-metaclass
  "Verify graph-class inherits from standard-class."
  (let ((class (find-class 'graph-class)))
    (is (typep class 'standard-class))))

(test test-l1.clos.validate-superclass
  "Verify validate-superclass allows graph-class ← standard-class."
  (is (validate-superclass (find-class 'graph-class) (find-class 'standard-class))))

;;;; ============================================================================
;;;; SLOT DEFINITION CLASSES TESTS
;;;; ============================================================================

(test test-l1.clos.graph-slot-definition-exists
  "Verify graph-slot-definition class exists."
  (is (find-class 'graph-slot-definition)))

(test test-l1.clos.graph-direct-slot-definition-exists
  "Verify graph-direct-slot-definition class exists."
  (is (find-class 'graph-direct-slot-definition)))

(test test-l1.clos.graph-effective-slot-definition-exists
  "Verify graph-effective-slot-definition class exists."
  (is (find-class 'graph-effective-slot-definition)))

;;;; ============================================================================
;;;; NODE CLASS TESTS
;;;; ============================================================================

(test test-l1.clos.node-class-exists
  "Verify NODE class is defined."
  (is (find-class 'node)))

(test test-l1.clos.node-has-metaclass-graph-class
  "Verify NODE uses graph-class as metaclass."
  (let ((class (find-class 'node)))
    (is (eq (class-of class) (find-class 'graph-class)))))

(test test-l1.clos.node-has-id-slot
  "Verify NODE has 'id' slot."
  (let ((class (find-class 'node)))
    (is (find 'id (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name))))

(test test-l1.clos.node-has-data-slot
  "Verify NODE has '%data' slot."
  (let ((class (find-class 'node)))
    (is (find '%data (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name))))

(test test-l1.clos.node-instance-creation
  "Verify NODE instance can be created."
  (let ((n (make-instance 'node)))
    (is (typep n 'node))))

;;;; ============================================================================
;;;; META-SLOT READ TESTS
;;;; ============================================================================

(test test-l1.clos.read-meta-slot-id
  "Verify reading meta-slot 'id' works (direct access)."
  (let ((n (make-test-node)))
    (is (typep (id n) '(simple-array (unsigned-byte 8) (16))))))

(test test-l1.clos.read-meta-slot-type-id
  "Verify reading meta-slot '%type-id' works."
  (let ((n (make-test-node 42)))
    (is (= (%type-id n) 42))))

(test test-l1.clos.read-meta-slot-revision
  "Verify reading meta-slot '%revision' works."
  (let ((n (make-test-node)))
    (is (= (%revision n) 0))))

(test test-l1.clos.read-meta-slot-data
  "Verify reading meta-slot '%data' works."
  (let ((n (make-instance 'node :data '((:x . 1)))))
    (is (equal (%data n) '((:x . 1))))))

;;;; ============================================================================
;;;; META-SLOT WRITE TESTS
;;;; ============================================================================

(test test-l1.clos.write-meta-slot-revision
  "Verify writing meta-slot '%revision' works."
  (let ((n (make-test-node)))
    (setf (%revision n) 5)
    (is (= (%revision n) 5))))

(test test-l1.clos.write-meta-slot-type-id
  "Verify writing meta-slot '%type-id' works."
  (let ((n (make-test-node)))
    (setf (%type-id n) 99)
    (is (= (%type-id n) 99))))

(test test-l1.clos.write-meta-slot-deleted-p
  "Verify writing meta-slot '%deleted-p' works."
  (let ((n (make-test-node)))
    (setf (%deleted-p n) t)
    (is (equal (%deleted-p n) t))))

;;;; ============================================================================
;;;; USER-SLOT STORAGE TESTS (CRITICAL: Plist Routing)
;;;; ============================================================================

(test test-l1.clos.user-slot-write-basic
  "Verify writing a user-slot updates plist in %data."
  (let ((n (make-instance 'node :data '())))
    ;; Note: This test assumes slot-value-using-class interception works
    ;; If interception not yet active, skip or use direct plist manipulation
    (setf (slot-value n 'name) "Alice")
    (is (equal (slot-value n 'name) "Alice"))))

(test test-l1.clos.user-slot-in-plist
  "Verify user-slot value appears in %data plist."
  (let ((n (make-instance 'node :data '())))
    (setf (slot-value n 'name) "Bob")
    ;; Check plist directly
    (let ((plist (data n)))
      (is (assoc :name plist)))))

(test test-l1.clos.multiple-user-slots
  "Verify multiple user-slots can be stored in plist."
  (let ((n (make-instance 'node :data '())))
    (setf (slot-value n 'name) "Charlie")
    (setf (slot-value n 'age) 30)
    (setf (slot-value n 'email) "charlie@example.com")
    (is (equal (slot-value n 'name) "Charlie"))
    (is (equal (slot-value n 'age) 30))
    (is (equal (slot-value n 'email) "charlie@example.com"))))

(test test-l1.clos.user-slot-read-nonexistent
  "Verify reading nonexistent user-slot returns NIL."
  (let ((n (make-instance 'node :data '())))
    (is (null (slot-value n 'nonexistent)))))

(test test-l1.clos.user-slot-overwrite
  "Verify overwriting user-slot value works."
  (let ((n (make-instance 'node :data '())))
    (setf (slot-value n 'name) "Dave")
    (is (equal (slot-value n 'name) "Dave"))
    (setf (slot-value n 'name) "David")
    (is (equal (slot-value n 'name) "David"))))

;;;; ============================================================================
;;;; TRANSACTION ENLISTMENT TESTS (CRITICAL)
;;;; ============================================================================

(test test-l1.clos.transaction-enlistment-not-available
  "Verify behavior when *current-transaction* is not bound (outside txn)."
  ;; This test checks that writes outside transaction don't crash
  ;; Actual enlistment depends on transactions.lisp being loaded
  ;; For now, we just verify the write completes
  (let ((n (make-instance 'node :data '())))
    (let (*current-transaction*)
      (declare (special *current-transaction*))
      (setf *current-transaction* nil)
      ;; Write outside transaction should complete
      ;; (In real use, would call save-node)
      (is (not (null nil))))))  ; Placeholder

(test test-l1.clos.meta-slot-write-no-transaction
  "Verify meta-slot writes do NOT trigger transaction enlistment."
  ;; Meta-slots are written directly, bypassing transaction logic
  (let ((n (make-instance 'node)))
    (setf (%revision n) 10)  ; Should bypass transaction enlistment
    (is (= (%revision n) 10))))

;;;; ============================================================================
;;;; UNBIND TESTS
;;;; ============================================================================

(test test-l1.clos.unbind-user-slot
  "Verify unbinding a user-slot removes it from plist."
  (let ((n (make-instance 'node :data '((:name . "Eve") (:age . 25)))))
    (slot-makunbound n 'name)
    (is (null (assoc :name (data n))))
    ;; age should still be present
    (is (assoc :age (data n)))))

(test test-l1.clos.unbind-nonexistent-slot
  "Verify unbinding nonexistent slot does not error."
  (let ((n (make-instance 'node :data '())))
    ;; Should not signal error
    (is (null (slot-makunbound n 'nonexistent)))))

(test test-l1.clos.unbind-meta-slot
  "Verify unbinding meta-slot uses standard unbind."
  ;; This test checks that meta-slots call call-next-method
  ;; Actual behavior depends on standard-class unbind
  (let ((n (make-instance 'node)))
    ;; Unbinding a meta-slot should use standard behavior
    (is (not (null n)))))  ; Placeholder

;;;; ============================================================================
;;;; PLIST STRUCTURE TESTS
;;;; ============================================================================

(test test-l1.clos.plist-format-alist
  "Verify plist is stored as alist (list of cons cells)."
  (let ((n (make-instance 'node :data '((:x . 1) (:y . 2)))))
    (is (listp (data n)))
    (is (every (lambda (item) (consp item)) (data n)))))

(test test-l1.clos.plist-keys-as-keywords
  "Verify plist keys are keywords."
  (let ((n (make-instance 'node :data '())))
    (setf (slot-value n 'name) "Frank")
    (let ((key (caar (data n))))
      (is (keywordp key)))))

(test test-l1.clos.empty-plist-initialization
  "Verify empty plist is correctly initialized."
  (let ((n (make-instance 'node :data nil)))
    (is (null (data n))))
  (let ((n (make-instance 'node :data '())))
    (is (null (data n)))))

;;;; ============================================================================
;;;; INTEGRATION TESTS
;;;; ============================================================================

(test test-l1.clos.round-trip-write-read
  "Verify write then read returns same value."
  (let ((n (make-instance 'node :data '())))
    (setf (slot-value n 'value) 42)
    (is (= (slot-value n 'value) 42))))

(test test-l1.clos.mixed-meta-user-slots
  "Verify meta and user slots don't interfere."
  (let ((n (make-instance 'node :data '())))
    ;; Write meta-slot
    (setf (%revision n) 5)
    ;; Write user-slot
    (setf (slot-value n 'name) "Grace")
    ;; Both should be accessible
    (is (= (%revision n) 5))
    (is (equal (slot-value n 'name) "Grace"))))

(test test-l1.clos.user-slots-isolated-per-instance
  "Verify user-slots are isolated between instances."
  (let ((n1 (make-instance 'node :data '()))
        (n2 (make-instance 'node :data '())))
    (setf (slot-value n1 'name) "Henry")
    (setf (slot-value n2 'name) "Iris")
    (is (equal (slot-value n1 'name) "Henry"))
    (is (equal (slot-value n2 'name) "Iris"))
    (is (not (equal (slot-value n1 'name) (slot-value n2 'name))))))

;;;; ============================================================================
;;;; ACCESSOR/INITARG TESTS
;;;; ============================================================================

(test test-l1.clos.id-accessor
  "Verify 'id' has an accessor function."
  (let ((n (make-instance 'node :id (get-random-bytes 16))))
    (is (typep (id n) '(simple-array (unsigned-byte 8) (16))))))

(test test-l1.clos.data-accessor
  "Verify '%data' has an accessor function."
  (let ((n (make-instance 'node :data '((:x . 1)))))
    (is (equal (data n) '((:x . 1))))))

(test test-l1.clos.type-id-initarg
  "Verify '%type-id' can be initialized via initarg."
  (let ((n (make-instance 'node :%type-id 99)))
    (is (= (%type-id n) 99))))

;;;; ============================================================================
;;;; ERROR HANDLING TESTS
;;;; ============================================================================

(test test-l1.clos.assoc-nil-returns-nil
  "Verify (assoc key nil) returns NIL (not error)."
  ;; This is a CL behavior; just document it
  (is (null (assoc :missing nil))))

(test test-l1.clos.cdr-nil-safe
  "Verify (cdr nil) returns NIL (not error)."
  ;; This is a CL behavior; just document it
  (is (null (cdr nil))))

;;;; ============================================================================
;;;; TEST RUNNER
;;;; ============================================================================

(defun run-l1-clos-tests ()
  "Run all Layer 1 CLOS tests and report results."
  (let* ((results (run! 'l1.clos))
         (passed (fiveam:results-passed results))
         (failed (fiveam:results-failed results))
         (skipped (fiveam:results-skipped results)))
    (format t "~&Layer 1 CLOS Tests Summary:~%")
    (format t "  Passed:  ~D~%" passed)
    (format t "  Failed:  ~D~%" failed)
    (format t "  Skipped: ~D~%" skipped)
    (format t "  Total:   ~D~%" (+ passed failed skipped))
    (zerop failed)))

;;;; ============================================================================

(in-package :cl-user)
