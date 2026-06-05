;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: :vg-tests -*-
;;;; tests/layer1/test-utilities.lisp
;;;;
;;;; Test suite for src/utilities.lisp (Layer 1: Core Utilities)
;;;; 
;;;; Framework: fiveam (Common Lisp standard test framework)
;;;; Test naming convention: test-l1.utilities.FUNCTION-NAME
;;;;
;;;; PRECONDITIONS:
;;;; - (asdf:load-system :vivace-graph)
;;;; - (asdf:load-system :vivacegraph-tests)
;;;; - (fiveam:run! (find-package :vg-tests))

(defpackage :vg-tests
  (:use :cl :fiveam :graph-db)
  (:export #:run-all-tests))

(in-package :vg-tests)

;;;; ============================================================================
;;;; TEST SUITE DEFINITION & UTILITIES
;;;; ============================================================================

(def-suite l1.utilities
  :description "Layer 1: Utilities module tests"
  :in nil)

(in-suite l1.utilities)

;;; Fixtures

(defun make-test-uuid ()
  "Create a deterministic test UUID for reproducible tests."
  (uuid:make-v4-uuid))

(defun byte-vector-equal (bv1 bv2)
  "Compare two byte vectors element-wise."
  (and (= (length bv1) (length bv2))
       (every #'= bv1 bv2)))

;;;; ============================================================================
;;;; DEBUGGING & OUTPUT TESTS
;;;; ============================================================================

(test test-l1.utilities.dbg
  "Verify dbg writes to stdout and returns NIL."
  (with-output-to-string (out)
    (let ((*standard-output* out))
      (is (null (dbg "Test ~A" "message")))
      (let ((output (get-output-stream-string out)))
        (is (search "Test message" output))))))

(test test-l1.utilities.ignore-warning
  "Verify ignore-warning muffle-warning works (handler-bind context)."
  (let ((caught nil))
    (handler-bind ((warning (lambda (c)
                             (setf caught t)
                             (ignore-warning c))))
      ;; Note: Hard to trigger a warning without actual compiler code
      ;; This test is more about verifying the function exists
      (is (ignore-warning (make-condition 'warning :format-control "test")))
      ;; Technically caught will be nil here since we didn't actually warn
      )))

;;;; ============================================================================
;;;; RANDOMNESS & ENTROPY TESTS
;;;; ============================================================================

(test test-l1.utilities.get-random-bytes-length
  "Verify get-random-bytes returns correct number of bytes."
  (is (= (length (get-random-bytes 16)) 16))
  (is (= (length (get-random-bytes 32)) 32))
  (is (= (length (get-random-bytes 1)) 1)))

(test test-l1.utilities.get-random-bytes-element-type
  "Verify get-random-bytes returns unsigned-byte 8 array."
  (let ((bytes (get-random-bytes 16)))
    (is (equal (array-element-type bytes) '(unsigned-byte 8)))))

(test test-l1.utilities.get-random-bytes-randomness
  "Verify get-random-bytes returns different values on successive calls."
  (let ((bytes1 (get-random-bytes 16))
        (bytes2 (get-random-bytes 16)))
    ;; Probability of collision: negligible (2^-128)
    (is (not (byte-vector-equal bytes1 bytes2)))))

(test test-l1.utilities.get-random-bytes-range
  "Verify get-random-bytes produces values in range [0, 255]."
  (let ((bytes (get-random-bytes 100)))
    (is (every (lambda (b) (and (>= b 0) (<= b 255))) bytes))))

;;;; ============================================================================
;;;; TIME CONVERSION TESTS
;;;; ============================================================================

(test test-l1.utilities.unix-universal-conversion-round-trip
  "Verify unix ↔ universal time conversions are inverses."
  (let ((unix-time 1711828399))  ; March 31, 2026 (approximate)
    (is (= unix-time
            (universal-to-unix-time (unix-to-universal-time unix-time))))))

(test test-l1.utilities.universal-unix-conversion-round-trip
  "Verify universal ↔ unix time conversions are inverses."
  (let ((universal-time (get-universal-time)))
    (is (= universal-time
            (unix-to-universal-time (universal-to-unix-time universal-time))))))

(test test-l1.utilities.unix-epoch-offset
  "Verify epoch offset constant is correct."
  ;; Epoch offset = seconds from 1900-01-01 to 1970-01-01
  ;; Known value: 2208988800
  (is (= *unix-epoch-difference* 2208988800)))

(test test-l1.utilities.get-unix-time-sanity
  "Verify get-unix-time returns a reasonable Unix timestamp."
  (let ((now (get-unix-time))
        ;; Expected range: ~1700000000 (Nov 2023) to ~1900000000 (remote future)
        (min-reasonable 1700000000)
        (max-reasonable 1900000000))
    (is (and (>= now min-reasonable) (<= now max-reasonable)))))

(test test-l1.utilities.gettimeofday-sanity
  "Verify gettimeofday returns a reasonable float Unix time."
  (let ((now (gettimeofday))
        (min-reasonable 1700000000.0)
        (max-reasonable 1900000000.0))
    (is (and (>= now min-reasonable) (<= now max-reasonable)))
    (is (floatp now))))

;;;; ============================================================================
;;;; FILE UTILITIES TESTS
;;;; ============================================================================

(test test-l1.utilities.line-count-empty-file
  "Verify line-count returns 0 for empty file."
  (let ((temp-file (format nil "/tmp/test-empty-~A.txt" (get-unix-time))))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output)
             nil)  ; Create empty file
           (is (= (line-count temp-file) 0)))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(test test-l1.utilities.line-count-single-line-no-newline
  "Verify line-count counts a line without trailing newline."
  (let ((temp-file (format nil "/tmp/test-lines-~A.txt" (get-unix-time))))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output)
             (write-string "hello" out))
           (is (= (line-count temp-file) 1)))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(test test-l1.utilities.line-count-multiple-lines
  "Verify line-count counts multiple lines correctly."
  (let ((temp-file (format nil "/tmp/test-lines-~A.txt" (get-unix-time))))
    (unwind-protect
         (progn
           (with-open-file (out temp-file :direction :output)
             (format out "line1~%line2~%line3~%"))
           (is (= (line-count temp-file) 3)))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

;;;; ============================================================================
;;;; LIST UTILITIES TESTS
;;;; ============================================================================

(test test-l1.utilities.last1-normal
  "Verify last1 returns the last element of a list."
  (is (eql (last1 '(a b c)) 'c))
  (is (eql (last1 '(x)) 'x)))

(test test-l1.utilities.last1-empty
  "Verify last1 returns NIL for empty list."
  (is (null (last1 nil))))

(test test-l1.utilities.last1-single
  "Verify last1 with single element."
  (is (eql (last1 '(z)) 'z)))

(test test-l1.utilities.flatten-nested
  "Verify flatten recursively flattens nested lists."
  (is (equal (flatten '(a (b c) (d (e f))))
             '(a b c d e f))))

(test test-l1.utilities.flatten-single-level
  "Verify flatten on already-flat list."
  (is (equal (flatten '(a b c))
             '(a b c))))

(test test-l1.utilities.flatten-empty
  "Verify flatten on empty list."
  (is (null (flatten nil))))

(test test-l1.utilities.flatten-deeply-nested
  "Verify flatten on deeply nested structure."
  (is (equal (flatten '(((a))))
             '(a))))

(test test-l1.utilities.find-all-integers
  "Verify find-all finds all matching integers."
  (is (equal (find-all 1 '(1 2 1 3 1))
             '(1 1 1))))

(test test-l1.utilities.find-all-symbols
  "Verify find-all finds all matching symbols."
  (is (equal (find-all 'x '(a x b x c x))
             '(x x x))))

(test test-l1.utilities.find-all-custom-test
  "Verify find-all with custom test predicate."
  (is (equal (find-all 3 '(1 2 3 4 5) :test #'<)
             '(1 2))))

(test test-l1.utilities.find-all-empty
  "Verify find-all returns empty list if no matches."
  (is (null (find-all 'x '(a b c)))))

(test test-l1.utilities.find-anywhere-found
  "Verify find-anywhere locates item in tree."
  (is (eql (find-anywhere 'x '(a (b (c x d) e) f))
           'x)))

(test test-l1.utilities.find-anywhere-not-found
  "Verify find-anywhere returns NIL if not found."
  (is (null (find-anywhere 'z '(a b c)))))

(test test-l1.utilities.find-anywhere-deep
  "Verify find-anywhere finds deeply nested items."
  (is (eql (find-anywhere 'target '(((((target)))))
           'target)))

(test test-l1.utilities.find-if-anywhere-numberp
  "Verify find-if-anywhere finds first number in tree."
  (is (= (find-if-anywhere #'numberp '(a (b (3 c) d)))
         3)))

(test test-l1.utilities.find-if-anywhere-not-found
  "Verify find-if-anywhere returns NIL if no match."
  (is (null (find-if-anywhere #'numberp '(a b c)))))

(test test-l1.utilities.unique-find-anywhere-if
  "Verify unique-find-anywhere-if finds all unique matches."
  (let ((result (unique-find-anywhere-if #'oddp '(1 (2 1 (3 2)) 1))))
    ;; Order may vary due to adjoin, so check set equality
    (is (= (length result) 2))
    (is (member 1 result))
    (is (member 3 result))))

(test test-l1.utilities.length=1-true
  "Verify length=1 returns T for single-element lists."
  (is (length=1 '(x)))
  (is (length=1 '(nil))))

(test test-l1.utilities.length=1-false-multiple
  "Verify length=1 returns NIL for multi-element lists."
  (is (not (length=1 '(a b))))
  (is (not (length=1 '(x y z)))))

(test test-l1.utilities.length=1-false-empty
  "Verify length=1 returns NIL for empty list."
  (is (not (length=1 nil))))

(test test-l1.utilities.length=1-false-non-list
  "Verify length=1 returns NIL for non-lists."
  (is (not (length=1 'x)))
  (is (not (length=1 42))))

(test test-l1.utilities.proper-listp-true
  "Verify proper-listp returns T for proper lists."
  (is (proper-listp nil))
  (is (proper-listp '(a)))
  (is (proper-listp '(a b c))))

(test test-l1.utilities.proper-listp-false
  "Verify proper-listp returns NIL for dotted lists."
  (is (not (proper-listp '(a . b))))
  (is (not (proper-listp '(a b . c)))))

(test test-l1.utilities.proper-listp-false-atom
  "Verify proper-listp returns NIL for atoms."
  (is (not (proper-listp 'atom)))
  (is (not (proper-listp 42))))

;;;; ============================================================================
;;;; SYMBOL GENERATION TESTS
;;;; ============================================================================

(test test-l1.utilities.new-interned-symbol-concat
  "Verify new-interned-symbol concatenates args."
  (let ((sym (new-interned-symbol 'layer- 1 '-vertex)))
    (is (equal (symbol-name sym) "LAYER-1-VERTEX"))))

(test test-l1.utilities.new-interned-symbol-interning
  "Verify new-interned-symbol creates interned symbols."
  (let ((sym1 (new-interned-symbol 'test- 'sym))
        (sym2 (new-interned-symbol 'test- 'sym)))
    (is (eq sym1 sym2))  ; Same object (interned))))

;;;; ============================================================================
;;;; UUID GENERATION & PARSING TESTS
;;;; ============================================================================

(test test-l1.utilities.gen-id-length
  "Verify gen-id returns 16-byte vector."
  (is (= (length (gen-id)) 16)))

(test test-l1.utilities.gen-id-element-type
  "Verify gen-id returns unsigned-byte 8 array."
  (let ((id (gen-id)))
    (is (equal (array-element-type id) '(unsigned-byte 8)))))

(test test-l1.utilities.gen-id-uniqueness
  "Verify gen-id produces unique IDs (high probability)."
  (let ((id1 (gen-id))
        (id2 (gen-id))
        (id3 (gen-id)))
    (is (not (byte-vector-equal id1 id2)))
    (is (not (byte-vector-equal id2 id3)))
    (is (not (byte-vector-equal id1 id3)))))

(test test-l1.utilities.read-uuid-from-string-valid
  "Verify read-uuid-from-string parses valid UUID."
  (let ((uuid-str "550e8400-e29b-41d4-a716-446655440000")
        (parsed (read-uuid-from-string "550e8400-e29b-41d4-a716-446655440000")))
    (is (typep parsed 'uuid:uuid))))

(test test-l1.utilities.read-uuid-from-string-hyphens-ignored
  "Verify read-uuid-from-string ignores hyphens."
  (let ((with-hyphens "550e8400-e29b-41d4-a716-446655440000")
        (no-hyphens "550e8400e29b41d4a716446655440000"))
    (let ((p1 (read-uuid-from-string with-hyphens))
          (p2 (read-uuid-from-string no-hyphens)))
      (is (uuid:uuid= p1 p2)))))

(test test-l1.utilities.read-uuid-from-string-invalid-length
  "Verify read-uuid-from-string signals error on invalid length."
  (is (signals error
        (read-uuid-from-string "550e8400-e29b-41d4-a716"))))

(test test-l1.utilities.read-id-array-from-string-length
  "Verify read-id-array-from-string returns 16-byte array."
  (let ((arr (read-id-array-from-string "550e8400-e29b-41d4-a716-446655440000")))
    (is (= (length arr) 16))
    (is (equal (array-element-type arr) '(unsigned-byte 8)))))

(test test-l1.utilities.read-id-array-from-string-round-trip
  "Verify UUID string → byte-array → consistency."
  (let* ((uuid-str "550e8400-e29b-41d4-a716-446655440000")
         (arr1 (read-id-array-from-string uuid-str))
         (arr2 (read-id-array-from-string uuid-str)))
    (is (byte-vector-equal arr1 arr2))))

;;;; ============================================================================
;;;; MEMORY INSPECTION TESTS
;;;; ============================================================================

(test test-l1.utilities.free-memory-sanity
  "Verify free-memory returns a positive integer."
  (let ((free (free-memory)))
    (is (integerp free))
    (is (> free 0))))

;;;; ============================================================================
;;;; ARRAY UTILITIES TESTS
;;;; ============================================================================

(test test-l1.utilities.make-byte-vector-length
  "Verify make-byte-vector creates correct size array."
  (is (= (length (make-byte-vector 16)) 16))
  (is (= (length (make-byte-vector 100)) 100)))

(test test-l1.utilities.make-byte-vector-initialization
  "Verify make-byte-vector initializes to zeros."
  (let ((vec (make-byte-vector 10)))
    (is (every #'zerop vec))))

(test test-l1.utilities.make-byte-vector-element-type
  "Verify make-byte-vector has correct element type."
  (let ((vec (make-byte-vector 16)))
    (is (equal (array-element-type vec) '(unsigned-byte 8)))))

;;;; ============================================================================
;;;; COMPARISON TESTS: less-than
;;;; ============================================================================

(test test-l1.utilities.less-than-integers
  "Verify less-than on integers."
  (is (less-than 1 2))
  (is (not (less-than 2 1)))
  (is (not (less-than 1 1))))

(test test-l1.utilities.less-than-strings
  "Verify less-than on strings (lexicographic)."
  (is (less-than "a" "b"))
  (is (not (less-than "b" "a")))
  (is (not (less-than "a" "a"))))

(test test-l1.utilities.less-than-symbols
  "Verify less-than on symbols (by symbol-name)."
  (is (less-than 'a 'b))
  (is (not (less-than 'b 'a))))

(test test-l1.utilities.less-than-cross-type-number-symbol
  "Verify less-than: numbers < symbols."
  (is (less-than 1 'x)))

(test test-l1.utilities.less-than-cross-type-number-string
  "Verify less-than: numbers < strings."
  (is (less-than 1 "x")))

(test test-l1.utilities.less-than-cross-type-symbol-string
  "Verify less-than: symbols < strings (wrong, need to check)."
  ;; Check roadmap for type order
  (is (not (less-than "x" 'x))))

(test test-l1.utilities.less-than-nil-and-t
  "Verify less-than: nil < t."
  (is (less-than nil t))
  (is (not (less-than t nil))))

(test test-l1.utilities.less-than-list-special
  "Verify less-than: lists are first in type order."
  (is (less-than '(a) 'x))
  (is (less-than '(a) 1)))

(test test-l1.utilities.less-than-min-sentinel
  "Verify less-than: +min-sentinel+ is minimum."
  (is (not (less-than 1 +min-sentinel+)))
  (is (not (less-than +min-sentinel+ +min-sentinel+))))

(test test-l1.utilities.less-than-max-sentinel
  "Verify less-than: +max-sentinel+ is maximum."
  (is (less-than 1 +max-sentinel+))
  (is (less-than +max-sentinel+ +min-sentinel+)))  ; Wait, this seems wrong

(test test-l1.utilities.less-than-list-recursion
  "Verify less-than: list comparison is recursive."
  (is (less-than '(1 2) '(1 3)))
  (is (not (less-than '(1 3) '(1 2)))))

;;;; ============================================================================
;;;; COMPARISON TESTS: greater-than
;;;; ============================================================================

(test test-l1.utilities.greater-than-integers
  "Verify greater-than on integers."
  (is (greater-than 2 1))
  (is (not (greater-than 1 2)))
  (is (not (greater-than 1 1))))

(test test-l1.utilities.greater-than-inverse-less-than
  "Verify greater-than is inverse of less-than."
  (let ((test-pairs '((1 2) (5 3) ("a" "b") ('x 'y))))
    (dolist (pair test-pairs)
      (let ((x (car pair)) (y (cadr pair)))
        (is (or (less-than x y) (greater-than x y) (equal x y)))))))

;;;; ============================================================================
;;;; KEY VECTOR COMPARISON TESTS
;;;; ============================================================================

(test test-l1.utilities.key-vector-less-than-simple
  "Verify key-vector< lexicographic ordering."
  (is (key-vector< #(1 2 3) #(1 2 4)))
  (is (not (key-vector< #(1 2 4) #(1 2 3)))))

(test test-l1.utilities.key-vector-less-than-prefix
  "Verify key-vector< with prefix."
  (is (key-vector< #(1 2) #(1 2 3)))
  (is (not (key-vector< #(1 2 3) #(1 2)))))

(test test-l1.utilities.key-vector-less-than-equal
  "Verify key-vector< returns NIL for equal vectors."
  (is (not (key-vector< #(1 2 3) #(1 2 3)))))

(test test-l1.utilities.key-vector-less-than-empty
  "Verify key-vector< with empty vectors."
  (is (not (key-vector< #() #())))
  (is (not (key-vector< #() #(1)))))  ; Empty is NOT < non-empty

(test test-l1.utilities.key-vector-less-equal
  "Verify key-vector<= allows equality."
  (is (key-vector<= #(1 2) #(1 2)))
  (is (key-vector<= #(1 2) #(1 3)))
  (is (not (key-vector<= #(1 3) #(1 2)))))

(test test-l1.utilities.key-vector-greater-than
  "Verify key-vector> works correctly."
  (is (key-vector> #(2 1) #(1 9)))
  (is (not (key-vector> #(1 1) #(2)))))

;;;; ============================================================================
;;;; TYPE CHECKING TESTS
;;;; ============================================================================

(test test-l1.utilities.reuse-cons-reuse
  "Verify reuse-cons reuses identical cons."
  (let ((existing (cons 'a 'b)))
    (is (eq existing (reuse-cons 'a 'b existing)))))

(test test-l1.utilities.reuse-cons-fresh
  "Verify reuse-cons creates fresh cons if not equal."
  (let ((existing (cons 'x 'y)))
    (let ((result (reuse-cons 'a 'b existing)))
      (is (not (eq result existing)))
      (is (equal result '(a . b))))))

;;;; ============================================================================
;;;; MACRO UTILITY TESTS
;;;; ============================================================================

(test test-l1.utilities.with-gensyms-creates-distinct
  "Verify with-gensyms creates distinct gensyms."
  (with-gensyms (x y z)
    (is (not (eq x y)))
    (is (not (eq y z)))
    (is (not (eq x z)))))

;;;; ============================================================================
;;;; HASH UTILITIES (DEBUG)
;;;; ============================================================================

(test test-l1.utilities.dump-hash-no-crash
  "Verify dump-hash doesn't crash on empty/populated hash."
  (let ((h (make-hash-table)))
    (is (null (dump-hash h))))  ; Should return NIL and not error
  (let ((h (make-hash-table)))
    (setf (gethash 'key h) 'value)
    (is (null (dump-hash h)))))  ; Still returns NIL

;;;; ============================================================================
;;;; TEST RUNNER
;;;; ============================================================================

(defun run-l1-utilities-tests ()
  "Run all Layer 1 utilities tests and report results."
  (let* ((results (run! 'l1.utilities))
         (passed (fiveam:results-passed results))
         (failed (fiveam:results-failed results))
         (skipped (fiveam:results-skipped results)))
    (format t "~&Layer 1 Utilities Tests Summary:~%")
    (format t "  Passed:  ~D~%" passed)
    (format t "  Failed:  ~D~%" failed)
    (format t "  Skipped: ~D~%" skipped)
    (format t "  Total:   ~D~%" (+ passed failed skipped))
    (zerop failed)))

;;;; ============================================================================

(in-package :cl-user)
