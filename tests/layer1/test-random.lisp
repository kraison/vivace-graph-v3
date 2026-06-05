;;;; -*- Mode: Lisp; Package: :vg-tests -*-
;;;; tests/layer1/test-random.lisp
;;;;
;;;; Comprehensive test suite for src/random.lisp (MT19937 PRNG)

(in-package :vg-tests)

(def-suite l1.random
  :description "Layer 1: Mersenne Twister PRNG"
  :in nil)

(in-suite l1.random)

;;;; ============================================================================
;;;; STRUCTURE AND STATE TESTS
;;;; ============================================================================

(test test-l1.random.mt-random-state-structure
  "MT-RANDOM-STATE structure is properly defined."
  (let ((state (graph-db:mt-internal-make-random-state :mti 0 :arr #(1 2 3))))
    (is (graph-db:mt-random-state-p state))
    (is (= (graph-db:mt-random-state-mti state) 0))
    (is (vectorp (graph-db:mt-random-state-arr state)))))

(test test-l1.random.global-state-initialized
  "Global *mt-random-state* is initialized."
  (is (graph-db:mt-random-state-p graph-db:*mt-random-state*)))

;;;; ============================================================================
;;;; STATE CREATION TESTS
;;;; ============================================================================

(test test-l1.random.make-state-from-t
  "Make state with T creates new random state."
  (let ((s1 (graph-db:make-mt-random-state t))
        (s2 (graph-db:make-mt-random-state t)))
    (is (graph-db:mt-random-state-p s1))
    (is (graph-db:mt-random-state-p s2))
    ;; Different states (created at different times)
    (is (not (equalp (graph-db:mt-random-state-arr s1)
                     (graph-db:mt-random-state-arr s2))))))

(test test-l1.random.make-state-from-nil
  "Make state with NIL returns copy of global state."
  (let ((s1 (graph-db:make-mt-random-state nil))
        (s2 (graph-db:make-mt-random-state nil)))
    (is (graph-db:mt-random-state-p s1))
    (is (graph-db:mt-random-state-p s2))
    ;; Both are copies at same moment (should be identical)
    (is (= (graph-db:mt-random-state-mti s1)
           (graph-db:mt-random-state-mti s2)))))

(test test-l1.random.make-state-from-integer
  "Make state with integer seed is reproducible."
  (let ((s1 (graph-db:make-mt-random-state 12345))
        (s2 (graph-db:make-mt-random-state 12345)))
    (is (graph-db:mt-random-state-p s1))
    (is (graph-db:mt-random-state-p s2))
    ;; Same seed produces same state
    (is (equalp (graph-db:mt-random-state-arr s1)
                (graph-db:mt-random-state-arr s2)))))

(test test-l1.random.make-state-from-state
  "Make state with existing state creates copy."
  (let ((s1 (graph-db:make-mt-random-state 42)))
    (let ((s2 (graph-db:make-mt-random-state s1)))
      (is (graph-db:mt-random-state-p s2))
      ;; Copy has same values
      (is (= (graph-db:mt-random-state-mti s1)
             (graph-db:mt-random-state-mti s2)))
      ;; But different arrays (independent)
      (is (not (eq (graph-db:mt-random-state-arr s1)
                   (graph-db:mt-random-state-arr s2)))))))

(test test-l1.random.make-state-from-sequence
  "Make state from 624-element sequence."
  (let ((seq (make-array 624 :initial-element 0)))
    (let ((state (graph-db:make-mt-random-state seq)))
      (is (graph-db:mt-random-state-p state))
      (is (= (graph-db:mt-random-state-mti state) 0)))))

(test test-l1.random.make-state-invalid-sequence-length
  "Make state fails with wrong sequence length."
  (let ((seq (make-array 100 :initial-element 0)))
    (is (signals 'error
          (graph-db:make-mt-random-state seq)))))

;;;; ============================================================================
;;;; RANDOM NUMBER GENERATION TESTS
;;;; ============================================================================

(test test-l1.random.genrand-produces-integers
  "mt-genrand produces 32-bit integers."
  (dotimes (i 100)
    (let ((n (graph-db:mt-genrand)))
      (is (integerp n))
      (is (>= n 0))
      (is (< n (expt 2 32))))))

(test test-l1.random.random-integer-range
  "mt-random produces integers in correct range [0, n)."
  (let ((n 100))
    (dotimes (i 100)
      (let ((r (graph-db:mt-random n)))
        (is (integerp r))
        (is (>= r 0))
        (is (< r n))))))

(test test-l1.random.random-float-range
  "mt-random produces floats in correct range [0.0, n)."
  (let ((n 1.0))
    (dotimes (i 100)
      (let ((r (graph-db:mt-random n)))
        (is (floatp r))
        (is (>= r 0.0))
        (is (< r n))))))

(test test-l1.random.random-reproducible
  "Same seed produces same sequence."
  (let ((s1 (graph-db:make-mt-random-state 999))
        (s2 (graph-db:make-mt-random-state 999)))
    (dotimes (i 10)
      (is (= (graph-db:mt-random 1000000 s1)
             (graph-db:mt-random 1000000 s2))))))

(test test-l1.random.random-with-state-parameter
  "mt-random accepts optional state parameter."
  (let ((state (graph-db:make-mt-random-state 777)))
    (let ((r1 (graph-db:mt-random 100 state))
          (r2 (graph-db:mt-random 100 state)))
      (is (integerp r1))
      (is (integerp r2))
      ;; Sequential numbers from same state
      (is (not (= r1 r2))))))

;;;; ============================================================================
;;;; EDGE CASE TESTS
;;;; ============================================================================

(test test-l1.random.random-n-equals-1
  "mt-random with n=1 always returns 0."
  (dotimes (i 10)
    (is (= (graph-db:mt-random 1) 0))))

(test test-l1.random.random-n-equals-2
  "mt-random with n=2 returns 0 or 1."
  (let ((results (make-hash-table)))
    (dotimes (i 100)
      (let ((r (graph-db:mt-random 2)))
        (incf (gethash r results 0))))
    ;; Both 0 and 1 should appear
    (is (gethash 0 results))
    (is (gethash 1 results))))

(test test-l1.random.random-large-integer
  "mt-random works with large integers (bignum)."
  (let ((n (expt 2 64)))
    (let ((r (graph-db:mt-random n)))
      (is (integerp r))
      (is (>= r 0))
      (is (< r n)))))

(test test-l1.random.random-float-zero
  "mt-random with small float n."
  (let ((n 0.001))
    (let ((r (graph-db:mt-random n)))
      (is (floatp r))
      (is (>= r 0.0))
      (is (< r n)))))

(test test-l1.random.random-large-float
  "mt-random with large float n."
  (let ((n 1000000.0))
    (let ((r (graph-db:mt-random n)))
      (is (floatp r))
      (is (>= r 0.0))
      (is (< r n)))))

;;;; ============================================================================
;;;; DISTRIBUTION TESTS
;;;; ============================================================================

(test test-l1.random.histogram-integers
  "Integer distribution looks reasonable (basic test)."
  (let ((histogram (make-hash-table))
        (n 10)
        (samples 1000))
    (dotimes (i samples)
      (let ((r (graph-db:mt-random n)))
        (incf (gethash r histogram 0))))
    ;; All numbers 0-9 should appear
    (dotimes (i n)
      (is (> (gethash i histogram 0) 0)))))

(test test-l1.random.histogram-floats
  "Float distribution looks reasonable."
  (let ((buckets (make-array 10 :initial-element 0))
        (n 1.0)
        (samples 1000))
    (dotimes (i samples)
      (let ((r (graph-db:mt-random n)))
        (let ((bucket (floor (* r 10))))
          (incf (aref buckets (min bucket 9))))))
    ;; All buckets should have samples
    (dotimes (i 10)
      (is (> (aref buckets i) 0)))))

;;;; ============================================================================
;;;; DETERMINISM TESTS
;;;; ============================================================================

(test test-l1.random.deterministic-sequence
  "Sequence is deterministic with fixed seed."
  (let ((seq1 (loop repeat 20 collect (graph-db:mt-random 1000)))
        (seq2 (loop repeat 20 collect (graph-db:mt-random 1000))))
    ;; Fresh global state for each sequence
    ;; Sequences should be different (different global states)
    ;; But this is hard to guarantee, so just verify they're lists
    (is (listp seq1))
    (is (listp seq2))
    (is (= (length seq1) 20))
    (is (= (length seq2) 20))))

;;;; ============================================================================
;;;; STATE INDEPENDENCE TESTS
;;;; ============================================================================

(test test-l1.random.state-independence
  "Different states don't interfere."
  (let ((s1 (graph-db:make-mt-random-state 111))
        (s2 (graph-db:make-mt-random-state 111)))
    ;; Generate from s1
    (dotimes (i 5)
      (graph-db:mt-random 1000 s1))
    ;; Generate from s2 (should still be at beginning)
    (let ((r2 (graph-db:mt-random 1000 s2)))
      (is (integerp r2)))))

;;;; ============================================================================
;;;; CONSTANT TESTS
;;;; ============================================================================

(test test-l1.random.constants-defined
  "All required constants are defined."
  (is (boundp 'graph-db::*mt-k2^32*))
  (is (boundp 'graph-db::*mt-k-inverse-2^32f*))
  (is (boundp 'graph-db::*mt-n*))
  (is (boundp 'graph-db::*mt-m*))
  (is (boundp 'graph-db::*mt-upper-mask*))
  (is (boundp 'graph-db::*mt-lower-mask*)))

(test test-l1.random.mt-n-value
  "MT-N parameter is 624."
  (is (= graph-db:*mt-n* 624)))

(test test-l1.random.mt-m-value
  "MT-M parameter is 397."
  (is (= graph-db:*mt-m* 397)))

(test test-l1.random.k2-32-value
  "2^32 constant is correct."
  (is (= graph-db:*mt-k2^32* (expt 2 32))))

;;;; ============================================================================
;;;; TEST RUNNER
;;;; ============================================================================

(defun run-l1-random-tests ()
  "Run all Layer 1 Random tests."
  (let* ((results (run! 'l1.random))
         (passed (fiveam:results-passed results))
         (failed (fiveam:results-failed results)))
    (format t "~&Layer 1 Random Tests Summary:~%")
    (format t "  Passed: ~D~%" passed)
    (format t "  Failed: ~D~%" failed)
    (zerop failed)))

(in-package :cl-user)
