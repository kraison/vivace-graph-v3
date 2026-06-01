;;;; CONCURRENT-TRANSACTION-SUITE
;;;;
;;;; Tests the ACID transaction layer under concurrent load:
;;;;   - no lost insertions
;;;;   - no lost updates (the classic lost-update / read-modify-write test)
;;;;   - readers never error during concurrent writes
;;;;   - retry-to-exclusive-lock fallback still commits all updates

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-transaction-suite
  :description "Transaction-layer correctness under concurrent load."
  :in concurrency-suite)

(in-suite concurrent-transaction-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: no inserted vertex is lost
;;; ---------------------------------------------------------------------------

(test concurrent-inserts-complete
  "N threads × M inserts: all N×M vertices must be present after all joins."
  (let* ((n   *thread-count*)
         (m   25))
    (with-conc-graph (g)
      (run-threads n
                   (lambda (i)
                     (declare (ignore i))
                     (dotimes (_ m)
                       (with-transaction ()
                         (make-c-item :value (random 10000))))))
      (is (= (* n m)
             (length (map-vertices #'identity g
                                   :collect-p t
                                   :vertex-type 'c-item)))
          "Expected ~D vertices; found ~D" (* n m)
          (length (map-vertices #'identity g
                                :collect-p t
                                :vertex-type 'c-item))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: no lost updates (read-modify-write under contention)
;;;
;;; N threads each increment a shared counter exactly once inside
;;; with-transaction.  Conflicting transactions retry automatically.  The
;;; final value must equal N — any shortfall is a lost update.
;;; ---------------------------------------------------------------------------

(test lost-update-prevention
  "N threads each increment a shared counter once; final value must equal N."
  (with-conc-graph (g)
    (let (counter-id)
      (with-transaction ()
        (setq counter-id (id (make-c-item :value 0))))
      (run-threads *thread-count*
                   (lambda (i)
                     (declare (ignore i))
                     (with-transaction ()
                       (let* ((item (copy (lookup-vertex counter-id)))
                              (old  (slot-value item 'value)))
                         (setf (slot-value item 'value) (1+ old))
                         (save item)))))
      (is (= *thread-count*
             (slot-value (lookup-vertex counter-id) 'value))
          "Expected counter=~D (no lost updates); got ~D"
          *thread-count*
          (slot-value (lookup-vertex counter-id) 'value)))))

;;; ---------------------------------------------------------------------------
;;; Test 3: concurrent readers never error during writes
;;; ---------------------------------------------------------------------------

(test concurrent-readers-never-error
  "Reader threads calling map-vertices must not error while writers insert."
  (with-conc-graph (g)
    (let* ((n       *thread-count*)
           (writes  10)
           (reads   50))
      ;; Pre-populate so readers have something to scan from the start.
      (with-transaction ()
        (dotimes (i 5) (make-c-item :value i)))
      ;; N writers + N readers run concurrently.
      (run-threads (* 2 n)
                   (lambda (i)
                     (if (< i n)
                         ;; Writer
                         (dotimes (_ writes)
                           (with-transaction ()
                             (make-c-item :value (random 1000))))
                         ;; Reader — just must not error
                         (dotimes (_ reads)
                           (map-vertices #'identity g
                                         :collect-p nil
                                         :vertex-type 'c-item)))))
      ;; Reaching here without error is the pass condition.
      (pass))))

;;; ---------------------------------------------------------------------------
;;; Test 4: retry-to-exclusive-lock fallback still commits every update
;;;
;;; By reducing *maximum-transaction-attempts* to 2 we force the exclusive-lock
;;; fallback to fire quickly under contention, and verify it still produces the
;;; correct final value.
;;; ---------------------------------------------------------------------------

(test transaction-retry-fallback-commits-all
  "With a very low retry limit the exclusive-lock fallback must commit all N updates."
  (with-conc-graph (g)
    (let ((*maximum-transaction-attempts* 2)
          counter-id)
      (with-transaction ()
        (setq counter-id (id (make-c-item :value 0))))
      (run-threads *thread-count*
                   (lambda (i)
                     (declare (ignore i))
                     (with-transaction ()
                       (let* ((item (copy (lookup-vertex counter-id)))
                              (old  (slot-value item 'value)))
                         (setf (slot-value item 'value) (1+ old))
                         (save item)))))
      (is (= *thread-count*
             (slot-value (lookup-vertex counter-id) 'value))
          "With retry-fallback: expected ~D, got ~D"
          *thread-count*
          (slot-value (lookup-vertex counter-id) 'value)))))
