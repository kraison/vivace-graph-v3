;;;; TRANSACTION-STRESS-SUITE
;;;;
;;;; Scale tests for the transaction layer:
;;;;   - high-conflict sequential: 500 txns each updating the same vertex
;;;;   - retry-exhaustion behaviour: forced exclusive-lock fallback
;;;;   - large write-set: single transaction touching 1K vertices

(in-package #:graph-db/stress-test)

(def-suite transaction-stress-suite
  :description "Scale tests for the transaction layer."
  :in stress-suite)

(in-suite transaction-stress-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: high-conflict sequential
;;;
;;; N sequential transactions each read-modify-write the same "hot" vertex.
;;; No concurrency; every transaction should succeed on first attempt.
;;; The final counter value must equal N.
;;; ---------------------------------------------------------------------------

(test high-conflict-sequential
  "N sequential txns each increment the same vertex; final value must equal N."
  (let ((n (scale 500 50)))
    (with-stress-graph (g)
      (let (counter-id)
        (with-transaction ()
          (setq counter-id (id (make-s-item :value 0 :label "counter"))))
        (let ((start (get-internal-real-time)))
          (dotimes (_ n)
            (with-transaction ()
              (let* ((item (copy (lookup-vertex counter-id)))
                     (old  (slot-value item 'value)))
                (setf (slot-value item 'value) (1+ old))
                (save item))))
          (record-throughput "high-conflict-sequential" n
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second))))
        (is (= n (slot-value (lookup-vertex counter-id) 'value))
            "Expected counter=~D after ~D sequential increments; got ~D"
            n n (slot-value (lookup-vertex counter-id) 'value))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: retry-exhaustion behaviour
;;;
;;; Setting *maximum-transaction-attempts* to 2 forces the exclusive-lock
;;; fallback to fire on nearly every update under contention.  With a single
;;; thread, real contention never occurs, but we verify that the low retry
;;; limit does not prevent commits: all N increments must succeed.
;;; ---------------------------------------------------------------------------

(test retry-exhaustion-behaviour
  "With *maximum-transaction-attempts* = 2 all N increments must still commit."
  (let ((n (scale 200 20)))
    (with-stress-graph (g)
      (let ((*maximum-transaction-attempts* 2)
            counter-id)
        (with-transaction ()
          (setq counter-id (id (make-s-item :value 0 :label "retry-counter"))))
        (dotimes (_ n)
          (with-transaction ()
            (let* ((item (copy (lookup-vertex counter-id)))
                   (old  (slot-value item 'value)))
              (setf (slot-value item 'value) (1+ old))
              (save item))))
        (is (= n (slot-value (lookup-vertex counter-id) 'value))
            "Expected ~D with low retry limit; got ~D"
            n (slot-value (lookup-vertex counter-id) 'value))))))

;;; ---------------------------------------------------------------------------
;;; Test 3: large write-set
;;;
;;; A single transaction creates N vertices, then a second transaction reads
;;; all of them and increments their values.  The large write-set exercises
;;; the transaction validation loop over many objects.
;;; ---------------------------------------------------------------------------

(test large-write-set
  "One transaction touching N vertices must commit; all changes visible after."
  (let ((n (scale 1000 100)))
    (with-stress-graph (g)
      (let (ids)
        ;; Create N vertices in one transaction.
        (with-transaction ()
          (setq ids
                (loop for i below n
                      collect (id (make-s-item :value i :label "batch")))))
        ;; One transaction updates all N.
        (let ((start (get-internal-real-time)))
          (with-transaction ()
            (dolist (vid ids)
              (let* ((item (copy (lookup-vertex vid)))
                     (old  (slot-value item 'value)))
                (setf (slot-value item 'value) (+ old 1000))
                (save item))))
          (record-throughput "large-write-set" n
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second))))
        ;; Verify all were updated.
        (is (loop for i from 0 for vid in ids
                  always (= (+ i 1000) (slot-value (lookup-vertex vid) 'value)))
            "Some vertices were not updated by the large-write-set transaction")))))
