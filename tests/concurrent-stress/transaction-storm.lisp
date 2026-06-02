;;;; CONCURRENT-TRANSACTION-STORM-SUITE
;;;;
;;;; Sustained concurrent transaction load:
;;;;   - All threads hammer a single counter vertex; verify every increment lands
;;;;   - Mixed read-write: writers + readers run for many rounds; verify counts

(in-package #:graph-db/concurrent-stress-test)

(def-suite concurrent-transaction-storm-suite
  :description "Sustained concurrent transaction load."
  :in concurrent-stress-suite)

(in-suite concurrent-transaction-storm-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: sustained conflict storm
;;;
;;; T threads each increment the SAME counter vertex K times.  Every thread
;;; always conflicts with every other thread, so the retry + exclusive-lock
;;; fallback path is exercised continuously.  Final counter value must equal
;;; T × K with no missing increments.
;;;
;;; This is the same pattern as transaction-conflict-storm in the concurrency
;;; suite, scaled up to stress-test volume.
;;; ---------------------------------------------------------------------------

(test sustained-conflict-storm
  "T threads × K increments on one shared counter; final value = T×K."
  (let* ((t-count (min *stress-thread-count* 8))
         (k       20))   ; increments per thread; 8×20 = 160 total
    (with-cstress-graph (g)
      (let (counter-id)
        (with-transaction ()
          (setq counter-id (id (make-cs-item :value 0 :label "counter"))))
        (let ((start (get-internal-real-time)))
          (run-threads t-count
                       (lambda (i)
                         (declare (ignore i))
                         (dotimes (_ k)
                           (with-transaction ()
                             (let* ((item (copy (lookup-vertex counter-id)))
                                    (old  (slot-value item 'value)))
                               (setf (slot-value item 'value) (1+ old))
                               (save item))))))
          (record-throughput "sustained-conflict-storm" (* t-count k)
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second))))
        (is (= (* t-count k)
               (slot-value (lookup-vertex counter-id) 'value))
            "Expected counter=~D (~D threads × ~D increments); got ~D"
            (* t-count k) t-count k
            (slot-value (lookup-vertex counter-id) 'value))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: mixed read-write storm
;;;
;;; Half the threads insert vertices in a loop (W × M inserts each).
;;; The other half scan map-vertices repeatedly (must not error).
;;; After all threads finish: total vertex count = writer-threads × M.
;;; ---------------------------------------------------------------------------

(test mixed-read-write-storm
  "Half writers, half readers; writer count verified; readers must not error."
  (let* ((t-count  (min *stress-thread-count* 8))
         (writers  (max 1 (floor t-count 2)))
         (m        100))   ; inserts per writer
    (with-cstress-graph (g)
      (let ((start (get-internal-real-time)))
        (run-threads t-count
                     (lambda (i)
                       (if (< i writers)
                           ;; Writer
                           (dotimes (_ m)
                             (with-transaction ()
                               (make-cs-item :value i :label "rw-item")))
                           ;; Reader — must not error
                           (dotimes (_ (* m 2))
                             (map-vertices #'identity g
                                           :collect-p nil
                                           :vertex-type 'cs-item)))))
        (record-throughput "mixed-read-write-storm" (* writers m)
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second))))
      (is (= (* writers m)
             (length (map-vertices #'identity g :collect-p t :vertex-type 'cs-item)))
          "Expected ~D vertices from writers; got ~D"
          (* writers m)
          (length (map-vertices #'identity g :collect-p t :vertex-type 'cs-item))))))
