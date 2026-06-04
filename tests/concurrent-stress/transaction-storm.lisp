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

;;; ---------------------------------------------------------------------------
;;; Test 3: concurrent-insert visibility (regression)
;;;
;;; Regression for a lost-update where committed pure inserts became invisible
;;; to type-indexed scans.  apply-tx-write set the node's WRITTEN-P flag only in
;;; finalize-node, AFTER lhash-insert, so the node was briefly in the table with
;;; written-p=0 on disk; a concurrent reader could deserialize and cache that
;;; stale copy, overwriting the committed one, and map-vertices then skipped it
;;; (its written-p guard failed).  With concurrent type-indexed readers and
;;; enough writers, a handful of the W*M committed inserts went missing (e.g.
;;; 3195/3200) even though the vertices were durably present in the lhash.
;;;
;;; Repeated over several iterations to make the guard reliable.  Pre-fix
;;; per-graph miss rate on this box was ~10% at 32 threads but ~48% at 64, so
;;; SBCL runs at 64 (~98% catch over 6 iterations) — SBCL has no observed
;;; concurrency ceiling.  CCL/ECL honor *stress-thread-count* (the documented
;;; interim ceiling, default 16): above it their commit/type-index convoy can
;;; blow the run-threads deadlock timeout (a separate, tracked perf issue), which
;;; would make THIS correctness test flaky there.  The fix under test is in
;;; shared code, so the SBCL run is the primary guard; CCL/ECL still exercise it
;;; within their supported concurrency.
;;; ---------------------------------------------------------------------------

(test concurrent-insert-visibility
  "Every committed insert is visible via the type index, even with concurrent
type-indexed readers (regression: written-p must be set before lhash-insert)."
  (let* ((t-count #+sbcl 64 #-sbcl (min 24 *stress-thread-count*))
         (writers (floor t-count 2))
         (m       100)
         (iters   6))
    (dotimes (iter iters)
      (with-cstress-graph (g)
        (run-threads t-count
                     (lambda (i)
                       (if (< i writers)
                           (dotimes (_ m)
                             (with-transaction ()
                               (make-cs-item :value i :label "vis")))
                           ;; Readers traverse the type index concurrently --
                           ;; this is what poisoned the cache before the fix.
                           (dotimes (_ (* m 2))
                             (map-vertices #'identity g
                                           :collect-p nil
                                           :vertex-type 'cs-item)))))
        (let ((visible (length (map-vertices #'identity g :collect-p t
                                             :vertex-type 'cs-item))))
          (is (= (* writers m) visible)
              "iter ~D: expected ~D type-indexed vertices; got ~D (lost ~D)"
              iter (* writers m) visible (- (* writers m) visible)))))))
