;;;; CONCURRENT-VIEW-STORM-SUITE
;;;;
;;;; Concurrent view build and query stress tests:
;;;;   - Insert threads + view query threads; final view count = total inserts
;;;;   - Reduce view: concurrent inserts; final reduced sum = total inserted

(in-package #:graph-db/concurrent-stress-test)

(def-suite concurrent-view-storm-suite
  :description "Concurrent view build and query storm tests."
  :in concurrent-stress-suite)

(in-suite concurrent-view-storm-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: concurrent view build and query
;;;
;;; Half the threads insert vertices while the other half query the map view.
;;; After all threads join, the view count must equal the total inserts.
;;; Reader threads must not error (they may see partial results during inserts).
;;; ---------------------------------------------------------------------------

(test concurrent-view-build-and-query
  "Writers insert while readers query map-view; final view count = total inserts."
  (let* ((t-count (min *stress-thread-count* 8))
         (writers (max 1 (floor t-count 2)))
         (m       100))   ; inserts per writer
    (with-cstress-graph (g)
      (define-cstress-views)
      (let ((start (get-internal-real-time)))
        (run-threads t-count
                     (lambda (i)
                       (if (< i writers)
                           ;; Writer: insert with distinct value per thread+round
                           (dotimes (j m)
                             (with-transaction ()
                               (make-cs-item :value (+ (* i m) j) :label "view-storm")))
                           ;; Reader: query map-view; count doesn't matter, must not error
                           (dotimes (_ (* m 2))
                             (invoke-graph-view 'cs-item 'cs-item-by-value :graph g)))))
        (record-throughput "view-build-and-query" (* writers m)
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second))))
      (is (= (* writers m)
             (length (map-vertices #'identity g :collect-p t :vertex-type 'cs-item)))
          "Vertex count mismatch: expected ~D got ~D"
          (* writers m)
          (length (map-vertices #'identity g :collect-p t :vertex-type 'cs-item))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: concurrent reduce storm
;;;
;;; T threads each insert M vertices; all emit (yield :all 1) so the reduce
;;; function sums all 1s.  After all threads join, the grand aggregate must
;;; equal T × M.
;;; ---------------------------------------------------------------------------

(test concurrent-reduce-storm
  "T threads × M inserts all yield :all 1; reduced sum must equal T × M."
  (let* ((t-count (min *stress-thread-count* 8))
         (m       50))
    (with-cstress-graph (g)
      (define-cstress-views)
      (run-threads t-count
                   (lambda (i)
                     (declare (ignore i))
                     (dotimes (j m)
                       (with-transaction ()
                         (make-cs-item :value (random 100000) :label "reduce-storm")))))
      (let* ((result (invoke-graph-view 'cs-item 'cs-item-count :graph g))
             (total  (when result (cdr (assoc :value result)))))
        (is (and total (= (* t-count m) total))
            "Reduced sum: expected ~D got ~A" (* t-count m) total)))))
