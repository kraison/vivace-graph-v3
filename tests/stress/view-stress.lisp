;;;; VIEW-STRESS-SUITE
;;;;
;;;; Scale tests for the view layer:
;;;;   - map-view over a large dataset (unique keys, exact value lookup)
;;;;   - map-reduce view: sum aggregation over a large dataset
;;;;   - view consistency after mass deletion

(in-package #:graph-db/stress-test)

(def-suite view-stress-suite
  :description "Scale tests for the view layer."
  :in stress-suite)

(in-suite view-stress-suite)

;;; ---------------------------------------------------------------------------
;;; Local view registration helpers
;;;
;;; Views must be defined inside each with-stress-graph body because they are
;;; stored per-graph instance.  We define them as a helper called at the start
;;; of each test that needs views.
;;; ---------------------------------------------------------------------------

(defun define-stress-views ()
  "Register stress views on *GRAPH*.  Call once inside with-stress-graph."
  (def-view s-item-by-value :lessp (s-item :graph-db-stress-test)
    (:map (lambda (item)
            (yield (slot-value item 'value) t))))
  (def-view s-item-count :lessp (s-item :graph-db-stress-test)
    (:map    (lambda (item)
               (declare (ignore item))
               (yield :all 1)))
    (:reduce (lambda (keys values)
               (declare (ignore keys))
               (apply #'+ values)))))

;;; ---------------------------------------------------------------------------
;;; Test 1: map-view over a large dataset
;;;
;;; Insert N s-item vertices each with a distinct integer value 0..N-1.
;;; Define a map view that yields the value as the key.  Verify that for
;;; every integer i in 0..N-1, invoke-graph-view with :key i returns exactly
;;; one result.
;;; ---------------------------------------------------------------------------

(test map-view-large-dataset
  "map-view over N items with distinct keys: every key must return 1 result."
  (let ((n (scale 2000 200)))
    (with-stress-graph (g)
      (define-stress-views)
      (with-transaction ()
        (dotimes (i n)
          (make-s-item :value i :label "view-item")))
      (let ((start (get-internal-real-time))
            (misses 0))
        (dotimes (i n)
          (let ((results (invoke-graph-view 's-item 's-item-by-value
                                            :key i :graph g)))
            (unless (= 1 (length results))
              (incf misses))))
        (record-throughput "map-view-lookup" n
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second)))
        (is (zerop misses)
            "~D out of ~D keys returned wrong result count" misses n)))))

;;; ---------------------------------------------------------------------------
;;; Test 2: reduce-view large aggregate
;;;
;;; Insert N s-item vertices, all yielding key :all with value 1.  The reduce
;;; function sums the 1s, so the grand total must equal N.
;;; ---------------------------------------------------------------------------

(test reduce-view-large-aggregate
  "Reduce (sum) over N items must equal N."
  (let ((n (scale 1000 100)))
    (with-stress-graph (g)
      (define-stress-views)
      (with-transaction ()
        (dotimes (i n)
          (make-s-item :value i :label "reduce-item")))
      ;; Grand-aggregate invoke-graph-view returns an alist with :value key.
      (let* ((result (invoke-graph-view 's-item 's-item-count :graph g))
             (total  (when result (cdr (assoc :value result)))))
        (is (and total (= n total))
            "Reduced total: expected ~D got ~A (full result: ~S)" n total result)))))

;;; ---------------------------------------------------------------------------
;;; Test 3: view consistency after mass deletion
;;;
;;; Insert 2N items, delete N of them (alternating), then verify that:
;;;   - map-view returns exactly N results (no stale deleted-node entries)
;;;   - every returned vertex is non-deleted (live)
;;; ---------------------------------------------------------------------------

(test view-consistency-after-deletes
  "After deleting half of 2N items, view must return exactly N live entries."
  (let ((n (scale 500 50)))
    (with-stress-graph (g)
      (define-stress-views)
      (let ((all-ids (make-array (* 2 n))))
        (with-transaction ()
          (dotimes (i (* 2 n))
            (setf (aref all-ids i)
                  (id (make-s-item :value i :label "del-item")))))
        ;; Delete every other vertex.
        (with-transaction ()
          (dotimes (i n)
            (mark-deleted (lookup-vertex (aref all-ids (* 2 i))))))
        (let* ((start   (get-internal-real-time))
               (results (map-view (lambda (key id value)
                                    (declare (ignore key value))
                                    id)
                                  's-item 's-item-by-value
                                  :graph g :collect-p t))
               (elapsed (/ (- (get-internal-real-time) start)
                           (float internal-time-units-per-second))))
          (record-throughput "view-after-deletes" (length results) elapsed)
          (is (= n (length results))
              "Expected ~D live view entries after deletions; got ~D" n (length results))
          (is (every (lambda (vid)
                       (let ((v (lookup-vertex vid)))
                         (and v (not (deleted-p v)))))
                     results)
              "Some view entries point to deleted or missing vertices"))))))
