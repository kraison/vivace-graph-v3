;;;; CONCURRENT-VIEW-SUITE
;;;;
;;;; Views are maintained incrementally inside with-transaction, protected by
;;;; their own rw-lock group.  These tests verify that concurrent inserts all
;;;; land in the view index and that concurrent readers never observe errors.

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-view-suite
  :description "View index correctness and stability under concurrent load."
  :in concurrency-suite)

(in-suite concurrent-view-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: every concurrently-inserted item appears in the view
;;; ---------------------------------------------------------------------------

(test concurrent-view-updates
  "N threads × M inserts: all N×M entries must appear in the view index."
  (let* ((n  *thread-count*)
         (m  25))
    (with-conc-graph (g)
      (define-concurrency-views)
      ;; Each thread inserts M items with a unique value (thread * m + j) so
      ;; all entries are distinct and we can count them precisely.
      (run-threads n
                   (lambda (i)
                     (dotimes (j m)
                       (with-transaction ()
                         (make-c-item :value (+ (* i m) j))))))
      (let ((view-count 0))
        (map-view (lambda (id key value)
                    (declare (ignore id key value))
                    (incf view-count))
                  'c-item 'c-item-by-value :graph g)
        (is (= (* n m) view-count)
            "Expected ~D view entries; found ~D" (* n m) view-count)))))

;;; ---------------------------------------------------------------------------
;;; Test 2: readers calling invoke-graph-view must not error during writes
;;; ---------------------------------------------------------------------------

(test concurrent-view-reads-during-writes
  "Readers calling invoke-graph-view must not error while writers insert."
  (let* ((n       *thread-count*)
         (writes  20)
         (reads   40))
    (with-conc-graph (g)
      (define-concurrency-views)
      ;; Pre-seed a few items so the view is non-empty for early readers.
      (with-transaction ()
        (dotimes (i 5)
          (make-c-item :value (* i 1000))))
      (run-threads (* 2 n)
                   (lambda (i)
                     (if (< i n)
                         ;; Writer
                         (dotimes (j writes)
                           (with-transaction ()
                             (make-c-item :value (+ (* i writes) j 100000))))
                         ;; Reader — must not error
                         (dotimes (_ reads)
                           (invoke-graph-view 'c-item 'c-item-by-value :graph g)))))
      (pass))))
