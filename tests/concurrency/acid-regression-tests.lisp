;;;; CONCURRENT-ACID-REGRESSION-SUITE
;;;;
;;;; Regression guards for known ACID correctness issues in the transaction
;;;; engine.  These tests are kept separate so they evolve in lockstep with
;;;; targeted bug fixes rather than being mixed into the general suite.

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-acid-regression-suite
  :description "Regression tests for ACID correctness issues in the transaction engine."
  :in concurrency-suite)

(in-suite concurrent-acid-regression-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: multi-vertex lost-update regression
;;;
;;; The original sustained-conflict-storm test revealed that with exactly 2
;;; threads per vertex (low per-vertex contention), ~16% of increments were
;;; silently lost.  With low contention the exclusive-lock fallback rarely
;;; fires, exposing a subtle race in the MVCC validation path.
;;;
;;; This test replicates the original failing pattern as a permanent regression
;;; guard: 8 threads, 5 hot vertices, 2 threads per vertex, 20 increments each.
;;; Each vertex must end at exactly 40 (2 threads × 20 increments).
;;; ---------------------------------------------------------------------------

(test multi-vertex-lost-update-regression
  "10 threads × 5 vertices, 2 threads per vertex, 20 increments each;
each vertex final value must equal 40 with no lost updates."
  (let* ((n-threads  10)
         (n-vertices 5)
         (k          20))    ; increments per thread → 2 threads × 20 = 40 per vertex
    (with-conc-graph (g)
      (let ((counter-ids (make-array n-vertices)))
        (dotimes (v n-vertices)
          (with-transaction ()
            (setf (aref counter-ids v) (id (make-c-item :value 0)))))
        ;; Thread i always increments vertex (floor i 2):
        ;;   threads 0-1 → vertex 0, threads 2-3 → vertex 1, …, threads 8-9 → vertex 4.
        ;; This gives exactly 2-thread contention per vertex — the low-contention
        ;; path that bypasses the exclusive-lock fallback and exposed the lost-update.
        (run-threads n-threads
                     (lambda (i)
                       (let ((vid (aref counter-ids (floor i 2))))
                         (dotimes (_ k)
                           (with-transaction ()
                             (let* ((item (copy (lookup-vertex vid)))
                                    (old  (slot-value item 'value)))
                               (setf (slot-value item 'value) (1+ old))
                               (save item)))))))
        (dotimes (v n-vertices)
          (let ((actual   (slot-value (lookup-vertex (aref counter-ids v)) 'value))
                (expected (* 2 k)))
            (is (= expected actual)
                "Vertex ~D: expected ~D (2 threads × ~D increments); got ~D"
                v expected k actual)))))))
