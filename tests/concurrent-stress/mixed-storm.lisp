;;;; CONCURRENT-MIXED-STORM-SUITE
;;;;
;;;; Full-system storm: 4 roles run simultaneously over many rounds.
;;;; This is the ultimate integration test for all subsystems under concurrent
;;;; load: inserters, deleters, Prolog query threads, and view readers.

(in-package #:graph-db/concurrent-stress-test)

(def-suite concurrent-mixed-storm-suite
  :description "Full-system storm: 4 roles × many rounds."
  :in concurrent-stress-suite)

(in-suite concurrent-mixed-storm-suite)

;;; ---------------------------------------------------------------------------
;;; Test: full-system storm
;;;
;;; T threads split into 4 roles (role = thread-index mod 4):
;;;   0 — inserter:     insert K vertices per round
;;;   1 — deleter:      scan and mark-deleted the first available vertex
;;;   2 — Prolog query: select-flat (is-a ?x cs-item) — count varies; must not error
;;;   3 — view reader:  invoke-graph-view on cs-item-by-value — must not error
;;;
;;; Each thread runs R rounds.  The test passes if all threads complete
;;; without errors or deadlock.  The only hard invariant checked at the end is
;;; that the vertex count is non-negative (sanity check).
;;;
;;; Per-thread latency is recorded when *collect-timings* is true.
;;; ---------------------------------------------------------------------------

(test full-system-storm
  "4-role storm (inserters/deleters/query/view readers) × R rounds; no errors."
  (let* (;; ECL 21.2.1: bt:wait-on-semaphore :timeout uses mp:interrupt-process
         ;; which is unreliable under high thread counts; 16 threads causes the
         ;; run-threads timeout to never fire.  All other tests cap at 8; do
         ;; the same here.  Still exercises all 4 roles with 2 threads each.
         (t-count (min *stress-thread-count* #+ecl 8 #-ecl 16))
         (k       5)   ; insertions per inserter round
         (r       30))  ; rounds per thread
    (with-cstress-graph (g)
      (define-cstress-views)
      ;; Pre-populate so deleters and readers have work from the start.
      (with-transaction ()
        (dotimes (i (* t-count k 2))
          (make-cs-item :value i :label "pre")))
      (let ((latencies (make-latency-table t-count))
            (start     (get-internal-real-time)))
        (run-threads t-count
                     (lambda (i)
                       (let ((thread-start (get-internal-real-time)))
                         (dotimes (_ r)
                           (case (mod i 4)
                             (0  ; inserter
                              (dotimes (_ k)
                                (with-transaction ()
                                  (make-cs-item :value (random 1000000) :label "storm"))))
                             (1  ; deleter
                              (let ((vs (map-vertices #'identity g
                                                      :collect-p t
                                                      :vertex-type 'cs-item)))
                                (when vs
                                  (with-transaction ()
                                    (mark-deleted (first vs))))))
                             (2  ; Prolog query — result count may vary; must not error
                              (select-flat (?x) (is-a ?x cs-item)))
                             (3  ; view reader
                              (invoke-graph-view 'cs-item 'cs-item-by-value :graph g))))
                         (setf (aref latencies i)
                               (/ (- (get-internal-real-time) thread-start)
                                  (float internal-time-units-per-second))))))
        (record-throughput "full-system-storm" (* t-count r)
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second)))
        (record-thread-latencies "full-system-storm-thread" latencies))
      ;; Live vertex count must be non-negative; no stricter invariant since
      ;; deleters remove an unpredictable number of vertices.
      (is (>= (length (map-vertices #'identity g
                                    :collect-p t :vertex-type 'cs-item))
              0)
          "Vertex count was negative — impossible")
      ;; Reaching here without error is the primary pass condition.
      (pass))))
