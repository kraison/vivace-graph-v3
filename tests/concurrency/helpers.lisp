;;;; Concurrency test helpers: barrier synchronization and thread runner.

(in-package #:graph-db/concurrency-test)

;;; ---------------------------------------------------------------------------
;;; run-threads
;;;
;;; Spawns N threads, holds them at a barrier until all are ready, then
;;; releases them simultaneously for maximum contention.  Joins all within
;;; TIMEOUT seconds (default 30); errors if any thread deadlocks or signals an
;;; unhandled condition.
;;;
;;; FN is called as (funcall fn thread-index) where thread-index is 0-based.
;;;
;;; *GRAPH* is captured at the call site and re-bound in every child thread so
;;; graph operations work regardless of how the Lisp implementation handles
;;; dynamic-variable inheritance across threads.
;;; ---------------------------------------------------------------------------

(defun run-threads (n fn &key (timeout 30))
  ;; bt:make-semaphore has been redefined by graph-db's utilities.lisp to take
  ;; no arguments (it wraps sb-thread:make-semaphore / ccl:make-semaphore with
  ;; no keyword args).  All semaphores start at count 0, which is what we want.
  (let* ((captured-graph *graph*)
         (arrival  (bt:make-semaphore))
         (go-gate  (bt:make-semaphore))
         (done     (bt:make-semaphore))
         (errors   (make-array n :initial-element nil)))
    (dotimes (i n)
      (let ((idx i))
        (bt:make-thread
         (lambda ()
           (let ((*graph* captured-graph))
             (unwind-protect
                  (handler-case
                      (progn
                        (bt:signal-semaphore arrival)
                        (bt:wait-on-semaphore go-gate)
                        (funcall fn idx))
                    (error (e)
                      (setf (aref errors idx) e)))
               (bt:signal-semaphore done))))
         :name (format nil "conc-test-~D" i))))
    ;; Wait for all threads to reach the barrier (5 s startup budget).
    (dotimes (_ n)
      (unless (bt:wait-on-semaphore arrival :timeout 5)
        (error "run-threads: thread did not start within 5 s")))
    ;; Release all at once (signal-semaphore accepts :count).
    (bt:signal-semaphore go-gate :count n)
    ;; Wait for completion within the overall timeout budget.
    (let* ((itps  (float internal-time-units-per-second 1.0d0))
           (start (get-internal-real-time))
           (budget (* timeout itps)))
      (dotimes (_ n)
        (let* ((elapsed   (- (get-internal-real-time) start))
               (remaining (/ (max 0 (- budget elapsed)) itps)))
          (unless (bt:wait-on-semaphore done :timeout remaining)
            (error "run-threads: timed out after ~As (deadlock?)" timeout)))))
    ;; Re-raise the first child error in the calling thread.
    (dotimes (i n)
      (when (aref errors i)
        (error "Thread ~D signaled: ~A" i (aref errors i))))))
