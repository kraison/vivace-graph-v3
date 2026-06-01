;;;; RW-LOCK-SUITE
;;;;
;;;; Tests the graph-db read-write lock directly:
;;;;   SBCL / ECL → pure-Lisp rw-lock.lisp implementation
;;;;   CCL        → ccl:make-read-write-lock via utilities.lisp shims
;;;;
;;;; These tests need no on-disk graph; they exercise the lock primitives alone.

(in-package #:graph-db/concurrency-test)

(def-suite rw-lock-suite
  :description "Read-write lock correctness and deadlock-freedom."
  :in concurrency-suite)

(in-suite rw-lock-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: multiple threads can hold a read lock simultaneously
;;; ---------------------------------------------------------------------------

(test rw-lock-multiple-concurrent-readers
  "N threads should be able to hold the read lock at the same time."
  (let ((lock        (make-rw-lock))
        (count-lock  (bt:make-lock "count"))
        (inside      0)
        (max-inside  0))
    (run-threads *thread-count*
                 (lambda (i)
                   (declare (ignore i))
                   (with-read-lock (lock)
                     (let ((n (bt:with-lock-held (count-lock)
                                (incf inside)
                                inside)))
                       ;; Hold lock briefly so threads overlap.
                       (sleep 0.02)
                       (bt:with-lock-held (count-lock)
                         (setf max-inside (max max-inside n))
                         (decf inside))))))
    ;; If reads are truly shared, >1 thread was inside simultaneously.
    (is (> max-inside 1)
        "Expected >1 concurrent reader; got max ~A (read lock may be exclusive)"
        max-inside)))

;;; ---------------------------------------------------------------------------
;;; Test 2: a writer cannot enter while any reader holds the lock
;;; ---------------------------------------------------------------------------

(test rw-lock-writer-excludes-readers
  "No reader should see in-writer=T while a writer holds the lock."
  (let ((lock       (make-rw-lock))
        (flag-lock  (bt:make-lock "flag"))
        (in-writer  nil)
        (violation  nil))
    ;; 1 writer + (n-1) reader threads, each doing many short ops.
    (run-threads *thread-count*
                 (lambda (i)
                   (dotimes (_ 40)
                     (if (zerop i)
                         ;; Writer: mark entry/exit under flag-lock so readers
                         ;; can check it while holding the read lock.
                         (with-write-lock (lock)
                           (bt:with-lock-held (flag-lock)
                             (setf in-writer t))
                           (bt:thread-yield)
                           (bt:with-lock-held (flag-lock)
                             (setf in-writer nil)))
                         ;; Reader: if in-writer is T we have a violation.
                         (with-read-lock (lock)
                           (bt:with-lock-held (flag-lock)
                             (when in-writer
                               (setf violation t)))
                           (bt:thread-yield))))))
    (is-false violation "A reader observed in-writer=T — mutual exclusion failure")))

;;; ---------------------------------------------------------------------------
;;; Test 3: only one writer at a time
;;; ---------------------------------------------------------------------------

(test rw-lock-no-concurrent-writers
  "Two writers must never overlap."
  (let ((lock       (make-rw-lock))
        (flag-lock  (bt:make-lock "flag"))
        (in-writer  nil)
        (violation  nil))
    (run-threads *thread-count*
                 (lambda (i)
                   (declare (ignore i))
                   (dotimes (_ 40)
                     (with-write-lock (lock)
                       (bt:with-lock-held (flag-lock)
                         (when in-writer
                           (setf violation t))
                         (setf in-writer t))
                       (bt:thread-yield)
                       (bt:with-lock-held (flag-lock)
                         (setf in-writer nil))))))
    (is-false violation "Two writers held the lock concurrently")))

;;; ---------------------------------------------------------------------------
;;; Test 4: no deadlock under sustained mixed read/write load
;;; ---------------------------------------------------------------------------

(test rw-lock-no-deadlock-under-load
  "N readers and N/2 writers all terminate within the timeout."
  (let* ((lock     (make-rw-lock))
         (n-read   *thread-count*)
         (n-write  (max 2 (floor *thread-count* 2)))
         (n-total  (+ n-read n-write)))
    ;; If anything deadlocks, run-threads' timeout fires and signals an error.
    (run-threads n-total
                 (lambda (i)
                   (if (< i n-read)
                       (dotimes (_ 60)
                         (with-read-lock (lock)
                           (bt:thread-yield)))
                       (dotimes (_ 60)
                         (with-write-lock (lock)
                           (bt:thread-yield))))))
    (pass)))
