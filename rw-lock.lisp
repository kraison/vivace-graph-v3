(in-package :graph-db)

(defun print-rw-lock (lock stream depth)
  (declare (ignore depth))
  (print-unreadable-object (lock stream :type t :identity t)
    (format stream "W: ~A, R: ~A"
            (lock-writer lock) (lock-readers lock))))

(defstruct (rw-lock
	     (:conc-name lock-)
	     (:print-function print-rw-lock)
	     (:predicate rw-lock-p))
  #+sbcl(lock (sb-thread:make-mutex) :type sb-thread:mutex)
  #+lispworks(lock (mp:make-lock) :type mp:lock)
  #+ecl(lock (mp:make-lock))
  (readers 0 :type integer)
  #+sbcl (semaphore (sb-thread:make-semaphore) :type sb-thread:semaphore)
  #+lispworks (semaphore (mp:make-semaphore) :type mp:semaphore)
  #+ecl (semaphore (mp:make-semaphore))
  (writer-queue (make-empty-queue) :type queue)
  (writer nil)
  #+lispworks(waitqueue (mp:make-condition-variable) :type mp:condition-variable)
  #+sbcl(waitqueue (sb-thread:make-waitqueue) :type sb-thread:waitqueue)
  #+ecl(waitqueue (mp:make-condition-variable)))

(defun %next-in-queue-p (rw-lock thread)
  ;; Callers must already hold (lock-lock rw-lock).
  (and (not (empty-queue-p (lock-writer-queue rw-lock)))
       (eq thread (queue-front (lock-writer-queue rw-lock)))))

(defun %lock-unused-p (rw-lock)
  ;; Callers must already hold (lock-lock rw-lock).
  (and (= 0 (lock-readers rw-lock))
       (= #+sbcl 0 #+lispworks 1 #+ecl 0
          (#+sbcl sb-thread:semaphore-count
           #+lispworks mp:semaphore-count
           #+ecl mp:semaphore-count
           (lock-semaphore rw-lock)))
       (null (lock-writer rw-lock))
       (empty-queue-p (lock-writer-queue rw-lock))))

(defun next-in-queue-p (rw-lock thread)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (%next-in-queue-p rw-lock thread)))

(defun lock-unused-p (rw-lock)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (%lock-unused-p rw-lock)))

(defun release-read-lock (rw-lock)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (assert (not (eql 0 (lock-readers rw-lock))))
    (when (eql 0 (decf (lock-readers rw-lock)))
      ;;(log:debug "~A RELEASED READ LOCK ~A" (current-thread) rw-lock)
      (when (lock-writer rw-lock)
        ;;(log:debug "~A SIGNALLED WRITER ON LOCK ~A" (current-thread) rw-lock)
        ;; Wake the waiting writer via its semaphore.  Older ECL (no
        ;; :graph-db-ecl-modern-mp) can't (mp:wait-on-semaphore blocked
        ;; indefinitely on 21.2.1); there the writer polls lock-readers instead.
        #+sbcl (sb-thread:signal-semaphore (lock-semaphore rw-lock))
        #+lispworks (mp:semaphore-release (lock-semaphore rw-lock))
        #+graph-db-ecl-modern-mp (mp:signal-semaphore (lock-semaphore rw-lock))))))

(defun acquire-read-lock (rw-lock &key (max-tries 1000))
  (declare (ignore max-tries))
  ;;(loop for tries from 0 to max-tries do
  ;; Older ECL (no :graph-db-ecl-modern-mp) cannot block on the waitqueue
  ;; reliably (condition-variable-broadcast missed waiters / timedwait unreliable
  ;; before 23.09.09), so it spin-sleeps outside the lock instead.  Modern ECL,
  ;; SBCL and LispWorks block on the condition variable.
  (loop
     (with-recursive-lock-held ((lock-lock rw-lock))
       (cond ((null (lock-writer rw-lock))
              (incf (lock-readers rw-lock))
              ;;(log:debug "~A GOT READ LOCK ~A" (current-thread) rw-lock)
              (return-from acquire-read-lock rw-lock))
             #+sbcl
             (t (sb-thread:condition-wait
                 (lock-waitqueue rw-lock) (lock-lock rw-lock)))
             #+lispworks
             (t (mp:condition-variable-wait
                 (lock-waitqueue rw-lock) (lock-lock rw-lock)))
             #+graph-db-ecl-modern-mp
             (t (mp:condition-variable-wait
                 (lock-waitqueue rw-lock) (lock-lock rw-lock)))))
     ;; Older ECL only: writer active; sleep briefly outside the lock and retry.
     #+(and ecl (not graph-db-ecl-modern-mp)) (sleep 0.001)))

(defmacro with-read-lock ((rw-lock) &body body)
  `(unwind-protect
	(if (rw-lock-p (acquire-read-lock ,rw-lock))
	    (progn ,@body)
	    (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-read-lock ,rw-lock)))

(defun release-write-lock (rw-lock &key reading-p)
  (with-recursive-lock-held ((lock-lock rw-lock))
    (if (%next-in-queue-p rw-lock (current-thread))
        (dequeue (lock-writer-queue rw-lock))
	(error "Cannot release lock I don't own!"))
    (if (%next-in-queue-p rw-lock (current-thread))
	;;(format t "Not releasing lock;  recursive ownership detected!~%")
	nil
	(progn
	  (setf (lock-writer rw-lock) nil)
          ;;(log:debug "~A RELEASED WRITE LOCK ~A" (current-thread) rw-lock)
	  (when reading-p
	    (incf (lock-readers rw-lock))
            ;;(log:debug "~A GOT READ LOCK ~A" (current-thread) rw-lock)
            )
          #+lispworks (mp:condition-variable-broadcast (lock-waitqueue rw-lock))
	  #+sbcl (sb-thread:condition-broadcast (lock-waitqueue rw-lock))
          #+graph-db-ecl-modern-mp
          (mp:condition-variable-broadcast (lock-waitqueue rw-lock))
          ;; Older ECL uses sleep-based polling; no condition waiters to notify.
          ))))

(defun acquire-write-lock (rw-lock &key (max-tries 1000) reading-p (wait-p t))
  (declare (ignore max-tries))
  (with-recursive-lock-held ((lock-lock rw-lock))
    (cond ((and (%next-in-queue-p rw-lock (current-thread))
                (eq (lock-writer rw-lock) (current-thread)))
           (enqueue-front (lock-writer-queue rw-lock)
                          (current-thread))
           ;;(log:debug "~A GOT WRITE LOCK ~A" (current-thread) rw-lock)
           (return-from acquire-write-lock rw-lock))
          (wait-p
           (enqueue (lock-writer-queue rw-lock) (current-thread)))
          (t
           (if (%lock-unused-p rw-lock)
               (progn
                 (enqueue (lock-writer-queue rw-lock)
                          (current-thread))
                 (setf (lock-writer rw-lock)
                       (current-thread))
                 (when reading-p
                   (decf (lock-readers rw-lock)))
                 (return-from acquire-write-lock rw-lock))
               (return-from acquire-write-lock nil)))))
  ;;(loop for tries from 0 to max-tries do
  (loop
     (if (eq (lock-writer rw-lock) (current-thread))
         (progn
           ;;(log:debug "~A GOT WRITE LOCK ~A" (current-thread) rw-lock)
           (return-from acquire-write-lock rw-lock))
         (let ((internal-wait-p nil))
           (handler-case
               (with-recursive-lock-held ((lock-lock rw-lock))
                 (if (and (null (lock-writer rw-lock))
                          (%next-in-queue-p rw-lock
                                            (current-thread)))
                     (progn
                       (setf (lock-writer rw-lock)
                             (current-thread))
                       (when reading-p
                         (decf (lock-readers rw-lock))
                         ;;(log:debug "~A RELEASED READ LOCK ~A" (current-thread) rw-lock)
                         )
                       (unless (eql 0 (lock-readers rw-lock))
                         (setf internal-wait-p t)))
                     #-(or sbcl ecl)
                     (condition-wait (lock-waitqueue rw-lock) (lock-lock rw-lock))
                     #+sbcl
                     (sb-thread:condition-wait
                      (lock-waitqueue rw-lock) (lock-lock rw-lock))
                     #+graph-db-ecl-modern-mp
                     (mp:condition-variable-wait
                      (lock-waitqueue rw-lock) (lock-lock rw-lock))))
             (error (c)
               (log:error "Got error ~A while acquiring write lock ~A"
                          c rw-lock)))
           (when internal-wait-p
             ;; We are the writer; block until the remaining readers drain.
             #+lispworks (mp:semaphore-acquire (lock-semaphore rw-lock))
             #+sbcl (sb-thread:wait-on-semaphore (lock-semaphore rw-lock))
             #+graph-db-ecl-modern-mp (mp:wait-on-semaphore (lock-semaphore rw-lock))
             ;; Older ECL: mp:wait-on-semaphore blocked indefinitely on 21.2.1
             ;; under high thread counts, so poll lock-readers instead.
             #+(and ecl (not graph-db-ecl-modern-mp))
             (loop
               (with-recursive-lock-held ((lock-lock rw-lock))
                 (when (= 0 (lock-readers rw-lock))
                   (return)))
               (sleep 0.001)))
           ;; Older ECL: we condition-wait above only on modern ECL; otherwise,
           ;; if we didn't become the writer this iteration, sleep briefly
           ;; outside the lock before retrying (avoids spinning at 100% CPU).
           #+(and ecl (not graph-db-ecl-modern-mp))
           (unless internal-wait-p
             (sleep 0.001))))))

(defmacro with-write-lock ((rw-lock &key reading-p) &body body)
  `(unwind-protect
	(if (rw-lock-p (acquire-write-lock ,rw-lock :reading-p ,reading-p))
	    (progn ,@body)
	    (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-write-lock ,rw-lock :reading-p ,reading-p)))

#|
(defun test-rw-locks ()
  (let ((lock (make-rw-lock)))
    (make-thread
     #'(lambda () (with-write-lock (lock)
		    (format t "1 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (with-write-lock (lock)
		      (format t "1 acquired recursive lock.~%")
		      (sleep 5)
		      (with-write-lock (lock)
			(format t "1 acquired recursive lock.~%")
			(sleep 5)
			(format t "1 releasing recursive write lock.~%"))
		      (format t "1 releasing recursive write lock.~%"))
		    (format t "1 releasing write lock.~%"))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "2 got read lock~%") (sleep 5))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "3 got read lock~%") (sleep 5))))
    (make-thread
     #'(lambda () (with-write-lock (lock)
		    (format t "4 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (with-write-lock (lock)
		      (format t "4 acquired recursive lock.~%")
		      (sleep 5)
		      (with-write-lock (lock)
			(format t "4 acquired recursive lock.~%")
			(sleep 5)
			(format t "4 releasing recursive write lock.~%"))
		      (format t "4 releasing recursive write lock.~%"))
		    (format t "4 releasing write lock.~%"))))
    (make-thread
     #'(lambda () (with-write-lock (lock)
		    (format t "5 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (format t "5 releasing write lock.~%"))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "6 got read lock~%") (sleep 5))))
    (make-thread
     #'(lambda () (with-read-lock (lock) (format t "7 got read lock~%") (sleep 5))))))
|#
