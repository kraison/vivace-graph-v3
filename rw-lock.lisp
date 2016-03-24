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
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (readers 0 :type integer)
  (semaphore (sb-thread:make-semaphore) :type sb-thread:semaphore)
  (writer-queue (make-empty-queue) :type queue)
  (writer nil)
  (waitqueue (sb-thread:make-waitqueue) :type sb-thread:waitqueue))

(defun next-in-queue-p (rw-lock thread)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (and (not (empty-queue-p (lock-writer-queue rw-lock)))
	 (eq thread (queue-front (lock-writer-queue rw-lock))))))

(defun lock-unused-p (rw-lock)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (and (= 0 (lock-readers rw-lock))
	 (= 0 (sb-thread:semaphore-count (lock-semaphore rw-lock)))
	 (null (lock-writer rw-lock))
	 (empty-queue-p (lock-writer-queue rw-lock)))))

(defun release-read-lock (rw-lock)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (assert (not (eql 0 (lock-readers rw-lock))))
    (when (eql 0 (decf (lock-readers rw-lock)))
      ;;(log:debug "~A RELEASED READ LOCK ~A" (current-thread) rw-lock)
      (when (lock-writer rw-lock)
        ;;(log:debug "~A SIGNALLED WRITER ON LOCK ~A" (current-thread) rw-lock)
	(sb-thread:signal-semaphore (lock-semaphore rw-lock))))))

(defun acquire-read-lock (rw-lock &key (max-tries 1000))
  (declare (ignore max-tries))
  ;;(loop for tries from 0 to max-tries do
  (loop
     (sb-thread:with-recursive-lock ((lock-lock rw-lock))
       (if (lock-writer rw-lock)
           (condition-wait (lock-waitqueue rw-lock) (lock-lock rw-lock))
           (progn
             (incf (lock-readers rw-lock))
             ;;(log:debug "~A GOT READ LOCK ~A" (current-thread) rw-lock)
             (return-from acquire-read-lock rw-lock))))))

(defmacro with-read-lock ((rw-lock) &body body)
  `(unwind-protect
	(if (rw-lock-p (acquire-read-lock ,rw-lock))
	    (progn ,@body)
	    (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-read-lock ,rw-lock)))

(defun release-write-lock (rw-lock &key reading-p)
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (if (next-in-queue-p rw-lock sb-thread:*current-thread*)
        (dequeue (lock-writer-queue rw-lock))
	(error "Cannot release lock I don't own!"))
    (if (next-in-queue-p rw-lock sb-thread:*current-thread*)
	;;(format t "Not releasing lock;  recursive ownership detected!~%")
	nil
	(progn
	  (setf (lock-writer rw-lock) nil)
          ;;(log:debug "~A RELEASED WRITE LOCK ~A" (current-thread) rw-lock)
	  (when reading-p
	    (incf (lock-readers rw-lock))
            ;;(log:debug "~A GOT READ LOCK ~A" (current-thread) rw-lock)
            )
	  (sb-thread:condition-broadcast (lock-waitqueue rw-lock))))))

(defun acquire-write-lock (rw-lock &key (max-tries 1000) reading-p)
  (declare (ignore max-tries))
  (sb-thread:with-recursive-lock ((lock-lock rw-lock))
    (if (and (next-in-queue-p rw-lock sb-thread:*current-thread*)
	     (eq (lock-writer rw-lock) sb-thread:*current-thread*))
	(progn
	  (enqueue-front (lock-writer-queue rw-lock)
                         sb-thread:*current-thread*)
          ;;(log:debug "~A GOT WRITE LOCK ~A" (current-thread) rw-lock)
	  (return-from acquire-write-lock rw-lock))
	(enqueue (lock-writer-queue rw-lock) sb-thread:*current-thread*)))
  ;;(loop for tries from 0 to max-tries do
  (loop
       (if (eq (lock-writer rw-lock) sb-thread:*current-thread*)
           (progn
             ;;(log:debug "~A GOT WRITE LOCK ~A" (current-thread) rw-lock)
             (return-from acquire-write-lock rw-lock))
	   (let ((wait-p nil))
	     (handler-case
		 (sb-thread:with-recursive-lock ((lock-lock rw-lock))
		   (if (and (null (lock-writer rw-lock))
			    (next-in-queue-p rw-lock
                                            sb-thread:*current-thread*))
		       (progn
			 (setf (lock-writer rw-lock)
                               sb-thread:*current-thread*)
			 (when reading-p
			   (decf (lock-readers rw-lock))
                           ;;(log:debug "~A RELEASED READ LOCK ~A" (current-thread) rw-lock)
                           )
			 (unless (eql 0 (lock-readers rw-lock))
			   (setf wait-p t)))
		       (sb-thread:condition-wait
			(lock-waitqueue rw-lock) (lock-lock rw-lock))))
	       (error (c)
		 (log:error "Got error ~A while acquiring write lock ~A"
                            c rw-lock)))
	     (when wait-p
	       (sb-thread:wait-on-semaphore (lock-semaphore rw-lock)))))))

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
