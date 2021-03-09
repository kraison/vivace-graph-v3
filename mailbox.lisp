(in-package :graph-db)

(defstruct (mailbox
             (:conc-name mb-)
             (:constructor %make-mailbox))
  (lock (make-lock))
  (queue (make-queue)))

(defun make-mailbox ()
  #+sbcl (sb-concurrency:make-mailbox)
  #+lispworks (mp:make-mailbox)
  #+ccl (%make-mailbox))

(defun send-message (mailbox message)
  #+sbcl (sb-concurrency:send-message mailbox message)
  #+lispworks(mp:mailbox-send mailbox message)
  #+ccl (with-lock ((mb-lock mailbox))
          (enqueue (mb-queue mailbox) message)))

(defun receive-message (mailbox &key (timeout 1))
  #+sbcl (sb-concurrency:receive-message mailbox :timeout timeout)
  #+lispworks (mp:mailbox-read mailbox nil timeout)
  #+ccl (with-lock ((mb-lock mailbox) :timeout timeout)
          (let ((result nil))
            (handler-case
                (trivial-timeout:with-timeout (timeout)
                  (loop until result do
                       (setq result (dequeue (mb-queue mailbox)))))
              (com.metabang.trivial-timeout:timeout-error (c)
                (declare (ignore c))))
            result)))
