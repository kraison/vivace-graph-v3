;;;; rw-lock-microbench.lisp
;;;;
;;;; Standalone, dependency-free reproduction of graph-db's custom rw-lock
;;;; (rw-lock.lisp) write/read handoff, used to root-cause the CCL/ECL
;;;; concurrent-stress timeouts.  Runs on SBCL using sb-thread only.
;;;;
;;;; It ports the EXACT acquire/release algorithm from rw-lock.lisp and exposes
;;;; a compile switch (*mode*) between:
;;;;   :block  - condition-wait / condition-broadcast  (SBCL/LispWorks/modern ECL)
;;;;   :poll   - (sleep 0.001) busy-wait               (current #+ecl path)
;;;;
;;;; Findings it demonstrates (see docs/concurrency-scaling-investigation.md):
;;;;   * Throughput collapses 14-18x from 4 -> 64 threads in BOTH modes, because
;;;;     every reader and writer serializes on the single internal mutex, and
;;;;     every write-release does a condition-broadcast thundering-herd.
;;;;   * The timeout is slow progress, not deadlock: sustained ~138% CPU on
;;;;     4 cores, and every run completes.
;;;;
;;;; Usage:
;;;;   sbcl --non-interactive --load docs/rw-lock-microbench.lisp
;;;;   sbcl --non-interactive --load docs/rw-lock-microbench.lisp readmostly
;;;; While running, sample CPU in another shell:
;;;;   watch -n1 'ps -o %cpu,nlwp -C sbcl'

(declaim (optimize (speed 2) (safety 1)))

;;; --- minimal FIFO queue (stands in for graph-db's queue) -----------------
(defstruct q (head nil) (tail nil))
(defun q-empty-p (q) (null (q-head q)))
(defun q-front (q) (car (q-head q)))
(defun q-enq (q x)
  (let ((cell (cons x nil)))
    (if (q-tail q) (setf (cdr (q-tail q)) cell) (setf (q-head q) cell))
    (setf (q-tail q) cell)))
(defun q-deq (q)
  (let ((c (q-head q)))
    (setf (q-head q) (cdr c))
    (unless (q-head q) (setf (q-tail q) nil))
    (car c)))

;;; --- the rw-lock, faithful to rw-lock.lisp -------------------------------
(defstruct rwl
  (lock (sb-thread:make-mutex))
  (readers 0 :type integer)
  (wq (make-q))
  (writer nil)
  (wait (sb-thread:make-waitqueue)))

(defun %front-p (l thr)
  (and (not (q-empty-p (rwl-wq l))) (eq thr (q-front (rwl-wq l)))))

(defvar *mode* :block)   ; :block or :poll

(defun acq-write (l)                    ; cf. acquire-write-lock
  (let ((self sb-thread:*current-thread*))
    (sb-thread:with-recursive-lock ((rwl-lock l)) (q-enq (rwl-wq l) self))
    (loop
      (when (eq (rwl-writer l) self) (return))
      (let ((got nil))
        (sb-thread:with-recursive-lock ((rwl-lock l))
          (if (and (null (rwl-writer l)) (%front-p l self))
              (progn (setf (rwl-writer l) self) (setf got t))
              (ecase *mode*
                (:poll nil)             ; fall through to sleep outside lock
                (:block (sb-thread:condition-wait (rwl-wait l) (rwl-lock l))))))
        (when (and (not got) (eq *mode* :poll)) (sleep 0.001))))))

(defun rel-write (l)                    ; cf. release-write-lock
  (let ((self sb-thread:*current-thread*))
    (sb-thread:with-recursive-lock ((rwl-lock l))
      (if (%front-p l self) (q-deq (rwl-wq l)) (error "not owner"))
      (unless (%front-p l self)
        (setf (rwl-writer l) nil)
        (ecase *mode*
          (:poll nil)                   ; ECL: waiters poll, nothing to notify
          (:block (sb-thread:condition-broadcast (rwl-wait l))))))))

(defun acq-read (l)                     ; cf. acquire-read-lock
  (loop
    (sb-thread:with-recursive-lock ((rwl-lock l))
      (when (null (rwl-writer l))
        (incf (rwl-readers l))
        (return-from acq-read)))
    (when (eq *mode* :poll) (sleep 0.001))))

(defun rel-read (l)                     ; cf. release-read-lock
  (sb-thread:with-recursive-lock ((rwl-lock l))
    (decf (rwl-readers l))))

;;; --- benchmarks ----------------------------------------------------------
(defun run-write (n-threads acq-per-thread mode)
  (let ((*mode* mode) (l (make-rwl)) (gate (sb-thread:make-semaphore))
        (threads '()) (counter 0))
    (declare (ignorable counter))
    (dotimes (i n-threads)
      (push (sb-thread:make-thread
             (lambda () (sb-thread:wait-on-semaphore gate)
               (dotimes (k acq-per-thread)
                 (acq-write l) (incf counter) (rel-write l))))
            threads))
    (let ((t0 (get-internal-real-time)))
      (sb-thread:signal-semaphore gate n-threads)
      (dolist (th threads) (sb-thread:join-thread th))
      (let* ((secs (/ (- (get-internal-real-time) t0)
                      (float internal-time-units-per-second)))
             (total (* n-threads acq-per-thread)))
        (format t "~&mode=~6A threads=~3D total=~8D wall=~7,3Fs handoffs/s=~10,1F~%"
                mode n-threads total secs (if (zerop secs) 0 (/ total secs)))
        (force-output)))))

(defun run-mixed (n-threads ops mode write-ratio)
  (let ((*mode* mode) (l (make-rwl)) (gate (sb-thread:make-semaphore))
        (threads '()) (ctr 0))
    (declare (ignorable ctr))
    (dotimes (i n-threads)
      (let ((seed (+ 1 i)))
        (push (sb-thread:make-thread
               (lambda () (sb-thread:wait-on-semaphore gate)
                 (let ((rng seed))   ; private LCG, no shared-RNG lock
                   (dotimes (k ops)
                     (setf rng (logand (+ (* rng 1103515245) 12345) #xffffff))
                     (if (< (/ (mod rng 1000) 1000.0) write-ratio)
                         (progn (acq-write l) (incf ctr) (rel-write l))
                         (progn (acq-read l) (rel-read l)))))))
              threads)))
    (let ((t0 (get-internal-real-time)))
      (sb-thread:signal-semaphore gate n-threads)
      (dolist (th threads) (sb-thread:join-thread th))
      (let* ((secs (/ (- (get-internal-real-time) t0)
                      (float internal-time-units-per-second)))
             (total (* n-threads ops)))
        (format t "~&mode=~6A wr=~3,0F% threads=~3D total=~8D wall=~7,3Fs ops/s=~10,1F~%"
                mode (* 100 write-ratio) n-threads total secs
                (if (zerop secs) 0 (/ total secs)))
        (force-output)))))

(defun main ()
  (let ((tag (second sb-ext:*posix-argv*)))
    (if (and tag (string= tag "readmostly"))
        (progn
          (format t "~&=== read-mostly (5% writes), BLOCK path ===~%")
          (dolist (n '(4 16 64)) (run-mixed n 5000 :block 0.05))
          (format t "~&=== read-mostly (5% writes), POLL path (ECL) ===~%")
          (dolist (n '(4 16 64)) (run-mixed n 5000 :poll 0.05)))
        (progn
          (format t "~&=== write contention, BLOCK path (SBCL/LispWorks/modern ECL) ===~%")
          (dolist (n '(4 8 16 32 64)) (run-write n 2000 :block))
          (format t "~&=== write contention, POLL path (current #+ecl sleep 0.001) ===~%")
          (dolist (n '(4 8 16 32 64)) (run-write n 2000 :poll))))))

(main)
(sb-ext:exit)
