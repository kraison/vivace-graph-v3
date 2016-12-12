(in-package :graph-db)

(defvar *buffer-pool*
  #+sbcl (make-hash-table :test 'eq :synchronized t)
  #+ccl (make-hash-table :test 'eq :shared t))
(defvar *buffer-pool-stats* nil)
(defvar *buffer-pool-thread* nil)
(defvar *stop-buffer-pool* nil)
(defvar *buffer-pool-low-water-mark* 1000)
(defvar *free-memory-low-water-mark* 10485760)
(defparameter *allow-force-gc-p* nil)
#+ccl
(defvar *buffer-pool-lock* (make-lock))

(defstruct (buffer-pool-stats
             (:conc-name bps-))
  (buffer-8 0 :type (UNSIGNED-BYTE 64))
  (buffer-16 0 :type (UNSIGNED-BYTE 64))
  (buffer-18 0 :type (UNSIGNED-BYTE 64))
  (buffer-24 0 :type (UNSIGNED-BYTE 64))
  (buffer-34 0 :type (UNSIGNED-BYTE 64))
  (pcons 0 :type (UNSIGNED-BYTE 64))
  (vertex 0 :type (UNSIGNED-BYTE 64))
  (edge 0 :type (UNSIGNED-BYTE 64))
  (skip-node 0 :type (UNSIGNED-BYTE 64)))

(defun dump-buffer-pool-stats ()
  (let* ((stats
          (list
           (cons :cache-misses
                 (list (cons :BUFFER-8 (bps-buffer-8 *buffer-pool-stats*))
                       (cons :BUFFER-16 (bps-buffer-16 *buffer-pool-stats*))
                       (cons :BUFFER-18 (bps-buffer-18 *buffer-pool-stats*))
                       (cons :BUFFER-24 (bps-buffer-24 *buffer-pool-stats*))
                       (cons :BUFFER-34 (bps-buffer-34 *buffer-pool-stats*))
                       (cons :pcons (bps-pcons *buffer-pool-stats*))
                       (cons :vertex (bps-vertex *buffer-pool-stats*))
                       (cons :edge (bps-edge *buffer-pool-stats*))
                       (cons :skip-node (bps-skip-node *buffer-pool-stats*))))))
           (buffers nil))
    (maphash (lambda (k v)
               (push (cons k (length (first v))) buffers))
             *buffer-pool*)
    (append stats (list (cons :available-buffers buffers)))))

(defun monitor-buffer-pool ()
  (let ((free-memory (free-memory)))
    (if (and *allow-force-gc-p*
             (<= free-memory *free-memory-low-water-mark*))
        (progn
          (log:warn "VG forcing a full GC, ~A bytes dynamic space available"
                    free-memory)
          #+sbcl (sb-ext:gc :full t)
          #+ccl (gc)
          )
        (let ((stats (dump-buffer-pool-stats)))
          (dolist (stat (cdr (assoc :available-buffers stats)))
            (when (< (cdr stat) *buffer-pool-low-water-mark*)
              (case (car stat)
                (16
                 (log:info "BUFFER-POOL: Refreshing byte-vector-16 buffers (~D)"
                           (- 1000000 (cdr stat)))
                 (dotimes (i (- 1000000 (cdr stat)))
                   (let ((b (make-byte-vector 16)))
                     #+sbcl
                     (sb-ext:atomic-push b (first (gethash 16 *buffer-pool*)))
                     #+ccl
                     (ccl:with-lock-grabbed (*buffer-pool-lock*)
                       (push b (first (gethash 16 *buffer-pool*)))))))
                ((8 18 24 34)
                 (log:info "BUFFER-POOL: Refreshing byte-vector-~D buffers (~D)"
                           (car stat) (- 100000 (cdr stat)))
                 (dotimes (i (- 100000 (cdr stat)))
                   (let ((b (make-byte-vector (car stat))))
                     #+sbcl
                     (sb-ext:atomic-push b (first (gethash (car stat) *buffer-pool*)))
                     #+ccl
                     (ccl:with-lock-grabbed (*buffer-pool-lock*)
                       (push b (first (gethash (car stat) *buffer-pool*)))))))
                (:skip-node
                 (log:info "BUFFER-POOL: Refreshing skip-node buffers (~D)"
                           (- 1000000 (cdr stat)))
                 (dotimes (i (- 1000000 (cdr stat)))
                   (make-skip-node-buffer)))
                (:pcons
                 (log:info "BUFFER-POOL: Refreshing pcons buffers (~D)"
                           (- 1000000 (cdr stat)))
                 (dotimes (i (- 1000000 (cdr stat)))
                   (make-pcons-buffer)))
                (:edge
                 (log:info "BUFFER-POOL: Refreshing edge buffers (~D)"
                           (- 100000 (cdr stat)))
                 (dotimes (i (- 100000 (cdr stat)))
                   (make-edge-buffer)))
                (:vertex
                 (log:info "BUFFER-POOL: Refreshing vertex buffers (~D)"
                           (- 100000 (cdr stat)))
                 (dotimes (i (- 100000 (cdr stat)))
                   (make-vertex-buffer))))))))))

(defun monitor-buffer-pool-loop ()
  (loop
     until *stop-buffer-pool*
     do
       (monitor-buffer-pool)
       (sleep 1)))

(defun reset-buffer-pool-stats ()
  (setq *buffer-pool-stats* (make-buffer-pool-stats)))

(defun make-pcons-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing pcons buffer")
    (let ((p (%make-pcons)))
      #+sbcl
      (sb-ext:atomic-push p (first (gethash :pcons *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push p (first (gethash :pcons *buffer-pool*)))))))

(defun make-vertex-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing vertex buffer")
<<<<<<< HEAD
    (let ((v (make-instance 'vertex :id (gen-vertex-id))))
=======
    (let ((v (make-instance 'vertex)))
>>>>>>> 2fa1d84cd2ae0aaefd0c3aa2a5c44fa76c24f5dd
      #+sbcl
      (sb-ext:atomic-push v (first (gethash :vertex *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push v (first (gethash :vertex *buffer-pool*)))))))

(defun make-edge-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing edge buffer")
<<<<<<< HEAD
    (let ((e (make-instance 'edge :id (gen-edge-id))))
=======
    (let ((e (make-instance 'edge)))
>>>>>>> 2fa1d84cd2ae0aaefd0c3aa2a5c44fa76c24f5dd
      #+sbcl
      (sb-ext:atomic-push e (first (gethash :edge *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push e (first (gethash :edge *buffer-pool*)))))))

(defun make-skip-node-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing skip-node buffer")
    (let ((s (%make-skip-node)))
      #+sbcl
      (sb-ext:atomic-push s (first (gethash :skip-node *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push s (first (gethash :skip-node *buffer-pool*)))))))

(defun make-byte-vector-8-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-8")
    (let ((b (make-byte-vector 8)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 8 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 8 *buffer-pool*)))))))

(defun make-byte-vector-16-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-16")
    (let ((b (make-byte-vector 16)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 16 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 16 *buffer-pool*)))))))

(defun make-byte-vector-18-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-18")
    (let ((b (make-byte-vector 18)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 18 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 18 *buffer-pool*)))))))

(defun make-byte-vector-24-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-24")
    (let ((b (make-byte-vector 24)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 24 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 24 *buffer-pool*)))))))

(defun make-byte-vector-34-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-34")
    (let ((b (make-byte-vector 34)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 34 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 34 *buffer-pool*)))))))

(defun buffer-pool-running-p ()
  (and (threadp *buffer-pool-thread*)
       (thread-alive-p *buffer-pool-thread*)))

(defun init-buffer-pool ()
  (when (buffer-pool-running-p)
    (error "~A is already running. Cannot init-buffer-pool"
           *buffer-pool-thread*))
  (setq *stop-buffer-pool* nil)
  (reset-buffer-pool-stats)
  (setq *buffer-pool*
        #+sbcl (make-hash-table :test 'eq :synchronized t)
        #+ccl (make-hash-table :test 'eq :shared t))
  (setf (gethash :vertex *buffer-pool*)
        (list nil)
        (gethash :edge *buffer-pool*)
        (list nil)
        (gethash :pcons *buffer-pool*)
        (list nil)
        (gethash :skip-node *buffer-pool*)
        (list nil))
  (dolist (num '(8 16 18 24 34))
    (setf (gethash num *buffer-pool*)
          (list nil)))
  (dotimes (i 100000)
    (make-vertex-buffer)
    (make-edge-buffer)
    (make-byte-vector-8-buffer)
    (make-byte-vector-18-buffer)
    (make-byte-vector-24-buffer)
    (make-byte-vector-34-buffer))
  (dotimes (i 1000000)
    (make-pcons-buffer)
    (make-skip-node-buffer)
    (make-byte-vector-16-buffer))
  (setq *buffer-pool-thread*
        (make-thread 'monitor-buffer-pool-loop :name "buffer-pool-thread"))
  *buffer-pool*)

(defun ensure-buffer-pool ()
  (or (buffer-pool-running-p)
      (init-buffer-pool)))

(defun stop-buffer-pool ()
  (setq *stop-buffer-pool* t)
  (when (and (threadp *buffer-pool-thread*)
             (thread-alive-p *buffer-pool-thread*))
    (join-thread *buffer-pool-thread*)))

(defun buffer-incf-stat (size)
  (when (buffer-pool-running-p)
    (case size
      (8
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-8 *buffer-pool-stats*)))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-8 *buffer-pool-stats*)))
      (16
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-16 *buffer-pool-stats*)))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-16 *buffer-pool-stats*)))
      (18
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-18 *buffer-pool-stats*)))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-18 *buffer-pool-stats*)))
      (24
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-24 *buffer-pool-stats*)))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-24 *buffer-pool-stats*)))
      (34
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-34 *buffer-pool-stats*)))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-34 *buffer-pool-stats*))))))

(defun get-buffer (size)
  (or (and (buffer-pool-running-p)
           #+sbcl
           (sb-ext:atomic-pop (first (gethash size *buffer-pool*)))
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash size *buffer-pool*)))))
      (progn
        (buffer-incf-stat size)
        (make-byte-vector size))))

(defun release-buffer (buffer)
  (when (buffer-pool-running-p)
    (let ((size (length buffer)))
      (dotimes (i size)
        (setf (aref buffer i) 0))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push buffer (first (gethash size *buffer-pool*))))
      #+sbcl
      (sb-ext:atomic-push buffer (first (gethash size *buffer-pool*)))
      nil)))

(defun get-vertex-buffer ()
  (or (and (buffer-pool-running-p)
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :vertex *buffer-pool*))))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :vertex *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-vertex *buffer-pool-stats*)))
          #+sbcl
          (sb-ext:atomic-incf (bps-vertex *buffer-pool-stats*)))
        (make-instance 'vertex))))

(defun get-edge-buffer ()
  (or (and (buffer-pool-running-p)
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :edge *buffer-pool*))))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :edge *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-edge *buffer-pool-stats*)))
          #+sbcl
          (sb-ext:atomic-incf (bps-edge *buffer-pool-stats*)))
        (make-instance 'edge))))

(defun get-skip-node-buffer ()
  (or (and (buffer-pool-running-p)
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :skip-node *buffer-pool*))))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :skip-node *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-skip-node *buffer-pool-stats*)))
          #+sbcl
          (sb-ext:atomic-incf (bps-skip-node *buffer-pool-stats*)))
        (%make-skip-node))))

(defun get-pcons-buffer ()
  (or (and (buffer-pool-running-p)
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :pcons *buffer-pool*))))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :pcons *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-pcons *buffer-pool-stats*)))
          #+sbcl
          (sb-ext:atomic-incf (bps-pcons *buffer-pool-stats*)))
        (%make-pcons))))
