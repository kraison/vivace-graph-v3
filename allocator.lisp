(in-package :graph-db)

(alexandria:define-constant +memory-magic-byte-offset+ 0)
(alexandria:define-constant +memory-storage-version-offset+ 1)
(alexandria:define-constant +memory-memory-pointer-offset+ 2)
(alexandria:define-constant +memory-bins-offset+ 10)
(alexandria:define-constant +memory-bin-count+ 128)
(alexandria:define-constant +memory-usable-offset+ 1034) ;; (+ 2 8 (* 8 128))
(alexandria:define-constant +memory-header-size+ 8)

(defstruct (memory
             (:print-function
              (lambda (m s d)
                (declare (ignore d))
                (format s "#<MEMORY :LOCATION ~S :SIZE ~S :DATA-OFFSET ~S>"
                        (memory-location m) (memory-size m) (memory-data-offset m)))))
  location size mmap
  (free-list (make-hash-table :synchronized t))
  free-list-thread
  (pointer 0 :type (UNSIGNED-BYTE 64))
  (lock (make-rw-lock))
  extent-size data-offset
  (bin-locks (make-array +memory-bin-count+ :initial-element (make-recursive-lock)))
  (cache-lock (make-rw-lock))
  (cache (make-hash-table :synchronized t :weakness :value)))

(defmethod set-byte ((memory memory) offset byte)
  (declare (type word offset))
  (declare (type (integer 0 255) byte))
  (unless (>= offset (memory-data-offset memory))
    (log:error "SEGV: Tried to write byte ~S at unsafe position ~S in ~S" byte offset memory)
    (error "SEGV: Tried to write byte ~S at unsafe position ~S in ~S" byte offset memory))
  ;;(dbg "SET-BYTE: ~A ADDR ~A TO ~A" (memory-location memory) offset byte)
  ;;(setf (cffi:mem-aref (m-pointer (memory-mmap memory)) :unsigned-char offset) byte))
  (set-byte (memory-mmap memory) offset byte))

(defmethod get-byte ((memory memory) offset)
  (declare (type word offset))
  ;;(cffi:mem-aref (m-pointer (memory-mmap memory)) :unsigned-char offset))
  (get-byte (memory-mmap memory) offset))

(defmethod get-bytes ((memory memory) offset length)
  (declare (type word offset length))
  (let ((vec (make-byte-vector length)))
    (dotimes (i length)
      (setf (aref vec i) (get-byte memory (+ i offset))))
    vec))

(defmethod serialize-uint64 ((memory memory) int offset)
  (declare (type word int offset))
  (unless (>= offset (memory-data-offset memory))
    (log:error "SEGV: Tried to serialize-uint64 ~S at unsafe position ~S in ~S" int offset memory)
    (error "SEGV: Tried to serialize-uint64 ~S at unsafe position ~S in ~S" int offset memory))
  ;;(dbg "MEMORY: SERIALIZING UINT64 ~A TO ADDR ~A" int offset)
  (serialize-uint64 (memory-mmap memory) int offset))

(defmethod deserialize-uint64 ((memory memory) offset)
  (declare (type word offset))
  (deserialize-uint64 (memory-mmap memory) offset))

(defmethod serialize-pointer ((memory memory) int offset)
  (serialize-uint64 memory int offset))

(defmethod deserialize-pointer ((memory memory) offset)
  (deserialize-uint64 memory offset))

(defmethod incf-uint64 ((memory memory) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 memory offset)))
    (incf int)
    (serialize-uint64 memory int offset)
    int))

(defmethod decf-uint64 ((memory memory) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 memory offset)))
    (serialize-uint64 memory (decf int) offset)
    int))

(defmacro with-locked-bin ((memory size &key wait-p) &body body)
  (let ((lock (gensym)))
    `(let ((bin (mod (sxhash ,size) +memory-bin-count+)))
       (let ((,lock (aref (memory-bin-locks ,memory) bin)))
         (sb-thread:with-recursive-lock (,lock :wait-p ,wait-p)
           ,@body)))))

(defun deserialize-header (mmap offset)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((int 0))
    (dotimes (i 8)
      (setf int
            (dpb (get-byte mmap (+ offset i)) (byte 8 (* i 8)) int)))
    (let ((free-p (null (ldb-test (byte 1 0) int))))
      ;;(ldb (byte 1 0) int)))
      (if free-p
          (let ((next (deserialize-pointer mmap (+ 8 offset)))
                (prev (deserialize-pointer mmap (+ 8 8 offset))))
            (values (ash int -1) free-p next prev))
          (values (ash int -1) free-p 0 0)))))

(defun serialize-header (mmap offset alloc-len &key (active-p t))
  ;; Max bytes we can allocate is 9223372036854775808 (ash (expt 2 64) -1)
  ;; since we use the first bit as the active / inactive tag
  ;;(declare (optimize (speed 3) (safety 0)))
  (assert (and (> alloc-len 0) (<= alloc-len (ash (expt 2 64) -1))))
  (let ((total-len (ash alloc-len 1)))
    (if active-p
        (setq total-len (dpb 1 (byte 1 0) total-len))
        (setq total-len (dpb 0 (byte 1 0) total-len)))
    (let ((n-bytes 8))
      ;; serialize the flags and length
      (dotimes (i n-bytes)
        ;;(dbg "SERIALIZING ~8,'0B AT OFFSET ~A" (ldb (byte 8 0) total-len) offset)
        (set-byte mmap offset (ldb (byte 8 0) total-len))
        (incf offset)
        (setq total-len (ash total-len -8)))
      ;; 0 out prev and next pointers
      (dotimes (i 16)
        ;;(dbg "SERIALIZING ~8,'0B AT OFFSET ~S (~S)" 0 (+ offset i) i)
        (set-byte mmap (+ offset i) 0))
      ;; Return the offset of the start of the user-accessible portion of the block
      offset)))

(defun block-free-p (memory offset)
  (multiple-value-bind (len free-p)
      (deserialize-header (memory-mmap memory) offset)
    (declare (ignore len))
    free-p))

(defun map-memory (fn memory &key collect-p include-free-p)
  (let ((offset (memory-data-offset memory)) (done-p nil) (r nil))
    (loop until done-p
       do
         (multiple-value-bind (block-len free-p)
             (deserialize-header (memory-mmap memory) offset)
           ;;(dbg "GOT BLOCK OF ~A BYTES AT OFFSET ~A" block-len offset)
           (if (= 0 block-len)
               (setq done-p t)
               (progn
                 (when (or include-free-p (not free-p))
                   (if collect-p
                       (push (funcall fn (+ 8 offset) block-len free-p) r)
                       (funcall fn (+ 8 offset) block-len free-p)))
                 (incf offset (+ 8 block-len))))))
    (when collect-p
      (nreverse r))))

(defun free-list (memory)
  (remove-if
   'null
   (map-memory
    (lambda (offset size free?)
      (when free?
        (cons offset size)))
    memory
    :collect-p t
    :include-free-p t)))

(defun set-memory-pointer (memory pointer)
  (setf (memory-pointer memory) pointer)
  (let ((n-bytes 8) (offset +memory-memory-pointer-offset+))
    (dotimes (i n-bytes)
      (set-byte (memory-mmap memory) offset (ldb (byte 8 0) pointer))
      (incf offset)
      (setq pointer (ash pointer -8)))))

(defun read-memory-pointer (memory)
  (let ((n-bytes 8) (offset +memory-memory-pointer-offset+) (int 0))
    (dotimes (i n-bytes)
      (setq int (dpb (get-byte (memory-mmap memory) (+ offset i))
                     (byte 8 (* i 8)) int)))
    int))

(defmethod get-chunk-of-size ((memory memory) size)
  (with-locked-bin (memory size)
    (let ((header-offset (pop (gethash size (memory-free-list memory)))))
      (when header-offset
        ;; FIXME: zero the bytes?
        (serialize-header (memory-mmap memory) header-offset size :active-p t)
        (+ +memory-header-size+ header-offset)))))

(defun add-to-free-list (memory offset length)
  ;; Use header offset, not user offset!
  (with-locked-bin (memory length)
    (push offset (gethash length (memory-free-list memory)))))

(defmethod init-free-list ((memory memory))
  (dolist (pair (free-list memory))
    (push (- (car pair) +memory-header-size+)
          (gethash (cdr pair) (memory-free-list memory)))))

(defun create-memory (location size &key (extent-size (* 1024 1024 100)))
  (let ((memory
         (make-memory :location location
                      :pointer +memory-usable-offset+
                      :data-offset +memory-usable-offset+
                      :extent-size extent-size
                      :size size
                      :lock (make-rw-lock)
                      :mmap (mmap-file location
                                       :size (+ 1 +memory-usable-offset+ size)))))
    (set-byte (memory-mmap memory) +memory-magic-byte-offset+ +data-magic-byte+)
    (set-byte (memory-mmap memory) +memory-storage-version-offset+ +storage-version+)
    (init-free-list memory)
    (set-memory-pointer memory (memory-pointer memory))
    memory))

(defun memory-magic-byte-corrupt-p (memory)
  (let ((byte (get-byte (memory-mmap memory) +memory-magic-byte-offset+)))
    (unless (= +data-magic-byte+ byte)
      (log:error "SEGV: ~A magic-byte corrupted: 0x~X" (memory-location memory) byte)
      byte)))

(defun open-memory (location &key (extent-size (* 1024 1024 100)))
  (let ((memory
         (make-memory :location location
                      :data-offset +memory-usable-offset+
                      :extent-size extent-size
                      :lock (make-rw-lock)
                      :mmap (mmap-file location :create-p nil))))
    (when (memory-magic-byte-corrupt-p memory)
      (munmap-file (memory-mmap memory))
      (error "~A is not a memory file!" location))
    (unless (= +storage-version+ (get-byte (memory-mmap memory) +memory-storage-version-offset+))
      (munmap-file (memory-mmap memory))
      (error "~A is the wrong data version!" location))
    (setf (memory-pointer memory) (read-memory-pointer memory)
          (memory-size memory) (size-of (memory-mmap memory)))
    (init-free-list memory)
    memory))

(defun close-memory (memory)
  (with-write-lock ((memory-lock memory))
    (munmap-file (memory-mmap memory) :save-p t)
    (setf (memory-mmap memory) nil)))

(defun grow-memory (memory length)
  (let ((num-extents (ceiling (/ length (memory-extent-size memory)))))
    (log:info "GROWING MEMORY BY ~A EXTENTS" num-extents)
    (extend-mapped-file (memory-mmap memory)
                        (* num-extents (memory-extent-size memory)))
    (setf (memory-size memory)
          (mapped-file-length (memory-mmap memory)))))

(defun free (memory offset)
  ;; offset is the beginning of the user-accessible portion of the data block
  (unless (>= offset (memory-data-offset memory))
    (log:error "SEGV: Unsafe call to free: pointer ~S ~S" offset memory)
    (error "SEGV: Cannot free pointer ~S" offset))
  (let ((header-offset (- offset +memory-header-size+)))
    (let ((size (deserialize-header (memory-mmap memory) header-offset)))
      (log:info "ADDING ~S TO ~A FREE LIST, SIZE ~S"
                header-offset (m-path (memory-mmap memory)) size)
      (serialize-header (memory-mmap memory) header-offset size :active-p nil)
      ;; FIXME: zero the bytes?
      (add-to-free-list memory header-offset size)
      header-offset)))

(defun normalize-chunk-size (size)
  (cond ((<= size 16)
         16)
        ((<= size 512)
         (* 8 (ceiling size 8)))
        (t
         size)))

(defun allocate (memory size)
  (setq size (normalize-chunk-size size))
  (with-write-lock ((memory-lock memory))
    (let ((pointer
           (or
            ;;(get-chunk-of-size memory size)
            (let ((current-pointer nil))
              (let ((additional-memory-needed
                     ;; size plus 8 bytes for header
                     (- (+ +memory-header-size+ size)
                        (- (memory-size memory) (memory-pointer memory)))))
                (when (> additional-memory-needed 0)
                  (grow-memory memory additional-memory-needed))
                (setq current-pointer (memory-pointer memory))
                (set-memory-pointer memory
                                    (+ (memory-pointer memory)
                                       +memory-header-size+
                                       size))
                (let ((block-begin
                       (serialize-header (memory-mmap memory)
                                         current-pointer
                                         size)))
                  block-begin))))))
      (unless (>= pointer (memory-data-offset memory))
        (log:error "SEGV: Tried to allocate at unsafe position ~S in ~S" pointer memory)
        (error "Tried to allocate at unsafe position ~S in ~S" pointer memory))
      pointer)))

(defun test-allocator ()
  (let ((memory (create-memory "/var/tmp/testmem.dat" 10240))
        (pointers nil) (threads nil)
        (gate (sb-concurrency:make-gate :open nil)))
    (unwind-protect
         (progn
           (dotimes (i 100)
             (push
              (make-thread
               (lambda ()
                 (sb-concurrency:wait-on-gate gate)
                 (let ((pointer (allocate memory 100)))
                   (push pointer pointers))))
              threads))
           (sb-concurrency:open-gate gate)
           (dbg "THREAD: ~A" (length threads))
           (loop until (notany 'thread-alive-p threads) do (sleep 1))
           (dolist (p pointers)
             (free memory p)))
      (progn
        (close-memory memory)
        (delete-file "/var/tmp/testmem.dat")))))



