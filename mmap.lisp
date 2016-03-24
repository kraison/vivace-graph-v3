(in-package :graph-db)

(cffi:defctype size :unsigned-int)
(deftype uint32 () '(integer 0 4294967295))
(deftype uint40 () '(integer 0 1099511627775))
(deftype uint64 () '(integer 0 18446744073709551615))

(defstruct (mapped-file
	     (:conc-name m-)
	     (:predicate mapped-file-p))
  path pointer fd
  ;;(remap-lock (make-rw-lock))
  )

(defstruct mpointer mmap loc)

(defmethod mapped-file-length ((mapped-file mapped-file))
  (osicat-posix:stat-size (osicat-posix:fstat (m-fd mapped-file))))

(defmethod set-byte :around (mf offset byte)
  (handler-case
      (call-next-method)
    (sb-kernel::memory-fault-error (c)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-byte mf offset byte))))

(defmethod set-byte ((mapped-file mapped-file) offset byte)
  (declare (type word offset))
  (declare (type (integer 0 255) byte))
  ;;(log:debug "SET-BYTE: ~A ADDR ~A TO ~A" (m-path mapped-file) offset byte)
  (setf (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset) byte))

(defmethod get-byte :around (mf offset)
  (handler-case
      (call-next-method)
    (sb-kernel::memory-fault-error (c)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-byte mf offset))))

(defmethod get-byte ((mapped-file mapped-file) offset)
  (declare (type word offset))
  (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset))

(defmethod get-bytes :around (mf offset length)
  (handler-case
      (call-next-method)
    (sb-kernel::memory-fault-error (c)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (get-bytes mf offset length))))

(defmethod get-bytes ((mapped-file mapped-file) offset length)
  (declare (type word offset length))
  (let ((vec (make-byte-vector length)))
    (dotimes (i length)
      (setf (aref vec i) (get-byte mapped-file (+ i offset))))
    vec))

(defmethod set-bytes :around (mf vec offset length)
  (handler-case
      (call-next-method)
    (sb-kernel::memory-fault-error (c)
      (log:error "SEGV: GOT ~A in ~A; retrying." c mf)
      (set-bytes mf offset length))))

(defmethod set-bytes ((mapped-file mapped-file) vec offset length)
  (declare (type word offset length))
  (dotimes (i length)
    (set-byte mapped-file (+ i offset) (aref vec i)))
  vec)

(defmethod size-of ((mmap mapped-file))
  (osicat-posix:stat-size (osicat-posix:stat (m-path mmap))))

(defun mmap-file (file &key (create-p t) (size (* 4096 25600)))
  "Use mmap() to map FILE into memory."
  (log:debug "Opening mmap ~A" file)
  (when (and (not create-p) (not (probe-file file)))
    (error "mmap-file: ~A does not exist and create-p is not true." file))
  (let* ((fd (osicat-posix:open
              file
              (if create-p
                  (logior osicat-posix:O-CREAT osicat-posix:O-RDWR)
                  osicat-posix:O-RDWR))))
    (when create-p
      (osicat-posix:lseek fd (1- size) osicat-posix:seek-set)
      (cffi:with-foreign-string (null (format nil "~A" (code-char 0)))
        (cffi:foreign-funcall "write"
                              :int fd
                              :pointer null
                              size 1)))
    ;; Make sure the file size is set right!
    (setq size (osicat-posix:stat-size (osicat-posix:fstat fd)))
    (let* ((pointer
	    (osicat-posix:mmap
             (cffi:null-pointer)
             size
             (logior osicat-posix:prot-read osicat-posix:prot-write)
             osicat-posix:map-shared
             fd
             0)))
;	(cffi:foreign-funcall "madvise"
;			      :int fd
;			      size size
;			      :int MADV_RANDOM))) ;; NEED MADV_RANDOM FROM ?
      (make-mapped-file :path (truename file)
			:fd fd
			:pointer pointer))))

(defmethod sync-region ((mapped-file mapped-file) &key addr length
			(sync osicat-posix:ms-sync))
  (osicat-posix:msync (or addr (m-pointer mapped-file))
                      (or length (mapped-file-length mapped-file))
                      sync))

(defmethod munmap-file ((mapped-file mapped-file) &key (save-p nil)
			(sync osicat-posix:ms-sync))
  (when save-p
    ;;(log:debug "Calling msync on ~S" mapped-file)
    (osicat-posix:msync (m-pointer mapped-file)
                        (mapped-file-length mapped-file)
                        sync))
  ;;(log:debug "Calling munmap on ~S" mapped-file)
  (osicat-posix:munmap (m-pointer mapped-file) (mapped-file-length mapped-file))
  (osicat-posix:close (m-fd mapped-file))
  (setf (m-pointer mapped-file) nil)
  nil)

(defmethod extend-mapped-file ((mapped-file mapped-file) (length integer))
  (log:debug "EXTENDING MMAP ~A" mapped-file)
  (let ((ptr (osicat-posix:mremap (m-pointer mapped-file)
                                  (mapped-file-length mapped-file)
                                  (+ length (mapped-file-length mapped-file))
                                  osicat-posix:MREMAP-MAYMOVE)))
    (setf (m-pointer mapped-file) ptr)
    (osicat-posix:lseek (m-fd mapped-file)
                        (1- (+ length (mapped-file-length mapped-file)))
                        osicat-posix:seek-set)
    (cffi:with-foreign-string (null (format nil "~A" (code-char 0)))
      (cffi:foreign-funcall "write"
                            :int (m-fd mapped-file)
                            :pointer null
                            size 1))
    mapped-file))

(defmethod serialize-uint64 ((mf mapped-file) int offset)
  (declare (type word int offset))
  ;;(log:debug "MMAP: SERIALIZING UINT64 ~A TO ADDR ~A" int offset)
  (dotimes (i 8)
    ;;(log:debug "WRITING BYTE ~X" (ldb (byte 8 0) int))
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    ;;(log:debug "   WROTE BYTE ~A AT OFFSET ~X" (ldb (byte 8 0) int) offset)
    (incf offset))
  ;;  (incf offset))
  offset)

(defmethod deserialize-uint64 ((mf mapped-file) offset)
  "Decode a UINT64."
  (declare (type word offset))
  (let ((int 0))
    (declare (type word int))
    (dotimes (i 8)
      (setq int (dpb (get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod deserialize-uint64 ((array array) offset)
  "Decode a UINT64."
  (let ((int 0))
    (declare (type word int))
    (dotimes (i 8)
      (setq int (dpb (aref array (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod incf-uint64 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 mf offset)))
    (incf int)
    (serialize-uint64 mf int offset)
    int))

(defmethod decf-uint64 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int (deserialize-uint64 mf offset)))
    (serialize-uint64 mf (decf int) offset)
    int))

(defmethod serialize-pointer ((mf mapped-file) pointer offset)
  ;;(log:debug "SERIALIZING POINTER ~A TO ADDR ~A" pointer offset)
  (serialize-uint64 mf pointer offset))

(defmethod deserialize-pointer ((mf mapped-file) offset)
  ;;(log:debug "DESERIALIZING POINTER AT ADDR ~A" offset)
  (deserialize-uint64 mf offset))

(defmethod serialize-uint32 ((mf mapped-file) int offset)
  (declare (type uint32 int))
  (declare (type word offset))
  (dotimes (i 4)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  (incf offset))

(defmethod deserialize-uint32 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int 0))
    (declare (type uint32 int))
    (dotimes (i 4)
      (setq int (dpb (get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))

(defmethod serialize-uint40 ((mf mapped-file) int offset)
  (declare (type uint40 int))
  (declare (type word offset))
  (dotimes (i 5)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset))
  (incf offset))

(defmethod deserialize-uint40 ((mf mapped-file) offset)
  (declare (type word offset))
  (let ((int 0))
    (declare (type uint40 int))
    (dotimes (i 5)
      (setq int (dpb (get-byte mf (+ i offset)) (byte 8 (* i 8)) int)))
    int))
