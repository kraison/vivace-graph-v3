(in-package :graph-db)

;;;; Minimal direct-CFFI POSIX shim.
;;;;
;;;; Replaces the osicat dependency in the embeddable core (used by mmap.lisp,
;;;; transactions.lisp, node-id.lisp).  osicat ships a C grovel/wrapper that must
;;;; be cross-built per target; calling libc / Bionic directly via foreign-funcall
;;;; removes that native build step so graph-db/core cross-compiles to
;;;; aarch64-linux-android under ECL+NDK with nothing to compile but Lisp->C.
;;;; (The codebase already calls foreign-funcall "write" directly in mmap.lisp;
;;;; this just completes the set.)
;;;;
;;;; All targets are LP64 -- long = off_t = size_t = 64-bit -- on both the desktop
;;;; validation host (darwin-arm64) and the deployment target
;;;; (aarch64-linux-android).  Constants that differ between Darwin and
;;;; Linux/Bionic are gated on #+darwin; everything else assumes the Linux ABI
;;;; (which is what Bionic follows).

;;; ---------------------------------------------------------------------------
;;; Constants.  open(2) flags, lseek(2) whence, mmap(2) prot/flags, msync flags.
;;; Several of these differ between Darwin and Linux -- the gated ones are the
;;; whole reason this shim is platform-aware rather than a blind libc call.
;;; ---------------------------------------------------------------------------
(defconstant +o-rdonly+ 0)
(defconstant +o-rdwr+   2)
(defconstant +o-creat+  #+darwin #x0200 #-darwin #o100) ; Darwin 0x200, Linux 0100

(defconstant +seek-set+ 0)
(defconstant +seek-end+ 2)

(defconstant +prot-none+  0)
(defconstant +prot-read+  1)
(defconstant +prot-write+ 2)

(defconstant +map-shared+    #x01)
(defconstant +map-private+   #x02)
(defconstant +map-fixed+     #x10)
(defconstant +map-anonymous+ #+darwin #x1000 #-darwin #x20)
(defconstant +map-noreserve+ #+darwin #x40   #-darwin #x4000)

(defconstant +ms-sync+       #+darwin #x10    #-darwin #x04) ; Darwin 16, Linux 4

;; (void *)-1 as an unsigned 64-bit address: mmap's failure sentinel (MAP_FAILED).
(defconstant +map-failed-address+ (1- (expt 2 64)))

;;; ---------------------------------------------------------------------------
;;; Syscall wrappers.  size_t -> :unsigned-long, off_t -> :long, mode_t ->
;;; :unsigned-int (all correct on the LP64 targets above).
;;; ---------------------------------------------------------------------------
(declaim (inline %posix-close %posix-lseek %posix-fchmod %posix-munmap
                 %posix-msync))

(defun %posix-open (path flags &optional (mode #o640))
  "open(2).  PATH is a Lisp pathname/string.  Returns the fd, signals on error."
  (let ((fd (cffi:foreign-funcall "open"
                                  :string (namestring path)
                                  :int flags
                                  :unsigned-int mode
                                  :int)))
    (when (minusp fd)
      (error "posix open failed for ~A (flags ~D)" path flags))
    fd))

(defun %posix-close (fd)
  (cffi:foreign-funcall "close" :int fd :int))

(defun %posix-lseek (fd offset whence)
  (cffi:foreign-funcall "lseek" :int fd :long offset :int whence :long))

(defun %posix-fchmod (fd mode)
  (cffi:foreign-funcall "fchmod" :int fd :unsigned-int mode :int))

(defun %posix-rename (old new)
  "rename(2): atomic, overwrites an existing target (POSIX).  Used for txn files."
  (let ((r (cffi:foreign-funcall "rename"
                                 :string (namestring old)
                                 :string (namestring new)
                                 :int)))
    (when (minusp r)
      (error "posix rename failed: ~A -> ~A" old new))
    r))

(defun %posix-mmap (addr length prot flags fd offset)
  "mmap(2).  ADDR is a foreign pointer (or null-pointer).  Returns the mapping
pointer, signals on MAP_FAILED."
  (let ((p (cffi:foreign-funcall "mmap"
                                 :pointer addr
                                 :unsigned-long length
                                 :int prot
                                 :int flags
                                 :int fd
                                 :long offset
                                 :pointer)))
    (when (= (cffi:pointer-address p) +map-failed-address+)
      (error "posix mmap failed (len=~D fd=~D prot=~D flags=~D)"
             length fd prot flags))
    p))

(defun %posix-munmap (addr length)
  (cffi:foreign-funcall "munmap" :pointer addr :unsigned-long length :int))

(defun %posix-msync (addr length flags)
  (cffi:foreign-funcall "msync" :pointer addr :unsigned-long length :int flags :int))

;;; ---------------------------------------------------------------------------
;;; File size without stat(2).  Avoids mirroring the platform-specific struct
;;; stat layout (st_size offset differs across Darwin/glibc/Bionic): for an open
;;; fd, lseek to SEEK_END returns the size; for a path, CL file-length suffices.
;;; ---------------------------------------------------------------------------
(defun %posix-file-size-fd (fd)
  (%posix-lseek fd 0 +seek-end+))

(defun %file-size (path)
  (with-open-file (s path :element-type '(unsigned-byte 8) :if-does-not-exist :error)
    (file-length s)))

;;; ---------------------------------------------------------------------------
;;; gettimeofday(2) -> (values seconds microseconds).  struct timeval is
;;; {time_t tv_sec; suseconds_t tv_usec}; tv_sec is 64-bit on both targets, but
;;; tv_usec is 32-bit on Darwin and 64-bit (long) on LP64 Linux/Bionic.
;;; ---------------------------------------------------------------------------
(defun %posix-gettimeofday ()
  (cffi:with-foreign-object (tv :uint8 16)
    (cffi:foreign-funcall "gettimeofday" :pointer tv :pointer (cffi:null-pointer) :int)
    (values (cffi:mem-ref tv :int64 0)
            #+darwin (cffi:mem-ref tv :int32 8)
            #-darwin (cffi:mem-ref tv :int64 8))))
