;;;; GEOS context pool.
;;;;
;;;; GEOS's reentrant API requires that a GEOSContextHandle_t is NEVER used by
;;;; two threads at once.  VivaceGraph runs transactions (and thus spatial
;;;; refine calls) in the CALLER's thread, with many concurrent callers, and has
;;;; no per-thread-state / thread-exit-hook machinery.  So instead of one
;;;; context per thread (which would leak for transient threads), we keep a
;;;; process-global BORROW/RETURN POOL: `with-geos-context' checks out a context
;;;; (creating one lazily if the free-list is empty), runs the body, and returns
;;;; it.  A checked-out context is removed from the free-list, so it is held by
;;;; exactly one thread at a time -- satisfying GEOS's contract -- and contexts
;;;; are reused across threads, bounded by peak concurrency.

(in-package :graph-db)

;;; --------------------------------------------------------------------------
;;; Per-call error capture
;;;
;;; GEOS reports errors through a message handler callback (non-variadic in the
;;; `_r' API: void(const char *msg, void *userdata)).  The callback runs
;;; synchronously, on the same thread, inside the GEOS call -- so a thread-local
;;; dynamic binding of *geos-last-error*, established by with-geos-context, is in
;;; effect when it fires.  We stash the message there and signal GEOS-ERROR from
;;; the operation wrapper when GEOS returns an error sentinel.
;;; --------------------------------------------------------------------------

(defvar *geos-last-error* nil
  "Most recent GEOS error message on this thread, set by the message handler
callback within the dynamic extent of WITH-GEOS-CONTEXT.")

(cffi:defcallback geos-error-message-handler :void
    ((message :string) (userdata :pointer))
  (declare (ignore userdata))
  (setf *geos-last-error* (and message (copy-seq message))))

(cffi:defcallback geos-notice-message-handler :void
    ((message :string) (userdata :pointer))
  (declare (ignore message userdata))
  nil)

;;; --------------------------------------------------------------------------
;;; Context objects + pool
;;; --------------------------------------------------------------------------

(defstruct (geos-ctx (:constructor %make-geos-ctx-struct))
  (handle (cffi:null-pointer) :type t)
  (reader (cffi:null-pointer) :type t)
  (writer (cffi:null-pointer) :type t)
  (in-use nil :type boolean))          ; debug-only checkout flag

(defvar *geos-pool* '() "Free list of available GEOS-CTX objects.")
(defvar *geos-pool-lock* (bordeaux-threads:make-lock "geos-pool"))
(defvar *geos-pool-created* 0 "Total contexts created (live + free).")
(defvar *geos-pool-in-use* 0 "Contexts currently checked out.")
(defvar *geos-pool-peak* 0 "High-water mark of concurrent checkouts.")
(defvar *geos-pool-debug* nil
  "When true, WITH-GEOS-CONTEXT asserts a context is never checked out twice.")

(defun %make-geos-context ()
  "Create a fresh GEOS context with error/notice handlers and a cached WKT
reader + writer (full precision).  Assumes libgeos is loaded."
  (let ((handle (%geos-init)))
    (when (cffi:null-pointer-p handle)
      (error 'geos-error :message "GEOS_init_r returned NULL"))
    (%geos-set-error-handler handle (cffi:callback geos-error-message-handler)
                             (cffi:null-pointer))
    (%geos-set-notice-handler handle (cffi:callback geos-notice-message-handler)
                              (cffi:null-pointer))
    (let ((reader (%geos-wktreader-create handle))
          (writer (%geos-wktwriter-create handle)))
      ;; Trim=1: on GEOS >= 3.12 this selects adaptive, round-trip-exact output
      ;; (and trims trailing zeros).  We never lower the rounding precision, which
      ;; would truncate.  Writer output only feeds make-valid readback + tests;
      ;; predicates read booleans, so their exactness rests on our geometry->wkt
      ;; input, not on the writer.
      (%geos-wktwriter-set-trim handle writer 1)
      (%make-geos-ctx-struct :handle handle :reader reader :writer writer))))

(defun %destroy-geos-context (ctx)
  "Free CTX's reader, writer, and handle.  Only call when CTX is not in use."
  (let ((h (geos-ctx-handle ctx)))
    (unless (cffi:null-pointer-p (geos-ctx-reader ctx))
      (%geos-wktreader-destroy h (geos-ctx-reader ctx)))
    (unless (cffi:null-pointer-p (geos-ctx-writer ctx))
      (%geos-wktwriter-destroy h (geos-ctx-writer ctx)))
    (unless (cffi:null-pointer-p h)
      (%geos-finish h))
    (setf (geos-ctx-handle ctx) (cffi:null-pointer)
          (geos-ctx-reader ctx) (cffi:null-pointer)
          (geos-ctx-writer ctx) (cffi:null-pointer))))

(defun %geos-checkout ()
  "Borrow a context: pop a free one or lazily create a new one.  Bumps in-use /
peak counters.  Called under no lock; takes the pool lock itself."
  (bordeaux-threads:with-lock-held (*geos-pool-lock*)
    (let ((ctx (if *geos-pool*
                   (pop *geos-pool*)
                   (progn (incf *geos-pool-created*) (%make-geos-context)))))
      (when (and *geos-pool-debug* (geos-ctx-in-use ctx))
        (error 'geos-error :message "GEOS context double-checkout detected"))
      (setf (geos-ctx-in-use ctx) t)
      (incf *geos-pool-in-use*)
      (when (> *geos-pool-in-use* *geos-pool-peak*)
        (setf *geos-pool-peak* *geos-pool-in-use*))
      ctx)))

(defun %geos-checkin (ctx)
  "Return CTX to the free list."
  (bordeaux-threads:with-lock-held (*geos-pool-lock*)
    (setf (geos-ctx-in-use ctx) nil)
    (decf *geos-pool-in-use*)
    (push ctx *geos-pool*)))

(defmacro with-geos-context ((var) &body body)
  "Borrow a GEOS context, bind VAR to it for BODY, and return it to the pool
afterwards (even on non-local exit).  *geos-last-error* is freshly bound NIL for
the extent so the error handler's message is scoped to this operation."
  `(let ((,var (%geos-checkout))
         (*geos-last-error* nil))
     (unwind-protect (progn ,@body)
       (%geos-checkin ,var))))

(defun geos-shutdown ()
  "Destroy all pooled GEOS contexts and reset the pool.  Call when the system is
quiescent (no GEOS operation in flight) -- e.g. between test runs or at clean
process exit.  Contexts currently checked out are NOT tracked here; ensure none
are in use."
  (bordeaux-threads:with-lock-held (*geos-pool-lock*)
    (mapc #'%destroy-geos-context *geos-pool*)
    (setf *geos-pool* '()
          *geos-pool-created* 0
          *geos-pool-in-use* 0
          *geos-pool-peak* 0)))
