;;;; Master test suite and shared fixtures for graph-db.

(in-package #:graph-db/test)

(def-suite graph-db-suite
  :description "All graph-db unit tests.")

(defun run-tests ()
  "Run the entire graph-db test suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db)."
  ;; The storage layers log prolifically at :debug/:info; keep test output
  ;; to genuine problems.
  (log:config :error)
  (let ((results (run 'graph-db-suite)))
    (explain! results)
    (results-status results)))

;;; ---------------------------------------------------------------------------
;;; Temp-file fixtures
;;;
;;; The storage layers all live in mmap'd files, so each test needs a
;;; private scratch directory that is reliably torn down afterwards.
;;; ---------------------------------------------------------------------------

(defun make-temp-directory ()
  "Create and return a fresh, unique scratch directory pathname."
  (let ((dir (merge-pathnames
              (format nil "graph-db-test-~36R/" (random (expt 36 12)))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-temp-directory ((var) &body body)
  "Bind VAR to a fresh scratch directory, run BODY, then delete the tree."
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defmacro with-temp-memory ((var &key (size '(* 1024 1024 64))) &body body)
  "Bind VAR to a freshly created MEMORY backed by a temp file, run BODY,
then close it and remove the scratch directory."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (create-memory (namestring (merge-pathnames "heap.dat" ,dir))
                                  ,size)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-memory ,var)))))))

(defmacro with-temp-lhash ((var &rest make-args) &body body)
  "Bind VAR to a freshly created LHASH rooted in a temp directory, run
BODY, then close it and remove the scratch directory.  MAKE-ARGS are
passed through to MAKE-LHASH (e.g. :buckets 4)."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (make-lhash :location ,dir ,@make-args)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-lhash ,var)))))))

;;; ---------------------------------------------------------------------------
;;; Skip-list construction helper
;;;
;;; Skip lists need a heap plus a full complement of key/value
;;; serializers and comparators.  This builds an integer-keyed list with
;;; fixnum sentinels, mirroring the configuration in graph-db's own
;;; sl-test / sl-perf-test routines.
;;; ---------------------------------------------------------------------------

(defun make-integer-skip-list (heap &key duplicates-allowed-p)
  "Return an integer-keyed skip list over HEAP."
  (make-skip-list :heap heap
                  :head-key most-negative-fixnum
                  :head-value 0
                  :tail-key most-positive-fixnum
                  :tail-value 0
                  :key-equal '=
                  :key-comparison '<
                  :key-serializer 'serialize
                  :key-deserializer 'deserialize
                  :value-serializer 'serialize
                  :value-deserializer 'deserialize
                  :value-equal 'equal
                  :duplicates-allowed-p duplicates-allowed-p))
