;;;; Master suite + runner for the GEOS integration tests.

(in-package #:graph-db/geos-test)

(def-suite geos-suite
  :description "Tests for the optional libgeos_c integration (graph-db/geos).")

(defun run-geos-tests ()
  "Run the GEOS test suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/geos)."
  (log:config :error)
  #+ecl (ext:set-limit 'ext:heap-size (* 6 1024 1024 1024))
  (let ((results (run 'geos-suite)))
    (explain! results)
    (results-status results)))

;;; A handful of tests need to run with GEOS forced unavailable to prove the
;;; fallback path.  This binds the flag off for the dynamic extent of BODY.
(defmacro without-geos (&body body)
  `(let ((*geos-available-p* nil)
         (*geos-makevalid-available-p* nil))
     ,@body))

;;; --------------------------------------------------------------------------
;;; Graph fixture + geometry vertex type for the S3 query tests.
;;; --------------------------------------------------------------------------

(defparameter *geos-graph-name* :graph-db-geos-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *geos-graph-name* graph-db::*schema-node-metadata*) nil))

;; A geometry slot marked :index t makes the type spatially indexed.
(def-vertex geos-place ()
  ((geom :type graph-db::geometry :index t))
  :graph-db-geos-test)

(defun make-temp-directory ()
  (let ((dir (merge-pathnames
              (format nil "graph-db-geos-~36R/" (random (expt 36 12)))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-geos-graph ((g) &body body)
  "Bind G + *graph* to a fresh on-disk graph; tear it down afterwards."
  (let ((dir (gensym "DIR")))
    `(let ((,dir (make-temp-directory)))
       (unwind-protect
            (let ((,g (make-graph *geos-graph-name* (namestring ,dir)
                                  :buffer-pool-size 1000)))
              (unwind-protect (let ((*graph* ,g)) ,@body)
                (ignore-errors (close-graph ,g :snapshot-p nil))))
         (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore)))))
