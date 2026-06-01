;;;; Master suite, shared fixtures, and schema for concurrency tests.
;;;;
;;;; Schema types are defined once at load time against
;;;; *CONCURRENCY-GRAPH-NAME*.  Each test creates a fresh graph of that name
;;;; via WITH-CONC-GRAPH so tests are isolated from each other.

(in-package #:graph-db/concurrency-test)

;;; ---------------------------------------------------------------------------
;;; Suite root
;;; ---------------------------------------------------------------------------

(def-suite concurrency-suite
  :description "Thread-safety and concurrency tests for graph-db.")

(defun run-concurrency-tests ()
  "Run the concurrency suite.  Returns T on all-pass.
Called by (asdf:test-system :graph-db/concurrency-test)."
  (log:config :error)
  (let ((results (run 'concurrency-suite)))
    (explain! results)
    (results-status results)))

;;; ---------------------------------------------------------------------------
;;; Temp-directory and GC helpers (mirrored from graph-db/test)
;;; ---------------------------------------------------------------------------

(defun make-temp-directory ()
  (let ((dir (merge-pathnames
              (format nil "graph-db-conc-~36R/" (random (expt 36 12)))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-temp-directory ((var) &body body)
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun collect-garbage ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl  (ccl:gc)
  #+lispworks (hcl:gc-all)
  #+ecl  (ext:gc t))

;;; ---------------------------------------------------------------------------
;;; Thread-count constant
;;;
;;; Keep modest on ECL (slower thread startup / teardown).
;;; ---------------------------------------------------------------------------

(defparameter *thread-count*
  #+ecl 4
  #-ecl 8)

;;; ---------------------------------------------------------------------------
;;; Schema
;;;
;;; Fresh types for the concurrency graph so these tests are independent of
;;; the integration schema in graph-db/test.  Reset the metadata list first
;;; so reloading this file does not accumulate duplicate registrations.
;;; ---------------------------------------------------------------------------

(defparameter *concurrency-graph-name* :graph-db-concurrency-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *concurrency-graph-name* *schema-node-metadata*) nil))

(def-vertex c-item ()
  ((value))
  :graph-db-concurrency-test)

(def-edge c-link ()
  ()
  :graph-db-concurrency-test)

;;; ---------------------------------------------------------------------------
;;; Graph fixture
;;; ---------------------------------------------------------------------------

(defmacro with-conc-graph ((g) &body body)
  "Bind G and *GRAPH* to a fresh on-disk graph; tear it down afterwards."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *concurrency-graph-name*
                             (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; View setup (called inside with-conc-graph, before any inserts)
;;; ---------------------------------------------------------------------------

(defun define-concurrency-views ()
  "Register the views used by the concurrent-view tests on *GRAPH*."
  (def-view c-item-by-value :lessp (c-item :graph-db-concurrency-test)
    (:map (lambda (item)
            (yield (slot-value item 'value) t)))))
