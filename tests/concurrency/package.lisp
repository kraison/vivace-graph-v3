;;;; Package definition for the graph-db concurrency / thread-safety test suite.
;;;;
;;;; Kept separate from graph-db/test so the concurrency suite can be loaded
;;;; independently without pulling in the sequential unit suite.

(in-package #:cl-user)

(defpackage #:graph-db/concurrency-test
  (:use #:cl #:fiveam)
  ;; graph-db's CUT symbol must win over fiveam's rerun-tests helper.
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; graph lifecycle
                #:*graph*
                #:make-graph
                #:close-graph
                #:with-transaction
                ;; node operations
                #:lookup-vertex
                #:map-vertices
                #:map-edges
                #:mark-deleted
                #:copy
                #:save
                #:id
                #:to
                #:from
                #:weight
                #:def-vertex
                #:def-edge
                #:*schema-node-metadata*
                ;; views
                #:def-view
                #:invoke-graph-view
                #:map-view
                #:yield
                ;; prolog
                #:select-flat
                #:<-
                #:lookup-functor
                #:delete-functor
                #:make-functor-symbol
                #:make-functor
                #:is-a/2
                #:select/2
                #:node-slot-value/3
                ;; rw-lock -- exported for sbcl/ecl, internal for ccl but
                ;; (:import-from) doesn't require export status
                #:make-rw-lock
                #:with-read-lock
                #:with-write-lock
                ;; misc
                #:gen-id)
  ;; Internal graph-db symbols the tests need but that are not exported.
  ;; Using (:import-from) works on unexported symbols too.
  (:import-from #:graph-db
                #:*maximum-transaction-attempts*
                #:functor-clauses
                #:add-functor-clause
                #:reset-functor)
  (:export #:run-concurrency-tests
           #:concurrency-suite))
