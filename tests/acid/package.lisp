;;;; Package definition for the graph-db ACID compliance test suite.

(in-package #:cl-user)

(defpackage #:graph-db/acid-test
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; graph lifecycle
                #:*graph*
                #:make-graph
                #:open-graph
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
                #:def-view
                #:*schema-node-metadata*
                ;; views
                #:invoke-graph-view
                #:map-view
                #:yield
                ;; transaction internals
                #:recover-transactions
                ;; misc
                #:gen-id)
  (:import-from #:graph-db
                #:*maximum-transaction-attempts*
                #:*after-apply-tx-writes-hook*)
  ;; bordeaux-threads for semaphore-based isolation tests
  (:import-from #:bordeaux-threads
                #:make-thread
                #:make-semaphore
                #:signal-semaphore
                #:wait-on-semaphore)
  (:export #:run-acid-tests
           #:acid-suite))
