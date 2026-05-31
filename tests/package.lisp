;;;; Test package for graph-db.
;;;;
;;;; The storage layers we exercise here (serialization, the allocator,
;;;; the linear hash, the skip list) are internal to GRAPH-DB and are not
;;;; part of the public export list, so we import the specific internal
;;;; symbols under test.  Keeping this list explicit doubles as an
;;;; inventory of the low-level surface the suite covers.

(in-package #:cl-user)

(defpackage #:graph-db/test
  (:use #:cl #:fiveam)
  ;; graph-db's cut symbol must win over fiveam's ! (its rerun-tests helper),
  ;; so the Prolog compiler recognizes ! in query goals as cut.
  (:shadowing-import-from #:graph-db #:!)
  (:import-from #:graph-db
                ;; serialization
                #:serialize
                #:deserialize
                #:serialized-equal
                ;; allocator / memory
                #:create-memory
                #:open-memory
                #:close-memory
                #:allocate
                #:free
                #:normalize-allocation-data-size
                #:map-memory
                #:free-list
                #:unallocated-memory-available
                #:set-byte
                #:get-byte
                #:get-bytes
                #:memory-data-offset
                #:memory-mmap
                #:memory-pointer
                ;; linear hash
                #:make-lhash
                #:open-lhash
                #:close-lhash
                #:lhash-insert
                #:lhash-get
                #:lhash-remove
                #:lhash-update
                #:map-lhash
                #:read-lhash-count
                #:duplicate-key-error
                #:nonexistent-key-error
                ;; skip list
                #:make-skip-list
                #:add-to-skip-list
                #:find-in-skip-list
                #:remove-from-skip-list
                #:skip-list-count
                #:skip-list-to-list
                #:%sn-value
                #:%sl-length
                ;; index list
                #:make-index-list
                #:index-list-push
                #:index-list-pushnew
                #:index-list-member-p
                #:map-index-list
                #:remove-from-index-list
                #:index-list-head
                #:delete-index-list
                ;; type index
                #:make-type-index
                #:close-type-index
                #:type-index-push
                #:type-index-remove
                #:get-type-index-list
                ;; graph model / public API
                #:*graph*
                #:make-graph
                #:open-graph
                #:close-graph
                #:with-transaction
                #:lookup-vertex
                #:lookup-edge
                #:outgoing-edges
                #:incoming-edges
                #:map-vertices
                #:map-edges
                #:weight
                #:to
                #:from
                #:id
                #:deleted-p
                #:mark-deleted
                #:copy
                #:save
                #:def-vertex
                #:def-edge
                #:node-slot-value
                #:*schema-node-metadata*
                ;; views
                #:def-view
                #:invoke-graph-view
                #:map-view
                #:map-reduced-view
                #:yield
                #:string-id
                ;; prolog queries.  The query compiler interns functor symbols
                ;; in the current package, so the built-in functors a query
                ;; references must be accessible here by name.  Edge functors
                ;; (e.g. g-knows/2) are generated in this package already.
                #:select
                #:select-flat
                #:select-one
                #:is-a/2
                #:select/2
                #:node-slot-value/3
                #:not/1
                #:bagof/3
                #:setof/3
                ;; rule definition + functor-table access (for stress tests).
                ;; (! is shadowing-imported above.)
                #:<-
                #:lookup-functor
                #:delete-functor
                #:make-functor-symbol
                ;; misc helpers
                #:gen-id)
  (:export #:run-tests
           #:graph-db-suite))
