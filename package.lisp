(in-package #:cl-user)

(defpackage #:graph-db
  (:use #:cl
        #:bordeaux-threads
        #:local-time
        #+ccl #:closer-mop
        #+lispworks #:clos
        #+sbcl #:sb-mop
        #+sbcl #:sb-pcl)
  #+sbcl (:shadowing-import-from "SB-EXT" "WORD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-METHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "FINALIZE-INHERITANCE")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-GENERIC-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "DEFMETHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "DEFGENERIC")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-CLASS")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-DISCRIMINATING-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-APPLICABLE-METHODS-USING-CLASSES")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-EFFECTIVE-METHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "METHOD-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "MAKE-METHOD-LAMBDA")
  (:export #:make-graph
           #:open-graph
           #:close-graph
           #:lookup-graph
           #:graph-stats
           #:check-data-integrity
           #:snapshot
           #:replay
           #:restore
           #:location
           #:schema
           #:indexes
           #:*graph*
           #:execute-tx
           #:transaction-p
           #:graph-name
           #:transaction-error
           #:master-host
           #:replication-port
           #:slave-socket
           #:replication-key
           #:master-txn-id
           #:stop-replication-p
           #:execute-tx-action
           #:write-last-txn-id
           #:read-last-txn-id
           #:start-replication
           #:stop-replication
           #:stop-buffer-pool

           #:start-rest
           #:stop-rest
           #:def-rest-procedure
           #:*rest-procedures*

           #:with-transaction
           #:lookup-object
           #:update-node
           #:delete-node
           #:commit
           #:rollback
           #:*transaction*
           #:no-transaction-in-progress

           #:def-node-type
           #:def-vertex
           #:def-edge
           #:edge-exists-p
           #:lookup-node-type-by-name
           #:instantiate-node-type
           #:*schema-node-metadata*
           #:with-write-locked-class
           #:with-read-locked-class
           #:schema-class-locks
           #+sbcl #:make-rw-lock
           #+sbcl #:with-read-lock
           #+sbcl #:with-write-lock
           #+sbcl #:acquire-read-lock
           #+sbcl #:release-read-lock
           #+sbcl #:acquire-write-lock
           #+sbcl #:release-write-lock
           #+sbcl #:rw-lock-p

           #:vertex
           #:edge
           #:generic-edge
           #:generic-vertex
           #:make-vertex
           #:make-edge
           #:lookup-vertex
           #:lookup-edge
           #:to
           #:from
           #:weight
           #:id
           #:string-id
           #:node-to-alist
           #:type-id
           #:revision
           #:deleted-p
           #:active-edge-p
           #:data
           #:traverse
           #:traversal-path
           #:end-vertex
           #:map-vertices
           #:map-edges
           #:outgoing-edges
           #:incoming-edges
           #:node-slot-value
           #:copy
           #:save
           #:mark-deleted
           #:stale-revision-error

           #:def-view
           #:*view-rv*
           #:yield
           #:map-view
           #:map-reduced-view
           #:invoke-graph-view
           #:make-view
           #:delete-view
           #:save-views
           #:restore-views
           #:get-view-table-for-class
           #:regenerate-view
           #:lookup-view-group
           #:lookup-view
           #:with-write-locked-view-group
           #:with-read-locked-view-group
           #:view-group-lock

           ;; Prolog
           #:def-global-prolog-functor
           #:def-prolog-compiler-macro
           #:compile-body
           #:args
           #:*prolog-global-functors*
           #:deref-exp
           #:unify
           #:select
           #:?
           #:?-
           #:q-
           #:!
           #:cut
           #:var-deref
           #:undo-bindings
           #:replace-?-vars
           #:variables-in
           #:make-functor-symbol
           #:*trail*
           #:*var-counter*
           #:*functor*
           #:make-functor
           #:maybe-add-undo-bindings
           #:compile-clause
           #:show-prolog-vars
           #:prolog-error
           #:prolog-ignore
           #:delete-functor
           #:set-functor-fn
           #:*seen-table*
           #:*select-flat*
           #:*select-list*
           #:select-count
           #:*select-count*
           #:*select-skip*
           #:*select-current-count*
           #:*select-current-skip*
           #:select-one
           #:select-flat
           #:select-first
           #:do-query
           #:map-query
           #:valid-prolog-query-p
           #:init-prolog
           #:*prolog-graph*
           #:*prolog-trace*
           #:trace-prolog
           #:untrace-prolog
           #:make-node-table
           #:node-equal

           ))
