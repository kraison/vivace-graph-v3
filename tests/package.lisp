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
                ;; misc helpers
                #:gen-id)
  (:export #:run-tests
           #:graph-db-suite))
