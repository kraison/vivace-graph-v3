(in-package #:cl-user)

(defpackage #:social-shopping
  (:use #:cl #:graph-db #:view-server #:drakma #:clouchdb #:parenscript #:json-rpc
        #:bordeaux-threads #:local-time #:sb-concurrency #:cl-elasticsearch
        #:cl-memcached)
  (:export
   #:defdoc
   #:create-bigcouch-db
   #:add-to-index
   #:delete-from-index
   #:enqueue-product-action
   #:list-pending-bookmarklet-actions
   #:with-doc-slots

   #:extract-validators
   #:extract-private-slots
   #:extract-read-only-slots
   #:make-slot-map
   #:*default-db-table*
   #:*validators*
   #:*private-slots*
   #:*read-only-slots*
   #:*doc-slot-map*
   #:get-doc
   #:save-doc
   #:return-appropriate-object
   #:with-retries
   #:id
   #:revision
   #:inactive
   #:deactivation-date
   #:owner
   #:document
   #:with-gensyms
   #:filter-doc
   #:db-connect

   #:super-user-p
   #:lookup-api-user
   #:send-error
   #:defun-admin-json-rpc
   #:defun-json-rpc-new
   #:invoke-auth-rpc
   #:get-auth-level
   #:*admin-enabled*
   #:json-rpc-handler
   #:sign-request
   #:api-logger
   #:*access-key*
   #:*var-id*
   #:method-allowed-p
   #:*api-docs*
   #:restricted-p
   #:*restricted-rpc-methods*
   #:*queue-db-name*
   #:*db-host*
   #:*db-name*
   #:describe-doc
   #:make-doc
   #:make-doc-alist
   #:make-doc-from-alist
   #:now-string
   #:api-request
   #:*api-url*
   #:massage-possible-bad-string
   #:null-or-string-p
   #:valid-currency-code-p
   #:valid-url-p
   #:gender-p
   ))
