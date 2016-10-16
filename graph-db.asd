;; ASDF package description for graph-db              -*- Lisp -*-

(defpackage :graph-db-system (:use :cl :asdf))
(in-package :graph-db-system)

(defsystem graph-db
  :name "VivaceGraph"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "1.0"
  :depends-on (:bordeaux-threads
               :alexandria
               :trivial-shell
               :iterate
               :cffi
               :osicat
               :cl-ppcre
               :uuid
               :split-sequence
               #+sbcl :sb-concurrency
               #+ccl :closer-mop
               #+ccl :trivial-timeout
               :cl-store
               :local-time
               :ieee-floats
               :cl-json
               :hunchentoot
               :ningle
               :clack
               :log4cl
               :usocket
               :md5)
  :components (;;(:file "uuid")
               (:file "package")
               (:file "globals" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "utilities" :depends-on ("globals"))
               (:file "queue" :depends-on ("utilities"))
               (:file "mailbox" :depends-on ("queue"))
               #+sbcl (:file "rw-lock" :depends-on ("queue"))
               #+sbcl (:file "mmap" :depends-on ("rw-lock"))
               #-sbcl (:file "mmap" :depends-on ("queue"))
               (:file "pcons" :depends-on ("mmap"))
               (:file "node-id" :depends-on ("package"))
               (:file "buffer-pool" :depends-on ("pcons" "node-id"))
               (:file "serialize" :depends-on ("conditions" "buffer-pool"))
               (:file "linear-hash" :depends-on ("serialize"))
               (:file "allocator" :depends-on ("serialize"))
               (:file "graph-class" :depends-on ("globals"))
               (:file "cursors" :depends-on ("package"))
               (:file "skip-list" :depends-on ("allocator" "linear-hash"))
               (:file "skip-list-cursors" :depends-on ("skip-list" "cursors"))
               (:file "index-list" :depends-on ("linear-hash" "allocator"))
               (:file "ve-index" :depends-on ("skip-list-cursors" "index-list" "graph-class"))
               (:file "vev-index" :depends-on ("index-list" "graph-class"))
               (:file "type-index" :depends-on ("vev-index"))
               (:file "graph" :depends-on
                      ("ve-index" "vev-index" "type-index" "linear-hash" "allocator"))
               (:file "stats" :depends-on ("graph"))
               (:file "schema" :depends-on ("stats"))
               (:file "node-class" :depends-on ("schema"))
               (:file "views" :depends-on ("node-class"))
               (:file "primitive-node" :depends-on ("views"))
               (:file "vertex" :depends-on ("primitive-node"))
               (:file "edge" :depends-on ("vertex"))
               (:file "gc" :depends-on ("edge" "vertex" "views"))
               (:file "transactions" :depends-on ("graph-class" "type-index" "vev-index" "ve-index" "edge" "vertex" "gc"))
               (:file "transaction-restore" :depends-on ("transactions"))
               (:file "transaction-log-streaming" :depends-on ("transactions"))
               (:file "transaction-streaming" :depends-on ("transaction-log-streaming" "mailbox"))
               (:file "backup" :depends-on ("edge"))
               (:file "replication" :depends-on ("backup"))
               (:file "txn-log" :depends-on ("replication"))
               (:file "functor" :depends-on ("vertex" "edge" "views" "schema"))
               (:file "prologc" :depends-on ("functor"))
               (:file "prolog-functors" :depends-on ("prologc"))
               (:file "interface" :depends-on ("schema" "edge" "vertex" "views"))
               (:file "traverse" :depends-on ("interface"))
               (:file "rest" :depends-on ("traverse"))))
