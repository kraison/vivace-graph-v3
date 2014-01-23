;; ASDF package description for graph-db              -*- Lisp -*-

(defpackage :graph-db-system (:use :cl :asdf))
(in-package :graph-db-system)

(defsystem graph-db
  :name "GraphDB"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :depends-on (:bordeaux-threads
               :iterate
               :cffi
               :osicat
               :cl-ppcre
               :uuid
               :split-sequence
               :sb-concurrency
               :cl-store
               :local-time
               :ieee-floats
               :cl-json
               :log4cl
               :usocket)
  :components ((:file "uuid")
               (:file "package" :depends-on ("uuid"))
               (:file "globals" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "guid" :depends-on ("package"))
               (:file "utilities" :depends-on ("globals"))
               (:file "queue" :depends-on ("utilities"))
               (:file "rw-lock" :depends-on ("queue"))
               (:file "mmap" :depends-on ("rw-lock"))
               (:file "pcons" :depends-on ("mmap"))
               (:file "buffer-pool" :depends-on ("pcons"))
               (:file "serialize" :depends-on ("conditions" "buffer-pool" "guid"))
               (:file "linear-hash" :depends-on ("serialize"))
               (:file "allocator" :depends-on ("serialize"))
               (:file "graph-class" :depends-on ("globals"))
               (:file "cursors" :depends-on ("package"))
               (:file "skip-list" :depends-on ("allocator" "linear-hash"))
               (:file "skip-list-cursors" :depends-on ("skip-list" "cursors"))
               (:file "index-list" :depends-on ("linear-hash" "allocator"))
               (:file "ve-index" :depends-on ("skip-list-cursors" "index-list" "rw-lock" "graph-class"))
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
               (:file "backup" :depends-on ("edge"))
               (:file "replication" :depends-on ("backup"))
               (:file "txn-log" :depends-on ("replication"))
               (:file "functor" :depends-on ("vertex" "edge" "views" "schema"))
               (:file "prologc" :depends-on ("functor"))
               (:file "prolog-functors" :depends-on ("prologc"))
               (:file "interface" :depends-on ("schema" "edge" "vertex" "views"))
               ))
