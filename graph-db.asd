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
               #+(or ccl lispworks) :closer-mop
               #+(or ccl lispworks) :trivial-timeout
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
               (:file "cl-store-ecl" :depends-on ("package"))
               (:file "globals" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "utilities" :depends-on ("globals"))
               (:file "queue" :depends-on ("utilities"))
               (:file "mailbox" :depends-on ("queue"))
               #+(or sbcl lispworks ecl) (:file "rw-lock" :depends-on ("queue"))
               #+(or sbcl lispworks ecl) (:file "mmap" :depends-on ("rw-lock"))
               #-(or sbcl lispworks ecl) (:file "mmap" :depends-on ("queue"))
               (:file "pcons" :depends-on ("mmap"))
               (:file "node-id" :depends-on ("package"))
               (:file "buffer-pool" :depends-on ("pcons" "node-id"))
               (:file "serialize" :depends-on ("conditions" "buffer-pool" "cl-store-ecl"))
               (:file "geometry" :depends-on ("serialize"))
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
               (:file "rest" :depends-on ("traverse")))
  :in-order-to ((test-op (test-op :graph-db/test))))

(defsystem graph-db/concurrency-test
  :name "VivaceGraph concurrency test suite"
  :description "FiveAM thread-safety and concurrency tests for graph-db."
  :depends-on (:graph-db :fiveam :bordeaux-threads)
  :pathname "tests/concurrency/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "helpers")
               (:file "rw-lock-tests")
               (:file "transaction-tests")
               (:file "data-structure-tests")
               (:file "graph-ops-tests")
               (:file "view-tests")
               (:file "prolog-tests")
               (:file "acid-regression-tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call
                             :graph-db/concurrency-test :run-concurrency-tests)
                      (error "graph-db concurrency tests failed."))))

(defsystem graph-db/acid-test
  :name "VivaceGraph ACID compliance tests"
  :depends-on (:graph-db :fiveam :bordeaux-threads)
  :pathname "tests/acid/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "atomicity-tests")
               (:file "isolation-tests")
               (:file "durability-tests"))
  :perform (test-op (op c)
              (unless (uiop:symbol-call :graph-db/acid-test :run-acid-tests)
                (error "graph-db ACID tests failed."))))

(defsystem graph-db/stress-test
  :name "VivaceGraph stress test suite"
  :description "Single-threaded scale and correctness stress tests for graph-db."
  :depends-on (:graph-db :fiveam)
  :pathname "tests/stress/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "storage-stress")
               (:file "graph-stress")
               (:file "transaction-stress")
               (:file "view-stress"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :graph-db/stress-test :run-stress-tests)
                      (error "graph-db stress tests failed."))))

(defsystem graph-db/concurrent-stress-test
  :name "VivaceGraph concurrent stress test suite"
  :description "Multi-threaded scale and stability tests for graph-db."
  :depends-on (:graph-db :fiveam :bordeaux-threads)
  :pathname "tests/concurrent-stress/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "graph-storm")
               (:file "transaction-storm")
               (:file "view-storm")
               (:file "mixed-storm")
               (:file "mmap-remap-stress"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call
                             :graph-db/concurrent-stress-test
                             :run-concurrent-stress-tests)
                      (error "graph-db concurrent stress tests failed."))))

(defsystem graph-db/perf-test
  :name "VivaceGraph performance benchmark suite"
  :description "SBCL-focused performance benchmarks for graph-db (measurement, not pass/fail)."
  :depends-on (:graph-db :bordeaux-threads)
  :pathname "tests/perf/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "benchmarks"))
  :perform (test-op (op c)
                    (uiop:symbol-call :graph-db/perf-test :run-perf)))

(defsystem graph-db/test
  :name "VivaceGraph test suite"
  :description "FiveAM unit tests for graph-db."
  :depends-on (:graph-db :fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "serialize-tests")
               (:file "geometry-tests")
               (:file "allocator-tests")
               (:file "linear-hash-tests")
               (:file "skip-list-tests")
               (:file "index-list-tests")
               (:file "type-index-tests")
               (:file "graph-tests")
               (:file "view-tests")
               (:file "query-tests")
               (:file "prolog-mutation-tests")
               (:file "prolog-functor-tests")
               (:file "traverse-tests")
               (:file "write-path-tests")
               (:file "reopen-tests")
               (:file "backup-tests")
               (:file "mvcc-tests")
               (:file "rest-tests")
               (:file "prolog-stress-tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :graph-db/test :run-tests)
                      (error "graph-db test suite failed."))))
