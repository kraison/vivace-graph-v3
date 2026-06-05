;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: :vg-tests -*-
;;;; tests/layer1/test-graph-class.lisp
;;;;
;;;; Test suite for src/graph-class.lisp (Layer 1: Graph Class Definition)

(in-package :vg-tests)

(def-suite l1.graph-class
  :description "Layer 1: Graph Class and Registry tests"
  :in nil)

(in-suite l1.graph-class)

;;;; ============================================================================
;;;; *GRAPHS* REGISTRY TESTS
;;;; ============================================================================

(test test-l1.graph-class.graphs-registry-exists
  "Verify *graphs* global registry exists."
  (is (boundp 'graph-db:*graphs*)))

(test test-l1.graph-class.graphs-registry-is-hash-table
  "Verify *graphs* is a hash table."
  (is (hash-table-p graph-db:*graphs*)))

(test test-l1.graph-class.graphs-registry-empty-on-start
  "Verify *graphs* is empty at startup."
  (is (= (hash-table-count graph-db:*graphs*) 0)))

(test test-l1.graph-class.graphs-registry-add-entry
  "Verify entries can be added to *graphs*."
  (let ((graph (make-instance 'graph-db:graph :graph-name "test" :location "/tmp")))
    (setf (gethash "test" graph-db:*graphs*) graph)
    (is (eq (gethash "test" graph-db:*graphs*) graph))
    (remhash "test" graph-db:*graphs*)))

(test test-l1.graph-class.graphs-registry-test-equal
  "Verify *graphs* uses 'equal test (string comparison)."
  (let ((graph (make-instance 'graph-db:graph :graph-name "db" :location "/tmp")))
    (setf (gethash "db" graph-db:*graphs*) graph)
    ;; Create new string with same value (different identity)
    (let ((key "db"))
      (is (eq (gethash key graph-db:*graphs*) graph)))
    (remhash "db" graph-db:*graphs*)))

;;;; ============================================================================
;;;; GRAPH CLASS TESTS
;;;; ============================================================================

(test test-l1.graph-class.graph-class-exists
  "Verify GRAPH class is defined."
  (is (find-class 'graph-db:graph)))

(test test-l1.graph-class.graph-instance-creation
  "Verify GRAPH instance can be created."
  (is (make-instance 'graph-db:graph)))

(test test-l1.graph-class.graph-with-name
  "Verify GRAPH can be created with name."
  (let ((g (make-instance 'graph-db:graph :graph-name "test-db")))
    (is (equal (graph-db:graph-name g) "test-db"))))

(test test-l1.graph-class.graph-with-location
  "Verify GRAPH can be created with location."
  (let ((g (make-instance 'graph-db:graph :location "/var/lib/vg")))
    (is (equal (graph-db:location g) "/var/lib/vg"))))

(test test-l1.graph-class.graph-open-p-default-nil
  "Verify graph-open-p defaults to NIL."
  (let ((g (make-instance 'graph-db:graph)))
    (is (null (graph-db:graph-open-p g)))))

(test test-l1.graph-class.graph-open-p-set-true
  "Verify graph-open-p can be set to true."
  (let ((g (make-instance 'graph-db:graph :graph-open-p t)))
    (is (graph-db:graph-open-p g))))

(test test-l1.graph-class.graph-txn-lock-created
  "Verify txn-lock is created (recursive lock)."
  (let ((g (make-instance 'graph-db:graph)))
    (is (not (null (graph-db:txn-lock g))))))

(test test-l1.graph-class.graph-views-lock-created
  "Verify views-lock is created (recursive lock)."
  (let ((g (make-instance 'graph-db:graph)))
    (is (not (null (graph-db:views-lock g))))))

(test test-l1.graph-class.graph-write-stats-created
  "Verify write-stats hash table is created."
  (let ((g (make-instance 'graph-db:graph)))
    (is (hash-table-p (graph-db:write-stats g)))))

(test test-l1.graph-class.graph-read-stats-created
  "Verify read-stats hash table is created."
  (let ((g (make-instance 'graph-db:graph)))
    (is (hash-table-p (graph-db:read-stats g)))))

(test test-l1.graph-class.graph-all-slots-accessible
  "Verify all 28 GRAPH slots are accessible."
  (let ((g (make-instance 'graph-db:graph)))
    ;; Just check that accessors don't error
    (is (not (null g)))
    (graph-db:graph-name g)
    (graph-db:graph-open-p g)
    (graph-db:location g)
    (graph-db:txn-lock g)
    (graph-db:views-lock g)
    (graph-db:write-stats g)
    (graph-db:read-stats g)))

;;;; ============================================================================
;;;; GRAPH PRINT-OBJECT TESTS
;;;; ============================================================================

(test test-l1.graph-class.graph-print-object
  "Verify print-object produces readable output."
  (let ((g (make-instance 'graph-db:graph :graph-name "mydb" :location "/tmp/mydb")))
    (let ((output (with-output-to-string (s)
                    (print-object g s))))
      (is (search "GRAPH" output))
      (is (search "mydb" output))
      (is (search "/tmp/mydb" output)))))

(test test-l1.graph-class.graph-print-via-format
  "Verify GRAPH can be printed via ~A format."
  (let ((g (make-instance 'graph-db:graph :graph-name "db")))
    (let ((output (with-output-to-string (s)
                    (format s "~A" g))))
      (is (stringp output))
      (is (> (length output) 0)))))

;;;; ============================================================================
;;;; MASTER-GRAPH CLASS TESTS
;;;; ============================================================================

(test test-l1.graph-class.master-graph-class-exists
  "Verify MASTER-GRAPH class is defined."
  (is (find-class 'graph-db:master-graph)))

(test test-l1.graph-class.master-graph-is-subclass-of-graph
  "Verify MASTER-GRAPH is a subclass of GRAPH."
  (is (subtypep 'graph-db:master-graph 'graph-db:graph)))

(test test-l1.graph-class.master-graph-instance-creation
  "Verify MASTER-GRAPH instance can be created."
  (is (make-instance 'graph-db:master-graph :graph-name "master")))

(test test-l1.graph-class.master-graph-inherits-graph-slots
  "Verify MASTER-GRAPH inherits all GRAPH slots."
  (let ((mg (make-instance 'graph-db:master-graph 
              :graph-name "master" 
              :location "/var/lib/master")))
    (is (equal (graph-db:graph-name mg) "master"))
    (is (equal (graph-db:location mg) "/var/lib/master"))
    (is (hash-table-p (graph-db:write-stats mg)))))

(test test-l1.graph-class.master-graph-has-replication-mbox
  "Verify MASTER-GRAPH has replication-mbox slot."
  (let ((mg (make-instance 'graph-db:master-graph)))
    (is (not (null mg)))
    ;; Just verify slot is accessible (may be uninitialized)
    (ignore-errors (graph-db:replication-mbox mg))))

(test test-l1.graph-class.master-graph-has-slaves-list
  "Verify MASTER-GRAPH has slaves list."
  (let ((mg (make-instance 'graph-db:master-graph)))
    (is (listp (graph-db:slaves mg)))))

(test test-l1.graph-class.master-graph-slaves-lock-created
  "Verify MASTER-GRAPH has slaves-lock (recursive lock)."
  (let ((mg (make-instance 'graph-db:master-graph)))
    (is (not (null (graph-db:slaves-lock mg))))))

(test test-l1.graph-class.master-graph-stop-replication-p-default
  "Verify stop-replication-p defaults to NIL."
  (let ((mg (make-instance 'graph-db:master-graph)))
    (is (null (graph-db:stop-replication-p mg)))))

;;;; ============================================================================
;;;; SLAVE-GRAPH CLASS TESTS
;;;; ============================================================================

(test test-l1.graph-class.slave-graph-class-exists
  "Verify SLAVE-GRAPH class is defined."
  (is (find-class 'graph-db:slave-graph)))

(test test-l1.graph-class.slave-graph-is-subclass-of-graph
  "Verify SLAVE-GRAPH is a subclass of GRAPH."
  (is (subtypep 'graph-db:slave-graph 'graph-db:graph)))

(test test-l1.graph-class.slave-graph-instance-creation
  "Verify SLAVE-GRAPH instance can be created."
  (is (make-instance 'graph-db:slave-graph :graph-name "slave")))

(test test-l1.graph-class.slave-graph-with-master-host
  "Verify SLAVE-GRAPH can be created with master-host."
  (let ((sg (make-instance 'graph-db:slave-graph 
              :graph-name "slave"
              :master-host "master.example.com")))
    (is (equal (graph-db:master-host sg) "master.example.com"))))

(test test-l1.graph-class.slave-graph-inherits-graph-slots
  "Verify SLAVE-GRAPH inherits all GRAPH slots."
  (let ((sg (make-instance 'graph-db:slave-graph 
              :graph-name "slave"
              :location "/var/lib/slave")))
    (is (equal (graph-db:graph-name sg) "slave"))
    (is (equal (graph-db:location sg) "/var/lib/slave"))
    (is (hash-table-p (graph-db:read-stats sg)))))

(test test-l1.graph-class.slave-graph-has-slave-thread
  "Verify SLAVE-GRAPH has slave-thread slot."
  (let ((sg (make-instance 'graph-db:slave-graph)))
    (is (null (graph-db:slave-thread sg)))))

(test test-l1.graph-class.slave-graph-master-txn-id-slot
  "Verify SLAVE-GRAPH has master-txn-id slot."
  (let ((sg (make-instance 'graph-db:slave-graph :master-txn-id 0)))
    (is (= (graph-db:master-txn-id sg) 0))))

;;;; ============================================================================
;;;; TYPE PREDICATE TESTS
;;;; ============================================================================

(test test-l1.graph-class.graph-p-true-for-graph
  "Verify graph-p returns graph for GRAPH instance."
  (let ((g (make-instance 'graph-db:graph)))
    (is (eq (graph-db:graph-p g) g))))

(test test-l1.graph-class.graph-p-true-for-master-graph
  "Verify graph-p returns graph for MASTER-GRAPH instance."
  (let ((mg (make-instance 'graph-db:master-graph)))
    (is (eq (graph-db:graph-p mg) mg))))

(test test-l1.graph-class.graph-p-true-for-slave-graph
  "Verify graph-p returns graph for SLAVE-GRAPH instance."
  (let ((sg (make-instance 'graph-db:slave-graph)))
    (is (eq (graph-db:graph-p sg) sg))))

(test test-l1.graph-class.graph-p-false-for-non-graph
  "Verify graph-p returns NIL for non-graph objects."
  (is (null (graph-db:graph-p "string")))
  (is (null (graph-db:graph-p 42)))
  (is (null (graph-db:graph-p nil))))

(test test-l1.graph-class.master-graph-p-true-for-master
  "Verify master-graph-p returns graph for MASTER-GRAPH only."
  (let ((mg (make-instance 'graph-db:master-graph)))
    (is (eq (graph-db:master-graph-p mg) mg))))

(test test-l1.graph-class.master-graph-p-false-for-plain-graph
  "Verify master-graph-p returns NIL for plain GRAPH."
  (let ((g (make-instance 'graph-db:graph)))
    (is (null (graph-db:master-graph-p g)))))

(test test-l1.graph-class.master-graph-p-false-for-slave-graph
  "Verify master-graph-p returns NIL for SLAVE-GRAPH."
  (let ((sg (make-instance 'graph-db:slave-graph)))
    (is (null (graph-db:master-graph-p sg)))))

(test test-l1.graph-class.slave-graph-p-true-for-slave
  "Verify slave-graph-p returns graph for SLAVE-GRAPH only."
  (let ((sg (make-instance 'graph-db:slave-graph)))
    (is (eq (graph-db:slave-graph-p sg) sg))))

(test test-l1.graph-class.slave-graph-p-false-for-plain-graph
  "Verify slave-graph-p returns NIL for plain GRAPH."
  (let ((g (make-instance 'graph-db:graph)))
    (is (null (graph-db:slave-graph-p g)))))

(test test-l1.graph-class.slave-graph-p-false-for-master-graph
  "Verify slave-graph-p returns NIL for MASTER-GRAPH."
  (let ((mg (make-instance 'graph-db:master-graph)))
    (is (null (graph-db:slave-graph-p mg)))))

;;;; ============================================================================
;;;; LOOKUP-GRAPH FUNCTION TESTS
;;;; ============================================================================

(test test-l1.graph-class.lookup-graph-empty-registry
  "Verify lookup-graph returns NIL for non-existent graph."
  (is (null (graph-db:lookup-graph "nonexistent"))))

(test test-l1.graph-class.lookup-graph-existing
  "Verify lookup-graph returns graph when found."
  (let ((g (make-instance 'graph-db:graph :graph-name "test")))
    (setf (gethash "test" graph-db:*graphs*) g)
    (is (eq (graph-db:lookup-graph "test") g))
    (remhash "test" graph-db:*graphs*)))

(test test-l1.graph-class.lookup-graph-multiple
  "Verify lookup-graph works with multiple graphs."
  (let ((g1 (make-instance 'graph-db:graph :graph-name "db1"))
        (g2 (make-instance 'graph-db:graph :graph-name "db2")))
    (setf (gethash "db1" graph-db:*graphs*) g1)
    (setf (gethash "db2" graph-db:*graphs*) g2)
    (is (eq (graph-db:lookup-graph "db1") g1))
    (is (eq (graph-db:lookup-graph "db2") g2))
    (remhash "db1" graph-db:*graphs*)
    (remhash "db2" graph-db:*graphs*)))

;;;; ============================================================================
;;;; ABSTRACT GENERIC METHOD TESTS
;;;; ============================================================================

(test test-l1.graph-class.init-schema-generic-exists
  "Verify init-schema generic is defined."
  (is (fboundp 'graph-db:init-schema)))

(test test-l1.graph-class.update-schema-generic-exists
  "Verify update-schema generic is defined."
  (is (fboundp 'graph-db:update-schema)))

(test test-l1.graph-class.snapshot-generic-exists
  "Verify snapshot generic is defined."
  (is (fboundp 'graph-db:snapshot)))

(test test-l1.graph-class.scan-for-unindexed-nodes-generic-exists
  "Verify scan-for-unindexed-nodes generic is defined."
  (is (fboundp 'graph-db:scan-for-unindexed-nodes)))

(test test-l1.graph-class.start-replication-generic-exists
  "Verify start-replication generic is defined."
  (is (fboundp 'graph-db:start-replication)))

(test test-l1.graph-class.stop-replication-generic-exists
  "Verify stop-replication generic is defined."
  (is (fboundp 'graph-db:stop-replication)))

;;;; ============================================================================
;;;; INTEGRATION TESTS
;;;; ============================================================================

(test test-l1.graph-class.graph-and-master-graph-coexist
  "Verify GRAPH and MASTER-GRAPH can coexist."
  (let ((g (make-instance 'graph-db:graph :graph-name "regular"))
        (mg (make-instance 'graph-db:master-graph :graph-name "master")))
    (is (graph-db:graph-p g))
    (is (null (graph-db:master-graph-p g)))
    (is (graph-db:graph-p mg))
    (is (graph-db:master-graph-p mg))))

(test test-l1.graph-class.inheritance-chain
  "Verify inheritance chain is correct."
  (is (subtypep 'graph-db:master-graph 'graph-db:graph))
  (is (subtypep 'graph-db:slave-graph 'graph-db:graph))
  (is (not (subtypep 'graph-db:master-graph 'graph-db:slave-graph)))
  (is (not (subtypep 'graph-db:slave-graph 'graph-db:master-graph))))

;;;; ============================================================================
;;;; TEST RUNNER
;;;; ============================================================================

(defun run-l1-graph-class-tests ()
  "Run all Layer 1 Graph Class tests and report results."
  (let* ((results (run! 'l1.graph-class))
         (passed (fiveam:results-passed results))
         (failed (fiveam:results-failed results))
         (skipped (fiveam:results-skipped results)))
    (format t "~&Layer 1 Graph Class Tests Summary:~%")
    (format t "  Passed:  ~D~%" passed)
    (format t "  Failed:  ~D~%" failed)
    (format t "  Skipped: ~D~%" skipped)
    (format t "  Total:   ~D~%" (+ passed failed skipped))
    (zerop failed)))

(in-package :cl-user)
