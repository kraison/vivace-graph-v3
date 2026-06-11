;;;; Capstone: close a populated graph and reopen it from disk, verifying that
;;;; data, slot values, adjacency and type information survive the round trip.
;;;; This exercises the full on-disk stack end to end (heap, vertex/edge
;;;; tables, ve indexes, type index, schema.dat) and the mmap file-mode fix
;;;; that makes reopening possible at all.
;;;;
;;;; Reuses the g-person / g-knows schema from graph-tests.lisp.

(in-package #:graph-db/test)

(def-suite reopen-suite
  :description "close-graph then open-graph preserves data."
  :in graph-db-suite)

(in-suite reopen-suite)

(test reopen-preserves-data-and-adjacency
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          aid bid)
      ;; --- populate, then close ---
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (let ((a (make-g-person :name "Persist" :age 7))
                  (b (make-g-person :name "Other")))
              (setq aid (id a) bid (id b))
              (make-g-knows :from a :to b :weight 3.0)))
          (close-graph g :snapshot-p nil)))
      ;; --- reopen the same directory ---
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               ;; vertex + slot values survived
               (let ((a (lookup-vertex aid)))
                 (is-true a)
                 (is (string= "Persist" (slot-value a 'name)))
                 (is (= 7 (slot-value a 'age)))
                 ;; adjacency (ve index) survived, with endpoints + weight
                 (let ((outs (outgoing-edges a)))
                   (is (= 1 (length outs)))
                   (is (equalp bid (to (first outs))))
                   (is (= 3.0 (weight (first outs))))))
               ;; type information survived: typed lookup and type query work
               (is-true (lookup-g-person aid))
               (is (= 2 (length (map-vertices #'identity g2 :collect-p t
                                                       :vertex-type 'g-person)))))
          (close-graph g2 :snapshot-p nil)
          (collect-garbage))))))

(test reopen-after-snapshot-close
  "A snapshotting (default) close also reopens cleanly."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-g-person :name "Snapshotted"))))
          (close-graph g)))              ; snapshot-p t (default)
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is (string= "Snapshotted" (slot-value (lookup-vertex id) 'name))))
          (close-graph g2 :snapshot-p nil)
          (collect-garbage))))))

(test schema-class-locks-restored-on-reopen
  "Regression for issue #32: after open-graph, the per-class rw-locks are rebuilt
(schema-class-locks is not nil), so the class locks work -- new nodes can be
created post-reopen (make-* goes through with-write-locked-class).  Locks are
not persisted; restore-schema-locks recreates them from the restored types."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction () (setq id (id (make-g-person :name "Before")))))
        (close-graph g :snapshot-p nil))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is-true (graph-db::schema-class-locks (graph-db::schema g2))
                        "schema-class-locks must be rebuilt (non-nil) after reopen")
               (is-true (gethash 'g-person
                                 (graph-db::schema-class-locks (graph-db::schema g2)))
                        "the restored g-person class must have a lock")
               ;; practical effect: creating a node post-reopen needs the class lock
               (with-transaction () (make-g-person :name "After"))
               (is (= 2 (length (map-vertices #'identity g2 :collect-p t
                                                        :vertex-type 'g-person)))
                   "a new node can be created after reopen")
               (is-true (lookup-vertex id) "pre-existing data intact"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
