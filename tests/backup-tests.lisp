;;;; Manual snapshot + replay (restore-from-snapshot) round trip.
;;;;
;;;; Exercises the high-level backup path: SNAPSHOT writes a snap-<ts> file (one
;;;; s-expression per node) under the graph's txn-log/ dir, and REPLAY rebuilds a
;;;; brand-new empty graph from the newest snapshot in a directory -- restoring
;;;; nodes, slot values, edge endpoints/weight and (preserved) ids, then
;;;; regenerating views.  Reuses the g-person / g-knows schema from
;;;; graph-tests.lisp.

(in-package #:graph-db/test)

(def-suite backup-suite
  :description "Manual snapshot and replay (restore-from-snapshot)."
  :in graph-db-suite)

(in-suite backup-suite)

(test snapshot-and-replay-round-trip
  "Snapshot a populated graph, then replay it into a fresh empty graph in a new
directory: vertex count, slot values, edge endpoints/weight and node ids all
survive the round trip."
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let ((p1 (namestring dir1)) (p2 (namestring dir2)) aid bid)
        ;; --- populate the source graph and take a MANUAL snapshot ---
        (let ((g (make-graph *integration-graph-name* p1 :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (let ((a (make-g-person :name "Snap" :age 42))
                    (b (make-g-person :name "Shot" :age 7)))
                (setq aid (id a) bid (id b))
                (make-g-knows :from a :to b :weight 2.5)))
            ;; SNAPSHOT writes <p1>/txn-log/snap-<ts>; it relies on *graph* (the
            ;; backup walk uses map-vertices/map-edges over *graph*).
            (let ((result (graph-db:snapshot g)))
              (is (integerp result)
                  "snapshot should return a node count, not ~S" result)
              (is (= 3 result)
                  "expected 3 nodes snapshotted (2 vertices + 1 edge); got ~A"
                  result)))
          (close-graph g :snapshot-p nil))   ; manual snapshot already on disk
        ;; --- replay into a brand-new EMPTY graph in a different directory ---
        (let ((g2 (make-graph *integration-graph-name* p2 :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1)
                                  :graph-db/test)
                 ;; both vertices restored
                 (is (= 2 (length (map-vertices #'identity g2 :collect-p t
                                                          :vertex-type 'g-person)))
                     "expected 2 restored g-person vertices")
                 ;; ids preserved + slot values intact
                 (let ((a (lookup-vertex aid)))
                   (is-true a "source vertex ~A was not restored" aid)
                   (is (string= "Snap" (slot-value a 'name)))
                   (is (= 42 (slot-value a 'age)))
                   ;; edge + adjacency restored, with endpoints and weight
                   (let ((outs (outgoing-edges a)))
                     (is (= 1 (length outs)) "expected 1 outgoing edge")
                     (is (equalp bid (to (first outs)))
                         "restored edge points to the wrong vertex")
                     (is (= 2.5 (weight (first outs))))))
                 (is-true (lookup-vertex bid) "target vertex ~A was not restored" bid))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))
