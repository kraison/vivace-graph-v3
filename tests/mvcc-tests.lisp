;;;; MVCC tests (the v2 node head + later the versioned write path / reaper).
;;;;
;;;; Phase 1: the head format gained commit-epoch (8) + prev-pointer (8),
;;;; appended after data-pointer (15 -> 31 bytes).  These tests serialize a
;;;; node head to a byte buffer and read it back, asserting the new fields
;;;; round-trip for both vertices and edges.

(in-package #:graph-db/test)

(def-suite mvcc-suite
  :description "MVCC: v2 node-head codec (commit-epoch + prev-pointer), versioning."
  :in graph-db-suite)

(in-suite mvcc-suite)

(test node-head-v2-round-trip-vertex
  "A vertex head round-trips revision, data-pointer, commit-epoch and prev-pointer
through serialize-node-head / deserialize-node-head (31-byte v2 head)."
  (with-test-graph (g)
    (declare (ignore g))
    (let (v)
      (with-transaction () (setq v (make-g-person :name "H" :age 3)))
      (setf (graph-db::revision v)     7
            (graph-db::data-pointer v) 4242
            (graph-db::commit-epoch v) 123456789
            (graph-db::prev-pointer v) 987654321)
      (let ((buf (graph-db::make-byte-vector graph-db::+node-header-size+)))
        (is (= 31 graph-db::+node-header-size+))
        (is (= 30 (graph-db::serialize-node-head buf v 0))
            "serialize returns the final offset (31-byte head ends at 30)")
        (multiple-value-bind (del wr hw tiw vw vew vvw type-id rev ptr epoch prev offset)
            (graph-db::deserialize-node-head buf 0)
          (declare (ignore del wr hw tiw vw vew vvw))
          (is (= (graph-db::type-id v) type-id))
          (is (= 7 rev))
          (is (= 4242 ptr))
          (is (= 123456789 epoch) "commit-epoch round-trips")
          (is (= 987654321 prev) "prev-pointer round-trips")
          (is (= 30 offset)))))))

(test node-head-v2-round-trip-edge
  "An edge head round-trips its from/to/weight AND the new commit-epoch /
prev-pointer (the edge codec positions from/to/weight after the node head, so it
auto-shifts with the larger v2 head)."
  (with-test-graph (g)
    (declare (ignore g))
    (let (e aid bid)
      (with-transaction ()
        (let ((a (make-g-person :name "A")) (b (make-g-person :name "B")))
          (setq aid (id a) bid (id b))
          (setq e (make-g-knows :from a :to b :weight 2.5))))
      (setf (graph-db::revision e)     4
            (graph-db::commit-epoch e) 555
            (graph-db::prev-pointer e) 666)
      (let ((buf (graph-db::make-byte-vector graph-db::+edge-header-size+)))
        (is (= 71 graph-db::+edge-header-size+))
        (graph-db::serialize-edge-head buf e 0)
        (let ((e2 (graph-db::deserialize-edge-head buf 0)))
          (is (= 4 (graph-db::revision e2)))
          (is (= 555 (graph-db::commit-epoch e2)) "edge commit-epoch round-trips")
          (is (= 666 (graph-db::prev-pointer e2)) "edge prev-pointer round-trips")
          (is (equalp aid (from e2)))
          (is (equalp bid (to e2)))
          (is (= 2.5 (weight e2))))))))
