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

;;; ---------------------------------------------------------------------------
;;; v1 -> v2 migration (MIGRATE-GRAPH)
;;;
;;; tests/fixtures/v1-graph.tar.gz is a pristine pre-MVCC (storage-version 1,
;;; 15-byte head) graph built on the experiment branch with this same test
;;; schema (g-person/g-employee/g-knows/g-likes): Alice(30) -knows-> Bob(25),
;;; Bob -knows-> Carol(40, employee "Boss"), Alice -likes-> Carol.  The heap is a
;;; 1 GB sparse file, so it ships tar+gzipped (~13 KB of zero-fill) and the test
;;; extracts it to a scratch dir.
;;; ---------------------------------------------------------------------------

(defun extract-v1-fixture (dest)
  "Extract the committed v1 graph fixture into DEST (created if needed); return
DEST as a string."
  (let ((tarball (asdf:system-relative-pathname
                  :graph-db/test "tests/fixtures/v1-graph.tar.gz")))
    (ensure-directories-exist dest)
    (uiop:run-program (list "tar" "xzf" (namestring tarball)
                            "-C" (namestring dest))
                      :output t :error-output t)
    (namestring dest)))

(test migrate-v1-graph-to-v2
  "A pre-MVCC (v1, 15-byte head) graph cannot be opened directly by v2 code but
MIGRATE-GRAPH carries it across (logical snapshot + replay), preserving every
node, its slot data, the subclass, and the edge topology."
  (with-temp-directory (root)
    (let ((old-dir (extract-v1-fixture (merge-pathnames "v1/" root)))
          (new-dir (namestring (merge-pathnames "v2/" root))))
      ;; v2 code refuses to open the v1 graph directly (the format gate).
      (signals error (graph-db:open-graph :mvcc-mig-guard old-dir
                                          :buffer-pool-p nil :gc-heap-p nil))
      ;; ...but MIGRATE-GRAPH brings it forward to v2.
      (let ((g (graph-db::migrate-graph :graph-db-mvcc-migration old-dir new-dir
                                        :package :graph-db/test)))
        (unwind-protect
             (let ((*graph* g))
               (is (= 2 graph-db::+storage-version+)
                   "the migrated graph is written in the current (v2) format")
               ;; All three people survive, with name + age intact.  :vertex-type
               ;; g-person includes the g-employee subclass (Carol) by default.
               (let ((people (sort (map-vertices
                                    (lambda (v) (list (slot-value v 'name)
                                                      (slot-value v 'age)))
                                    g :collect-p t :vertex-type 'g-person)
                                   #'string< :key #'car)))
                 (is (equal '(("Alice" 30) ("Bob" 25) ("Carol" 40)) people)))
               ;; Carol's subclass + extra slot round-tripped.
               (let ((titles (map-vertices (lambda (v) (slot-value v 'title))
                                           g :collect-p t :vertex-type 'g-employee)))
                 (is (equal '("Boss") titles)))
               ;; Edge topology: two g-knows, one g-likes.
               (is (= 2 (length (map-edges #'identity g
                                           :collect-p t :edge-type 'g-knows))))
               (is (= 1 (length (map-edges #'identity g
                                           :collect-p t :edge-type 'g-likes)))))
          (graph-db:close-graph g :snapshot-p nil))))))
