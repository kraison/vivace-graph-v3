;;;; graph-db/algorithms -- shared infrastructure for the graph-algorithm port.
;;;;
;;;; Part of the OPTIONAL `graph-db/algorithms' add-on.  These are analysis
;;;; algorithms ported from the standalone graph-utils library onto VivaceGraph's
;;;; persistent, MVCC object model.  Two execution modes (see the port plan):
;;;;
;;;;   * Mode B ("native"): streaming algorithms that run directly against the
;;;;     ve/vev adjacency indexes under a read snapshot, keyed by node UUID.
;;;;     Shortest-path, BFS, components, PageRank, HITS, degree.  Never mutate
;;;;     the persistent store.
;;;;
;;;;   * Mode A ("projection"): inherently in-memory algorithms (all-pairs,
;;;;     max-flow, clustering) that materialize a compact integer-indexed copy of
;;;;     the graph (see projection.lisp) and run vendored graph-utils code on it.
;;;;
;;;; This file holds the bits both modes share: the snapshot contract, node-
;;;; designator coercion, and weighted-adjacency iteration.
;;;;
;;;; INVARIANT: read-only algorithms run inside WITH-ALGORITHM-SNAPSHOT for a
;;;; consistent MVCC view; no algorithm mutates the persistent graph (mutating
;;;; algorithms operate only on an in-memory projection).

(in-package :graph-db)

;;; ------------------------------------------------------------------
;;; Snapshot contract
;;; ------------------------------------------------------------------

(defmacro with-algorithm-snapshot ((&optional (graph '*graph*)) &body body)
  "Evaluate BODY with reads pinned to a single consistent MVCC snapshot of GRAPH.
A thin alias over WITH-READ-SNAPSHOT establishing the read-only contract every
Mode-B algorithm runs under.  Inherits an enclosing transaction/snapshot if one
is already active."
  `(with-read-snapshot (,graph) ,@body))

;;; ------------------------------------------------------------------
;;; Node-designator coercion
;;;
;;; A "node designator" accepted by the public API is one of: a VERTEX object,
;;; its id (a 16-byte key vector), or its id rendered as a string.  Internally we
;;; key all working hash tables by the id vector under :TEST 'EQUALP -- the
;;; portable choice (ECL forbids custom hash-table tests) and the same one
;;; TRAVERSE already uses.
;;; ------------------------------------------------------------------

(defun algorithm-vertex (designator &optional (graph *graph*))
  "Coerce a node DESIGNATOR (vertex, id vector, or id string) to a VERTEX in
GRAPH.  Returns NIL if no such vertex exists."
  (typecase designator
    (vertex designator)
    (t (lookup-vertex designator :graph graph))))

(declaim (inline algorithm-node-key))
(defun algorithm-node-key (vertex)
  "The EQUALP hash key identifying VERTEX across the algorithm working tables --
its node id (UUID) vector."
  (id vertex))

;;; ------------------------------------------------------------------
;;; Weighted adjacency
;;; ------------------------------------------------------------------

(defun adjacent-vertices (vertex &key (graph *graph*) (direction :out) edge-type
                                   (weight-fn #'weight) unweighted)
  "Return a list of (NEIGHBOR-VERTEX . WEIGHT) cells adjacent to VERTEX in GRAPH.

DIRECTION is :OUT (follow edges whose FROM is VERTEX -- the default), :IN (edges
whose TO is VERTEX), or :BOTH (treat the graph as undirected).  EDGE-TYPE
restricts to a single edge type.  WEIGHT is taken from each edge via WEIGHT-FN
\(default #'WEIGHT); with UNWEIGHTED, every edge contributes weight 1.

Runs against the live ve/vev indexes via OUTGOING-EDGES / INCOMING-EDGES, so it
must be called inside a snapshot for a consistent view (see
WITH-ALGORITHM-SNAPSHOT)."
  (let ((result nil))
    (when (or (eq direction :out) (eq direction :both))
      (dolist (e (outgoing-edges vertex :graph graph :edge-type edge-type))
        (let ((nv (lookup-vertex (to e) :graph graph)))
          (when nv
            (push (cons nv (if unweighted 1 (funcall weight-fn e))) result)))))
    (when (or (eq direction :in) (eq direction :both))
      (dolist (e (incoming-edges vertex :graph graph :edge-type edge-type))
        (let ((nv (lookup-vertex (from e) :graph graph)))
          (when nv
            (push (cons nv (if unweighted 1 (funcall weight-fn e))) result)))))
    (nreverse result)))

(defun all-vertices (&optional (graph *graph*) vertex-type)
  "Collect every (live) vertex of GRAPH, optionally restricted to VERTEX-TYPE."
  (map-vertices 'identity graph :collect-p t :vertex-type vertex-type))
