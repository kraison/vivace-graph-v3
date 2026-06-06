(in-package :graph-db)

;;; Index-backed spatial queries (public spatial extension).
;;;
;;; These hit the graph's spatial index for candidate node ids, resolve them to
;;; live nodes, and refine with the exact geometry-ops predicates using each
;;; node's NODE-GEOMETRY.  A node is matched by its representative point: the
;;; point itself for a :POINT geometry, the bounding-box centre otherwise (so
;;; the EO-find-in-task-area case is exact; extended geometries are approximate).
;;;
;;; Both a Lisp API (FIND-NODES-WITHIN / FIND-NODES-NEAR) and Prolog functors
;;; (FIND-WITHIN/2, FIND-NEAR/4) are provided; the functors yield matching nodes
;;; so they compose with graph traversal in a query, e.g.:
;;;   (select-flat (?f) (is-a ?f eo-find) (find-near ?f 49.20 37.17 500.0))

(defun %node-by-id (id graph)
  "Resolve a spatial-index id (uuid bytes) to its live node, or NIL."
  (or (lookup-vertex id :graph graph)
      (lookup-edge id :graph graph)))

(defun %geometry-rep-point (geom)
  "Representative (values lat lon) for GEOM: the point itself for a :POINT,
otherwise the centre of its bounding box."
  (if (eq (geometry-kind geom) :point)
      (values (geometry-lat geom) (geometry-lon geom))
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox geom)
        (values (/ (+ min-lat max-lat) 2) (/ (+ min-lon max-lon) 2)))))

(defun %node-within-area-p (area geom)
  "True if node geometry GEOM lies within AREA.  Exact for a :POINT (always) and
-- with the graph-db/geos add-on -- for extended geometries too.  Without GEOS,
extended geometries fall back to the representative-point approximation (the
historical behaviour), so results are unchanged when the add-on is absent."
  (if (eq (geometry-kind geom) :point)
      (geometry-contains-point-p area (geometry-lon geom) (geometry-lat geom))
      (if *geos-available-p*
          (geometry-contains-geometry-p area geom)
          (multiple-value-bind (lat lon) (%geometry-rep-point geom)
            (geometry-contains-point-p area lon lat)))))

(defun find-nodes-within (area &key (graph *graph*))
  "List of live nodes whose geometry lies within AREA (a :POLYGON or
:MULTIPOLYGON geometry).  A :POINT node is judged exactly; an extended-geometry
node is judged exactly when graph-db/geos is loaded, otherwise by its
representative point (bbox centre)."
  (let ((idx (spatial-index graph)) (result '()))
    (when (and idx (geometryp area))
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (dolist (id (spatial-index-query-bbox idx min-lon min-lat max-lon max-lat))
          (let ((node (%node-by-id id graph)))
            (when (and node (not (deleted-p node)))
              (let ((geom (node-geometry node)))
                (when (and geom (%node-within-area-p area geom))
                  (push node result))))))))
    (nreverse result)))

(defun find-nodes-intersecting (area &key (graph *graph*))
  "List of live nodes whose geometry INTERSECTS AREA (any geometry kind).  Exact
with the graph-db/geos add-on; without it, extended-geometry candidates use a
COARSE bounding-box overlap test (point candidates are always exact)."
  (let ((idx (spatial-index graph)) (result '()))
    (when (and idx (geometryp area))
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (dolist (id (spatial-index-query-bbox idx min-lon min-lat max-lon max-lat))
          (let ((node (%node-by-id id graph)))
            (when (and node (not (deleted-p node)))
              (let ((geom (node-geometry node)))
                (when (and geom (geometry-intersects-p area geom))
                  (push node result))))))))
    (nreverse result)))

(defun find-nodes-near (lat lon radius &key (graph *graph*))
  "List of (NODE . DISTANCE-METRES) for live nodes within RADIUS of (LAT, LON),
nearest first."
  (let ((idx (spatial-index graph)) (result '()))
    (when (and idx (numberp lat) (numberp lon) (numberp radius))
      (dolist (id (spatial-index-query-radius idx lat lon radius))
        (let ((node (%node-by-id id graph)))
          (when (and node (not (deleted-p node)))
            (let ((geom (node-geometry node)))
              (when geom
                (multiple-value-bind (nlat nlon) (%geometry-rep-point geom)
                  (let ((d (geodesic-distance lat lon nlat nlon)))
                    (when (<= d radius)
                      (push (cons node d) result))))))))))
    (sort result #'< :key #'cdr)))

(def-global-prolog-functor find-within/2 (?node ?area cont)
  "Yield each indexed node whose geometry lies within the bound :POLYGON or
:MULTIPOLYGON ?AREA."
  (let ((node-var (var-deref ?node))
        (area (var-deref ?area)))
    (when (geometryp area)
      (dolist (node (find-nodes-within area :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var node)
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor find-intersects/2 (?node ?area cont)
  "Yield each indexed node whose geometry intersects the bound ?AREA geometry."
  (let ((node-var (var-deref ?node))
        (area (var-deref ?area)))
    (when (geometryp area)
      (dolist (node (find-nodes-intersecting area :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var node)
            (funcall cont))
          (undo-bindings old-trail))))))

(def-global-prolog-functor find-near/4 (?node ?lat ?lon ?radius cont)
  "Yield each indexed node within ?RADIUS metres of (?LAT, ?LON)."
  (let ((node-var (var-deref ?node))
        (lat (var-deref ?lat)) (lon (var-deref ?lon)) (radius (var-deref ?radius)))
    (when (and (numberp lat) (numberp lon) (numberp radius))
      (dolist (nd (find-nodes-near lat lon radius :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var (car nd))
            (funcall cont))
          (undo-bindings old-trail))))))

(defun find-nearest-k (lat lon k &key (graph *graph*) (max-radius 2.5d4))
  "List of (NODE . DISTANCE-METRES) for the K nodes nearest (LAT, LON), nearest
first (fewer than K if the graph holds fewer indexed nodes within MAX-RADIUS).

Correctness: FIND-NODES-NEAR returns every node within a given radius sorted by
distance, so once a radius encloses at least K nodes, those K are the global K
nearest -- anything outside the radius is farther than everything inside it.  We
start from one grid cell's size and double the radius until K are enclosed (or
MAX-RADIUS is reached), then keep the K closest.

MAX-RADIUS is a deliberate bound (default 25 km): kNN is \"K nearest within
MAX-RADIUS\".  Each widening re-runs the window query, whose cost grows with the
number of indexed nodes the window encloses (the bbox query covers a window with
a bounded set of coarse cells and range-scans them, so empty space is free);
widen MAX-RADIUS only if you accept scanning the larger candidate set."
  (let ((idx (spatial-index graph)))
    (when (and idx (numberp lat) (numberp lon) (integerp k) (plusp k))
      (let* ((prec (spatial-index-precision idx))
             ;; seed radius: the index cell's latitude extent in metres
             (r (max 1d0 (* (nth-value 1 (geohash-cell-size prec)) 111320d0)))
             (found '()))
        (loop
          (setf found (find-nodes-near lat lon r :graph graph))
          (when (or (>= (length found) k) (>= r max-radius))
            (return))
          (setf r (min max-radius (* r 2d0))))
        (subseq found 0 (min k (length found)))))))

(def-global-prolog-functor find-nearest/4 (?node ?lat ?lon ?k cont)
  "Yield each of the ?K nodes nearest (?LAT, ?LON), nearest first."
  (let ((node-var (var-deref ?node))
        (lat (var-deref ?lat)) (lon (var-deref ?lon)) (k (var-deref ?k)))
    (when (and (numberp lat) (numberp lon) (integerp k))
      (dolist (nd (find-nearest-k lat lon k :graph *graph*))
        (let ((old-trail (fill-pointer *trail*)))
          (when (unify node-var (car nd))
            (funcall cont))
          (undo-bindings old-trail))))))

(defun make-spatial-replication-filter (area)
  "Return a predicate (NODE) -> generalized boolean for use as a slave graph's
REPLICATION-FILTER (see MAKE-GRAPH :replication-filter).  It accepts a node when
it has no geometry (so non-spatial data -- schema, reference data -- replicates
in full) or when its geometry's representative point lies within AREA (a
:polygon / :multipolygon).  A field slave then receives only the nodes for its
area of operations, plus all non-spatial nodes."
  (lambda (node)
    (let ((geom (node-geometry node)))
      (or (null geom)
          (multiple-value-bind (lat lon) (%geometry-rep-point geom)
            (geometry-contains-point-p area lon lat))))))

(defun rebuild-spatial-index (graph &key precision)
  "Rebuild GRAPH's spatial index from scratch: drop the current index, create a
fresh one, and re-index every live node that has a NODE-GEOMETRY.  Returns the
number of nodes indexed.

Use this to change the grid PRECISION (otherwise the current precision is kept),
to adopt the index on a graph that predates the spatial extension, or to repair
it.  It mutates the index directly (outside the transaction write path), so run
it when the graph is quiescent -- analogous to REGENERATE-VIEW."
  (let ((prec (or precision
                  (and (spatial-index-p (spatial-index graph))
                       (spatial-index-precision (spatial-index graph)))
                  7)))
    (with-recursive-lock-held ((txn-lock graph))
      (when (spatial-index-p (spatial-index graph))
        (delete-spatial-index (spatial-index graph)))
      (init-spatial-index graph :precision prec)   ; fresh index + persisted sidecar
      (let ((idx (spatial-index graph)) (count 0))
        (flet ((reindex (node)
                 (unless (deleted-p node)
                   (let ((geom (node-geometry node)))
                     (when geom
                       (spatial-index-insert idx (id node) geom)
                       (incf count))))))
          (map-vertices (lambda (v) (reindex v)) graph)
          (map-edges (lambda (e) (reindex e)) graph))
        count))))
