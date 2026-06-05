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

(defun find-nodes-within (area &key (graph *graph*))
  "List of live nodes whose geometry's representative point lies within AREA
(a :POLYGON or :MULTIPOLYGON geometry)."
  (let ((idx (spatial-index graph)) (result '()))
    (when (and idx (geometryp area))
      (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox area)
        (dolist (id (spatial-index-query-bbox idx min-lon min-lat max-lon max-lat))
          (let ((node (%node-by-id id graph)))
            (when (and node (not (deleted-p node)))
              (let ((geom (node-geometry node)))
                (when geom
                  (multiple-value-bind (lat lon) (%geometry-rep-point geom)
                    (when (geometry-contains-point-p area lon lat)
                      (push node result))))))))))
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
