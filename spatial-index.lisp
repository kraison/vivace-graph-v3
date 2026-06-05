(in-package :graph-db)

;;; Geohash spatial index for VivaceGraph (public, general-purpose).
;;;
;;; A skip list keyed by fixed-precision geohash strings maps grid cells to the
;;; ids of nodes whose geometry occupies them.  Because every key has the same
;;; precision, a cell lookup is an exact key match (duplicates allowed: many
;;; nodes per cell, and an extended geometry occupies several cells).
;;;
;;; Queries are a FILTER: they return the candidate node ids whose cells meet
;;; the query window; the caller REFINES with the exact predicates in
;;; geometry-ops.lisp (point-in-polygon, geodesic-distance).  This is the
;;; standard filter/refine design and keeps the index independent of node
;;; semantics -- it stores ids and geometry only.
;;;
;;; The skip list lives in a caller-supplied MEMORY (heap); its root address
;;; (SPATIAL-INDEX-ADDRESS) is what a host persists to reopen the index, exactly
;;; as views persist their skip-list pointer.

(defstruct (spatial-index (:constructor %make-spatial-index) (:predicate spatial-index-p))
  skip-list
  heap
  (precision 7 :type (integer 1 12)))

;; Geohash uses only the base32 alphabet (max char #\z), so "" sorts before and
;; "{" (#\{ = 123) sorts after every possible key.
(alexandria:define-constant +spatial-min-key+ "" :test 'string=)
(alexandria:define-constant +spatial-max-key+ "{" :test 'string=)

;; Values are node ids -- 16-byte (unsigned-byte 8) uuid arrays.  The generic
;; SERIALIZE passes ub8 vectors through raw and untagged, which DESERIALIZE
;; cannot reverse, so we store ids as opaque bytes: the skip-node records each
;; value's length, so an identity codec round-trips them exactly.
(defun %spatial-make-sl (heap)
  (make-skip-list :heap heap
                  :key-equal 'string=
                  :key-comparison 'string<
                  :head-key +spatial-min-key+ :head-value +null-key+
                  :tail-key +spatial-max-key+ :tail-value +max-key+
                  :duplicates-allowed-p t
                  :value-equal 'equalp
                  :key-serializer 'serialize
                  :key-deserializer 'deserialize
                  :value-serializer 'identity
                  :value-deserializer 'identity))

(defun make-spatial-index (heap &key (precision 7))
  "Create a new spatial index in HEAP (a MEMORY).  PRECISION sets the geohash
grid resolution (7 ~ 150 m cells, 9 ~ 5 m)."
  (%make-spatial-index :skip-list (%spatial-make-sl heap)
                       :heap heap :precision precision))

(defun open-spatial-index (heap address &key (precision 7))
  "Reopen the spatial index whose skip list is rooted at ADDRESS in HEAP.
PRECISION must match the value used at creation."
  (%make-spatial-index
   :skip-list (open-skip-list :address address :heap heap
                              :key-equal 'string= :key-comparison 'string<
                              :duplicates-allowed-p t :value-equal 'equalp
                              :key-serializer 'serialize :key-deserializer 'deserialize
                              :value-serializer 'identity :value-deserializer 'identity)
   :heap heap :precision precision))

(defun spatial-index-address (idx)
  "Root heap address of IDX's skip list -- persist this to reopen the index."
  (%sl-address (spatial-index-skip-list idx)))

(defun delete-spatial-index (idx)
  "Free the index's skip list from its heap."
  (delete-skip-list (spatial-index-skip-list idx)))

(defun %bbox-cells (geom precision)
  (multiple-value-bind (min-lon min-lat max-lon max-lat) (geometry-bbox geom)
    (geohash-covering min-lon min-lat max-lon max-lat :precision precision)))

(defun %geometry-cells (geom precision)
  "The geohash cells (strings) GEOM occupies at PRECISION.  A point yields one
cell; a polygon/linestring yields the grid over its bbox.  A multipolygon is
covered PART BY PART (not by one overall bbox) so the empty gaps between
separated parts -- e.g. a city-scale task area -- are not indexed."
  (if (eq (geometry-kind geom) :multipolygon)
      (let ((seen (make-hash-table :test 'equal)) (cells '()))
        (dolist (poly (geometry-coordinates geom) cells)
          (dolist (c (%bbox-cells (%make-geometry :kind :polygon :coordinates poly)
                                  precision))
            (unless (gethash c seen)
              (setf (gethash c seen) t)
              (push c cells)))))
      (%bbox-cells geom precision)))

(defun spatial-index-insert (idx node-id geom)
  "Index NODE-ID under every cell GEOM occupies.  NODE-ID is any serializable
identifier (e.g. a node's uuid)."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)) node-id)
      (add-to-skip-list sl cell node-id))))

(defun spatial-index-remove (idx node-id geom)
  "Remove NODE-ID's entries for GEOM (using the same cells INSERT produced)."
  (let ((sl (spatial-index-skip-list idx)))
    (dolist (cell (%geometry-cells geom (spatial-index-precision idx)))
      (remove-from-skip-list sl cell node-id))))

(defun spatial-index-query-bbox (idx min-lon min-lat max-lon max-lat)
  "Candidate node-ids whose indexed cells meet the query bounding box.  A
cell-granular FILTER -- refine with exact geometry predicates."
  (let ((sl (spatial-index-skip-list idx))
        (seen (make-hash-table :test 'equalp))
        (result '()))
    (dolist (cell (geohash-covering min-lon min-lat max-lon max-lat
                                    :precision (spatial-index-precision idx)))
      (dolist (nid (skip-list-fetch-all sl cell))
        (unless (gethash nid seen)
          (setf (gethash nid seen) t)
          (push nid result))))
    result))

(defun spatial-index-query-radius (idx lat lon radius-m)
  "Candidate node-ids within ~RADIUS-M metres of (LAT, LON), via a bounding-box
prefilter.  Refine with GEODESIC-DISTANCE for an exact radius."
  (let* ((dlat (/ radius-m 111320d0))
         (dlon (/ radius-m (* 111320d0 (max 0.01d0 (cos (deg->rad lat)))))))
    (spatial-index-query-bbox idx (- lon dlon) (- lat dlat) (+ lon dlon) (+ lat dlat))))
