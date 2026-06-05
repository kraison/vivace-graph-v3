(in-package :graph-db)

;;; Geometry refine operations for the spatial index.
;;;
;;; Public, general-purpose, dependency-free predicates used to refine the
;;; geohash index's candidate set into exact answers: great-circle distance
;;; (haversine), point-in-polygon (ray casting, holes honoured), and
;;; bounding-box overlap.  Heavier topology (intersection, union, validity
;;; repair) is delegated to GEOS via cl-geos in a later, optional module.

(alexandria:define-constant +earth-radius-m+ 6371000d0 :test '=)
(alexandria:define-constant +radians-per-degree+
    #.(coerce (/ pi 180) 'double-float) :test '=)

(declaim (inline deg->rad))
(defun deg->rad (d)
  (* (coerce d 'double-float) +radians-per-degree+))

(defun geodesic-distance (lat1 lon1 lat2 lon2)
  "Great-circle distance in metres between two WGS84 points (haversine).
Accurate to ~0.5% vs the ellipsoid -- ample at minefield scale; a Vincenty or
Karney method can replace this later if sub-metre accuracy over long lines is
needed."
  (let* ((phi1 (deg->rad lat1))
         (phi2 (deg->rad lat2))
         (dphi (deg->rad (- lat2 lat1)))
         (dlam (deg->rad (- lon2 lon1)))
         (a (+ (expt (sin (/ dphi 2)) 2)
               (* (cos phi1) (cos phi2) (expt (sin (/ dlam 2)) 2)))))
    (* 2d0 +earth-radius-m+ (atan (sqrt a) (sqrt (- 1d0 a))))))

(defun point-in-ring-p (lon lat ring)
  "True if (LON, LAT) lies inside RING -- a list of (lon lat) -- by the
even-odd ray-casting rule (Franklin's PNPOLY).  Points exactly on the boundary
are resolved consistently rather than specially flagged."
  (let ((n (length ring)))
    (when (< n 3) (return-from point-in-ring-p nil))
    (let ((v (coerce ring 'vector)) (inside nil))
      (loop for i from 0 below n
            for j = (mod (+ i n -1) n)        ; previous vertex, wrapping
            do (let* ((vi (aref v i)) (vj (aref v j))
                      (xi (coerce (first vi) 'double-float))
                      (yi (coerce (second vi) 'double-float))
                      (xj (coerce (first vj) 'double-float))
                      (yj (coerce (second vj) 'double-float)))
                 ;; The (yi>lat) != (yj>lat) guard ensures (yj-yi) is non-zero
                 ;; before we divide, so this is short-circuit safe.
                 (when (and (not (eq (> yi lat) (> yj lat)))
                            (< lon (+ xi (/ (* (- xj xi) (- lat yi)) (- yj yi)))))
                   (setf inside (not inside)))))
      inside)))

(defun point-in-polygon-rings-p (lon lat rings)
  "RINGS is an exterior ring followed by zero or more hole rings.  True when the
point is inside the exterior ring and outside every hole."
  (and rings
       (point-in-ring-p lon lat (first rings))
       (notany (lambda (hole) (point-in-ring-p lon lat hole)) (rest rings))))

(defun geometry-contains-point-p (g lon lat)
  "True if the :POLYGON or :MULTIPOLYGON geometry G contains (LON, LAT)."
  (ecase (geometry-kind g)
    (:polygon (point-in-polygon-rings-p lon lat (geometry-coordinates g)))
    (:multipolygon (some (lambda (poly) (point-in-polygon-rings-p lon lat poly))
                         (geometry-coordinates g)))))

(defun bbox-overlap-p (a-min-lon a-min-lat a-max-lon a-max-lat
                       b-min-lon b-min-lat b-max-lon b-max-lat)
  "True if the two axis-aligned bounding boxes overlap (touching counts)."
  (and (<= a-min-lon b-max-lon) (>= a-max-lon b-min-lon)
       (<= a-min-lat b-max-lat) (>= a-max-lat b-min-lat)))

(defun geometry-distance (g1 g2)
  "Great-circle distance in metres between two :POINT geometries."
  (geodesic-distance (geometry-lat g1) (geometry-lon g1)
                     (geometry-lat g2) (geometry-lon g2)))
