(in-package :graph-db)

;;; Geometry value type for the spatial extension.
;;;
;;; Part of VivaceGraph's public, general-purpose spatial layer (no domain
;;; knowledge lives here).  Coordinates are WGS84 and stored in (LON LAT) order
;;; -- GIS x,y / GeoJSON convention -- as DOUBLE-FLOATs.  KIND is one of :POINT
;;; :LINESTRING :POLYGON :MULTIPOLYGON, with COORDINATES shaped accordingly:
;;;   :point         -> (lon lat)
;;;   :linestring    -> ((lon lat) (lon lat) ...)
;;;   :polygon       -> (ring ...) where ring = ((lon lat) ...); the first ring
;;;                     is the exterior boundary, any others are holes
;;;   :multipolygon  -> (polygon ...) where polygon = (ring ...)
;;;
;;; The wire format reuses the generic byte protocol:
;;;   [+geometry+][len-header][serialized kind-code][serialized coordinates]
;;; built with SERIALIZE-MULTIPLE, so EXTRACT-LENGTH's variable-length branch
;;; decodes it with no change to the serialization dispatch core.  The payload
;;; encoding may later be compacted (e.g. a flat double-float array) WITHOUT
;;; changing the type tag, the struct, or this public API.

(defstruct (geometry (:constructor %make-geometry) (:predicate geometryp))
  (kind :point :type symbol)
  (coordinates nil :type list))

(defparameter +geometry-kinds+ '(:point :linestring :polygon :multipolygon)
  "Ordered list; a geometry's KIND is serialized as its position here.")

(defun geometry-kind-code (kind)
  (or (position kind +geometry-kinds+)
      (error "Unknown geometry kind ~S" kind)))

(defun geometry-code-kind (code)
  (or (nth code +geometry-kinds+)
      (error "Unknown geometry kind code ~S" code)))

(declaim (inline %df))
(defun %df (x)
  "Coerce X to DOUBLE-FLOAT (all stored coordinates are double-floats)."
  (coerce x 'double-float))

(defun %coord (c)
  "Normalize a single (lon lat) coordinate to a list of two double-floats."
  (list (%df (first c)) (%df (second c))))

(defun %ring (ring)
  (mapcar #'%coord ring))

;;; -------------------------------------------------------------------------
;;; Constructors
;;; -------------------------------------------------------------------------

(defun make-point (lon lat)
  (%make-geometry :kind :point :coordinates (list (%df lon) (%df lat))))

(defun make-linestring (coords)
  "COORDS: a list of (lon lat)."
  (%make-geometry :kind :linestring :coordinates (mapcar #'%coord coords)))

(defun make-polygon (rings)
  "RINGS: a list of rings, each a list of (lon lat).  First ring is the exterior
boundary; subsequent rings are holes."
  (%make-geometry :kind :polygon :coordinates (mapcar #'%ring rings)))

(defun make-multipolygon (polygons)
  "POLYGONS: a list of polygons, each a list of rings."
  (%make-geometry :kind :multipolygon
                  :coordinates (mapcar (lambda (p) (mapcar #'%ring p)) polygons)))

;;; -------------------------------------------------------------------------
;;; Accessors
;;; -------------------------------------------------------------------------

(defun geometry-lon (g)
  "Longitude of a :POINT geometry."
  (first (geometry-coordinates g)))

(defun geometry-lat (g)
  "Latitude of a :POINT geometry."
  (second (geometry-coordinates g)))

(defun geometry-bbox (g)
  "Axis-aligned bounding box of G as (values min-lon min-lat max-lon max-lat)."
  (let ((min-lon nil) (min-lat nil) (max-lon nil) (max-lat nil))
    (labels ((visit (lon lat)
               (when (or (null min-lon) (< lon min-lon)) (setf min-lon lon))
               (when (or (null max-lon) (> lon max-lon)) (setf max-lon lon))
               (when (or (null min-lat) (< lat min-lat)) (setf min-lat lat))
               (when (or (null max-lat) (> lat max-lat)) (setf max-lat lat)))
             (walk (x)
               ;; X is either a (lon lat) pair or a nesting of such.
               (if (and (consp x) (numberp (first x)) (numberp (second x))
                        (not (consp (first x))))
                   (visit (first x) (second x))
                   (mapc #'walk x))))
      (if (eq (geometry-kind g) :point)
          (visit (geometry-lon g) (geometry-lat g))
          (walk (geometry-coordinates g)))
      (values min-lon min-lat max-lon max-lat))))

;;; -------------------------------------------------------------------------
;;; Serialization (reuses the generic byte protocol; no core dispatch changes)
;;; -------------------------------------------------------------------------

(defmethod serialize ((g geometry))
  (serialize-multiple +geometry+
                      (geometry-kind-code (geometry-kind g))
                      (geometry-coordinates g)))

(defmethod deserialize-help ((become (eql +geometry+)) (bytes array))
  (declare (type (array (unsigned-byte 8)) bytes))
  (let ((parts (extract-all-subseqs bytes)))
    (%make-geometry :kind (geometry-code-kind (deserialize (first parts)))
                    :coordinates (deserialize (second parts)))))

(defmethod deserialize-help-mmap ((become (eql +geometry+)) (p mpointer)
                                  n-bytes header-length)
  (declare (ignore header-length))
  (deserialize-help +geometry+
                    (get-bytes (mpointer-mmap p) (mpointer-loc p) n-bytes)))
