;;;; S4: validity repair (GEOSMakeValid) and exact PLANAR distance.

(in-package #:graph-db/geos-test)

(def-suite geos-makevalid-suite
  :description "geometry-make-valid + geometry-distance-exact."
  :in geos-suite)

(in-suite geos-makevalid-suite)

;; A self-intersecting "bowtie" (figure-8): edges (0,0)-(4,4) and (4,0)-(0,4)
;; cross at (2,2), so the ring is invalid.
(defun bowtie ()
  (make-polygon '(((0d0 0d0) (4d0 4d0) (4d0 0d0) (0d0 4d0) (0d0 0d0)))))

(defun valid-square ()
  (make-polygon '(((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0) (0d0 0d0)))))

(test bowtie-is-invalid-square-is-valid
  "Sanity: the bowtie is invalid, the plain square is valid."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (progn
        (is-false (geometry-valid-p (bowtie)))
        (is-true  (geometry-valid-p (valid-square))))))

(test make-valid-repairs-bowtie
  "make-valid turns the invalid bowtie into a valid geometry."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t (let ((fixed (geometry-make-valid (bowtie))))
             (is (geometryp fixed))
             (is-true (geometry-valid-p fixed) "repaired geometry is valid")
             ;; the bowtie repairs to a polygonal result (poly or multipolygon)
             (is (member (geometry-kind fixed) '(:polygon :multipolygon)))))))

(test make-valid-keeps-valid-valid
  "make-valid on an already-valid polygon yields a still-valid geometry."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not *geos-makevalid-available-p*) (skip "GEOS < 3.8: no makeValid"))
        (t (is-true (geometry-valid-p (geometry-make-valid (valid-square)))))))

;;; ---- exact planar distance ---------------------------------------------

(defun approx= (a b &optional (eps 1d-6)) (<= (abs (- a b)) eps))

(test distance-exact-points-is-planar
  "geometry-distance-exact between two points is the PLANAR (Euclidean) distance
in coordinate units: (0,0)-(3,4) = 5.0 (degrees, NOT metres)."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (is (approx= 5d0 (geometry-distance-exact (make-point 0d0 0d0)
                                                (make-point 3d0 4d0))))))

(test distance-exact-polygons
  "Distance between disjoint polygons is the gap; overlapping polygons are 0."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((a (make-polygon '(((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 1d0) (0d0 0d0)))))
            (b (make-polygon '(((3d0 0d0) (4d0 0d0) (4d0 1d0) (3d0 1d0) (3d0 0d0)))))
            (c (make-polygon '(((0.5d0 0d0) (2d0 0d0) (2d0 1d0) (0.5d0 1d0) (0.5d0 0d0))))))
        (is (approx= 2d0 (geometry-distance-exact a b)) "gap from x=1 to x=3")
        (is (approx= 0d0 (geometry-distance-exact a c)) "overlapping -> 0"))))
