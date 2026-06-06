;;;; S2: the topology seam -- GEOS-exact predicates vs the dependency-free
;;;; fallbacks.  The key case is two polygons that overlap only partially: the
;;;; bbox-overlap fallback says "intersects" while GEOS contains says "not
;;;; contained" -- proving GEOS exactness over the coarse fallback.

(in-package #:graph-db/geos-test)

(def-suite geos-ops-suite
  :description "Topology refine seam: intersects / contains, GEOS vs fallback."
  :in geos-suite)

(in-suite geos-ops-suite)

;; A 0..4 square; a 2..6 square (overlaps the first partially); a 10..12 square
;; (disjoint); a 1..3 square (fully inside the first).
(defun sq (x0 y0 x1 y1)
  (make-polygon (list (list (list x0 y0) (list x1 y0)
                            (list x1 y1) (list x0 y1) (list x0 y0)))))
(defparameter *sq-a* (sq 0d0 0d0 4d0 4d0))
(defparameter *sq-overlap* (sq 2d0 2d0 6d0 6d0))
(defparameter *sq-disjoint* (sq 10d0 10d0 12d0 12d0))
(defparameter *sq-inside* (sq 1d0 1d0 3d0 3d0))

;;; ---- intersects --------------------------------------------------------

(test intersects-overlapping-and-disjoint
  "Exact intersects: overlapping polygons true, disjoint false."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (progn
        (is-true  (geometry-intersects-p *sq-a* *sq-overlap*))
        (is-false (geometry-intersects-p *sq-a* *sq-disjoint*)))))

(test intersects-point-in-polygon-consistent
  "intersects of a point and a polygon agrees with point-in-polygon."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((inside (make-point 2d0 2d0)) (outside (make-point 9d0 9d0)))
        (is-true  (geometry-intersects-p *sq-a* inside))
        (is-false (geometry-intersects-p *sq-a* outside)))))

;;; ---- contains ----------------------------------------------------------

(test contains-exact-vs-partial-overlap
  "GEOS contains is exact: A contains the fully-inside square, but does NOT
contain the partially-overlapping one -- the case the bbox fallback gets wrong."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (progn
        (is-true  (geometry-contains-geometry-p *sq-a* *sq-inside*))
        (is-false (geometry-contains-geometry-p *sq-a* *sq-overlap*)))))

(test contains-point-matches-hand-rolled
  "contains-geometry of a point matches the hand-rolled point-in-polygon."
  (if (not *geos-available-p*) (skip "GEOS not available")
      (let ((p (make-point 2d0 2d0)))
        (is (eq (and (geometry-contains-point-p *sq-a* 2d0 2d0) t)
                (and (geometry-contains-geometry-p *sq-a* p) t))))))

;;; ---- fallback (GEOS forced off) ----------------------------------------

(test fallback-intersects-is-bbox-coarse
  "With GEOS off, intersects of two extended geometries is the COARSE bbox
overlap: the disjoint-bbox pair is false, but a partial overlap is true (same as
GEOS here) and -- critically -- a non-overlapping pair whose BBOXES touch would
read true.  We assert the documented coarse behaviour."
  (without-geos
    ;; bboxes [0,4]x[0,4] and [2,6]x[2,6] overlap -> true (matches GEOS)
    (is-true (geometry-intersects-p *sq-a* *sq-overlap*))
    ;; bboxes disjoint -> false
    (is-false (geometry-intersects-p *sq-a* *sq-disjoint*))
    ;; coarse over-approximation: two L-shaped-ish squares whose bboxes overlap
    ;; but polygons don't would read TRUE under fallback.  Here a [5,6] square
    ;; sharing only a bbox corner with [0,4] stays disjoint (bboxes don't meet).
    (is-false (geometry-intersects-p *sq-a* (sq 5d0 5d0 6d0 6d0)))))

(test fallback-contains-point-exact-but-extended-signals
  "With GEOS off, contains is exact for a point but signals for extended-in-extended."
  (without-geos
    (is-true (geometry-contains-geometry-p *sq-a* (make-point 2d0 2d0)))
    (is-false (geometry-contains-geometry-p *sq-a* (make-point 9d0 9d0)))
    (signals geos-required-for-operation
      (geometry-contains-geometry-p *sq-a* *sq-inside*))))

(test fallback-make-valid-and-distance-signal
  "With GEOS off, make-valid and distance-exact have no fallback and signal."
  (without-geos
    (signals geos-required-for-operation (geometry-make-valid *sq-a*))
    (signals geos-required-for-operation
      (geometry-distance-exact *sq-a* *sq-disjoint*))))
