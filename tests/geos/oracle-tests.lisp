;;;; S5: cross-check VivaceGraph+GEOS against an independent engine (Python
;;;; shapely).  shapely bundles its own GEOS, so agreement validates our WKT
;;;; bridge (axis order, ring closure, precision) and predicate wiring against a
;;;; separate implementation.  Skips cleanly when python3/shapely is absent.

(in-package #:graph-db/geos-test)

(def-suite geos-oracle-suite
  :description "VG+GEOS predicates vs the shapely oracle."
  :in geos-suite)

(in-suite geos-oracle-suite)

(defun shapely-available-p ()
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program
                        '("python3" "-c" "import shapely")
                        :ignore-error-status t)))))

(defun oracle-script ()
  (asdf:system-relative-pathname :graph-db "tests/geos/oracle.py"))

(defun run-oracle (pairs)
  "PAIRS is a list of (A . B) VG geometries.  Returns a list of (intersects
contains valid) integer triples from shapely, one per pair."
  (uiop:with-temporary-file (:stream s :pathname in :type "wkt")
    (dolist (p pairs)
      (format s "~A | ~A~%" (geometry->wkt (car p)) (geometry->wkt (cdr p))))
    :close-stream
    (let* ((out (uiop:run-program
                 (list "python3" (namestring (oracle-script)) (namestring in))
                 :output :string))
           (lines (remove "" (uiop:split-string out :separator '(#\Newline))
                          :test #'string=)))
      (mapcar (lambda (line)
                (mapcar #'parse-integer
                        (remove "" (uiop:split-string line :separator '(#\Space))
                                :test #'string=)))
              lines))))

(defun sq (x0 y0 x1 y1)
  (make-polygon (list (list (list x0 y0) (list x1 y0)
                            (list x1 y1) (list x0 y1) (list x0 y0)))))

;; All valid; chosen to span overlap / disjoint / containment / touching /
;; point-in / point-out / boundary cases.  shapely (GEOS) and our libgeos share
;; OGC semantics, so they must agree exactly.
(defun predicate-corpus ()
  (list (cons (sq 0d0 0d0 4d0 4d0) (sq 2d0 2d0 6d0 6d0))      ; overlap
        (cons (sq 0d0 0d0 1d0 1d0) (sq 3d0 3d0 4d0 4d0))      ; disjoint
        (cons (sq 0d0 0d0 4d0 4d0) (sq 1d0 1d0 3d0 3d0))      ; A contains B
        (cons (sq 0d0 0d0 4d0 4d0) (make-point 2d0 2d0))      ; point inside
        (cons (sq 0d0 0d0 4d0 4d0) (make-point 9d0 9d0))      ; point outside
        (cons (sq 0d0 0d0 2d0 2d0) (sq 2d0 0d0 4d0 2d0))      ; share an edge (touch)
        (cons (sq 0d0 0d0 4d0 4d0) (make-point 0d0 2d0))      ; point on boundary
        (cons (sq 0d0 0d0 4d0 4d0) (sq 1d0 1d0 5d0 3d0))))    ; poke-out (not contained)

(test predicates-match-shapely
  "geometry-intersects-p and geometry-contains-geometry-p agree with shapely on
every pair of the corpus."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not (shapely-available-p)) (skip "python3/shapely not available"))
        (t (let* ((corpus (predicate-corpus))
                  (oracle (run-oracle corpus)))
             (is (= (length corpus) (length oracle)))
             (loop for (a . b) in corpus
                   for (o-inter o-cont) in oracle
                   for n from 0 do
                     (is (eq (= 1 o-inter) (and (geometry-intersects-p a b) t))
                         "pair ~D: intersects VG~A vs shapely~A" n
                         (and (geometry-intersects-p a b) t) (= 1 o-inter))
                     (is (eq (= 1 o-cont) (and (geometry-contains-geometry-p a b) t))
                         "pair ~D: contains VG~A vs shapely~A" n
                         (and (geometry-contains-geometry-p a b) t) (= 1 o-cont)))))))

(test validity-matches-shapely
  "geometry-valid-p agrees with shapely on a valid square and an invalid bowtie."
  (cond ((not *geos-available-p*) (skip "GEOS not available"))
        ((not (shapely-available-p)) (skip "python3/shapely not available"))
        (t (let* ((good (sq 0d0 0d0 2d0 2d0))
                  (bowtie (make-polygon
                           '(((0d0 0d0) (4d0 4d0) (4d0 0d0) (0d0 4d0) (0d0 0d0)))))
                  ;; B side is unused for validity; pass A|A.
                  (oracle (run-oracle (list (cons good good) (cons bowtie bowtie)))))
             (destructuring-bind ((gi gc gv) (bi bc bv)) oracle
               (declare (ignore gi gc bi bc))
               (is (eq (= 1 gv) (geometry-valid-p good)))
               (is (eq (= 1 bv) (geometry-valid-p bowtie))))))))
