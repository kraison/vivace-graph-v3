;;;; GEOS implementations of the topology refine seam.
;;;;
;;;; These :AROUND methods take over the generic functions declared in core
;;;; geometry-ops.lisp WHEN GEOS is available; otherwise they CALL-NEXT-METHOD to
;;;; the dependency-free default.  All GEOS work happens inside WITH-GEOS-CONTEXT
;;;; (exclusive context checkout) with RAII geometry cleanup.

(in-package :graph-db)

(defun %geos-bool (result operation)
  "Interpret a GEOS predicate char result: 1 true, 0 false, anything else (2 =
exception) signals GEOS-ERROR."
  (case result
    (1 t)
    (0 nil)
    (t (error 'geos-error
              :message (or *geos-last-error*
                           (format nil "GEOS ~A returned ~A" operation result))))))

(defmethod geometry-intersects-p :around ((a geometry) (b geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (%geos-bool (%geos-intersects (geos-ctx-handle ctx) ga gb)
                      'intersects)))
      (call-next-method)))

(defmethod geometry-contains-geometry-p :around ((a geometry) (b geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (%geos-bool (%geos-contains (geos-ctx-handle ctx) ga gb)
                      'contains)))
      (call-next-method)))

(defun geometry-valid-p (g)
  "True if geometry G is topologically valid per GEOS (no self-intersections
etc.).  Requires graph-db/geos; signals GEOS-REQUIRED-FOR-OPERATION otherwise."
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (%geos-bool (%geos-is-valid (geos-ctx-handle ctx) gg) 'is-valid)))
      (error 'geos-required-for-operation :operation 'geometry-valid-p)))

(defmethod geometry-make-valid :around ((g geometry))
  ;; Requires GEOS >= 3.8 (GEOSMakeValid_r).  When unavailable, fall through to
  ;; the default method, which signals GEOS-REQUIRED-FOR-OPERATION.
  (if (and *geos-available-p* *geos-makevalid-available-p*)
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (let ((valid (%geos-make-valid (geos-ctx-handle ctx) gg)))
            (when (cffi:null-pointer-p valid)
              (error 'geos-error
                     :message (or *geos-last-error* "GEOSMakeValid returned NULL")))
            (unwind-protect (geos->geometry ctx valid)
              (%geos-geom-destroy (geos-ctx-handle ctx) valid)))))
      (call-next-method)))

(defmethod geometry-distance-exact :around ((a geometry) (b geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (cffi:with-foreign-object (out :double)
            (let ((rc (%geos-distance (geos-ctx-handle ctx) ga gb out)))
              (when (zerop rc)
                (error 'geos-error
                       :message (or *geos-last-error* "GEOSDistance failed")))
              (cffi:mem-ref out :double)))))
      (call-next-method)))

;;; ---- constructive (overlay) operations ---------------------------------

(defun %geos-overlay (op-fn a b operation)
  "Run a binary GEOS op returning a new geometry, and convert it back to VG."
  (with-geos-context (ctx)
    (with-geos-geoms ((ga ctx a) (gb ctx b))
      (let ((res (funcall op-fn (geos-ctx-handle ctx) ga gb)))
        (when (cffi:null-pointer-p res)
          (error 'geos-error
                 :message (or *geos-last-error*
                              (format nil "GEOS ~A returned NULL" operation))))
        (unwind-protect (geos->geometry ctx res)
          (%geos-geom-destroy (geos-ctx-handle ctx) res))))))

(defmethod geometry-union :around ((a geometry) (b geometry))
  (if *geos-available-p* (%geos-overlay #'%geos-union a b 'union) (call-next-method)))

(defmethod geometry-intersection :around ((a geometry) (b geometry))
  (if *geos-available-p* (%geos-overlay #'%geos-intersection a b 'intersection)
      (call-next-method)))

(defmethod geometry-difference :around ((a geometry) (b geometry))
  (if *geos-available-p* (%geos-overlay #'%geos-difference a b 'difference)
      (call-next-method)))

(defmethod geometry-buffer :around ((g geometry) width &optional (quadrant-segments 8))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (let ((res (%geos-buffer (geos-ctx-handle ctx) gg
                                   (coerce width 'double-float) quadrant-segments)))
            (when (cffi:null-pointer-p res)
              (error 'geos-error :message (or *geos-last-error* "GEOSBuffer returned NULL")))
            (unwind-protect (geos->geometry ctx res)
              (%geos-geom-destroy (geos-ctx-handle ctx) res)))))
      (call-next-method)))

(defmethod geometry-area :around ((g geometry))
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geom (gg ctx g)
          (cffi:with-foreign-object (out :double)
            (when (zerop (%geos-area (geos-ctx-handle ctx) gg out))
              (error 'geos-error :message (or *geos-last-error* "GEOSArea failed")))
            (cffi:mem-ref out :double))))
      (call-next-method)))

;;; ---- geodesic distance between geometries (real metres) ----------------

(defmethod geometry-geodesic-distance :around ((a geometry) (b geometry))
  ;; Point-point is handled by the core default (haversine) -- but routing it
  ;; through GEOS nearest-points gives the same answer, so we let GEOS handle all
  ;; cases when available for consistency, and fall back otherwise.
  (if *geos-available-p*
      (with-geos-context (ctx)
        (with-geos-geoms ((ga ctx a) (gb ctx b))
          (let ((seq (%geos-nearest-points (geos-ctx-handle ctx) ga gb)))
            (when (cffi:null-pointer-p seq)
              (error 'geos-error
                     :message (or *geos-last-error* "GEOSNearestPoints failed")))
            (unwind-protect
                 (cffi:with-foreign-objects ((x1 :double) (y1 :double)
                                             (x2 :double) (y2 :double))
                   ;; seq holds 2 points: idx 0 on A, idx 1 on B.  Coords are
                   ;; (lon lat) = (x y); haversine takes (lat lon).
                   (%geos-coordseq-getx (geos-ctx-handle ctx) seq 0 x1)
                   (%geos-coordseq-gety (geos-ctx-handle ctx) seq 0 y1)
                   (%geos-coordseq-getx (geos-ctx-handle ctx) seq 1 x2)
                   (%geos-coordseq-gety (geos-ctx-handle ctx) seq 1 y2)
                   (geodesic-distance (cffi:mem-ref y1 :double) (cffi:mem-ref x1 :double)
                                      (cffi:mem-ref y2 :double) (cffi:mem-ref x2 :double)))
              (%geos-coordseq-destroy (geos-ctx-handle ctx) seq)))))
      (call-next-method)))
