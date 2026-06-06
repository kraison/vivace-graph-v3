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
