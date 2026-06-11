;;;; Bridge between VivaceGraph `geometry' structs and GEOS geometries, via WKT.
;;;;
;;;; We own the geometry struct, so geometry->wkt is a pure, exact Lisp emitter
;;;; (full double-float precision, lon/lat = WKT x y with NO axis swap, explicit
;;;; ring closure).  GEOS parses/writes WKT through a per-context reader/writer.
;;;; wkt->geometry is a small parser used to bring GEOS results (e.g. makeValid
;;;; output) back into a VG geometry.  Coordinate-sequence construction is a
;;;; possible later optimization; WKT is the simplest correct path.

(in-package :graph-db)

(defparameter +wkt-whitespace+ '(#\Space #\Tab #\Newline #\Return))

;;; --------------------------------------------------------------------------
;;; geometry -> WKT  (pure)
;;; --------------------------------------------------------------------------

(defun %wkt-num (x)
  "Format X as a plain decimal preserving the double's value.  15 fractional
digits exceed a double's resolution across the lon/lat range, so reading the
string back yields the same double; trailing zeros are trimmed for tidiness."
  (let ((s (format nil "~,15F" (coerce x 'double-float))))
    (when (find #\. s)
      (setf s (string-right-trim "0" s))
      (when (char= (char s (1- (length s))) #\.)
        (setf s (concatenate 'string s "0"))))
    s))

(defun %coord->wkt (c)
  "C is a (lon lat) pair -> \"lon lat\"."
  (concatenate 'string (%wkt-num (first c)) " " (%wkt-num (second c))))

(defun %close-ring (ring)
  "Ensure RING's first and last vertices coincide (WKT requires closed rings)."
  (if (and ring (rest ring)
           (let ((f (first ring)) (l (car (last ring))))
             (and (= (first f) (first l)) (= (second f) (second l)))))
      ring
      (append ring (list (first ring)))))

(defun %ring->wkt (ring)
  (format nil "(~{~A~^, ~})" (mapcar #'%coord->wkt (%close-ring ring))))

(defun %polygon-body->wkt (rings)
  "RINGS = exterior + holes -> \"((ext), (hole), ...)\"."
  (format nil "(~{~A~^, ~})" (mapcar #'%ring->wkt rings)))

(defun geometry->wkt (g)
  "Emit the WKT string for geometry G (lon/lat axis order, full precision)."
  (ecase (geometry-kind g)
    (:point
     (let ((c (geometry-coordinates g)))
       (if c (format nil "POINT (~A)" (%coord->wkt c)) "POINT EMPTY")))
    (:linestring
     (let ((cs (geometry-coordinates g)))
       (if cs (format nil "LINESTRING (~{~A~^, ~})" (mapcar #'%coord->wkt cs))
           "LINESTRING EMPTY")))
    (:polygon
     (let ((rings (geometry-coordinates g)))
       (if rings (format nil "POLYGON ~A" (%polygon-body->wkt rings))
           "POLYGON EMPTY")))
    (:multipolygon
     (let ((polys (geometry-coordinates g)))
       (if polys
           (format nil "MULTIPOLYGON (~{~A~^, ~})"
                   (mapcar #'%polygon-body->wkt polys))
           "MULTIPOLYGON EMPTY")))))

;;; --------------------------------------------------------------------------
;;; WKT -> geometry  (pure, minimal parser)
;;; --------------------------------------------------------------------------

(defun %parse-coord-list (str)
  "Parse \"x y, x y, ...\" into a list of (lon lat) double-float pairs."
  (let ((*read-default-float-format* 'double-float))
    (loop for piece in (uiop:split-string str :separator ",")
          for trimmed = (string-trim +wkt-whitespace+ piece)
          unless (string= trimmed "")
            collect (let ((nums (remove "" (uiop:split-string trimmed :separator " ")
                                        :test #'string=)))
                      (list (coerce (read-from-string (first nums)) 'double-float)
                            (coerce (read-from-string (second nums)) 'double-float))))))

(defun %skip-ws (s i)
  (loop while (and (< i (length s)) (member (char s i) +wkt-whitespace+)) do (incf i))
  i)

(defun %parse-wkt-group (s i)
  "Parse a parenthesized group starting at S[I]=#\\( .  Returns (values node
next-index): a coordinate list when the group holds coordinates, else a list of
sub-groups (for nested POLYGON/MULTIPOLYGON structure)."
  (incf i)                              ; consume (
  (setf i (%skip-ws s i))
  (if (char= (char s i) #\()
      (let ((subs '()))                 ; group of groups
        (loop
          (setf i (%skip-ws s i))
          (multiple-value-bind (sub j) (%parse-wkt-group s i)
            (push sub subs) (setf i j))
          (setf i (%skip-ws s i))
          (cond ((char= (char s i) #\,) (incf i))
                ((char= (char s i) #\)) (incf i) (return))
                (t (error 'geos-error :message "Malformed WKT group"))))
        (values (nreverse subs) i))
      (let ((end (position #\) s :start i)))   ; leaf: coordinate list
        (unless end (error 'geos-error :message "Unterminated WKT group"))
        (values (%parse-coord-list (subseq s i end)) (1+ end)))))

(defun wkt->geometry (wkt)
  "Parse a WKT string (POINT/LINESTRING/POLYGON/MULTIPOLYGON, with EMPTY) into a
VG geometry.  Used to bring GEOS results back into VG.  Signals GEOS-ERROR on an
unsupported type."
  (let* ((s (string-trim +wkt-whitespace+ wkt))
         (paren (position #\( s))
         (kw (string-upcase (string-trim +wkt-whitespace+
                                         (subseq s 0 (or paren (length s)))))))
    (flet ((emptyp () (or (null paren) (search "EMPTY" (string-upcase s)))))
      (cond
        ((string= kw "POINT")
         (if (emptyp) (make-point 0d0 0d0)
             (let ((c (first (nth-value 0 (%parse-wkt-group s paren)))))
               (make-point (first c) (second c)))))
        ((string= kw "LINESTRING")
         (if (emptyp) (make-linestring '())
             (make-linestring (nth-value 0 (%parse-wkt-group s paren)))))
        ((string= kw "POLYGON")
         (if (emptyp) (make-polygon '())
             (make-polygon (nth-value 0 (%parse-wkt-group s paren)))))
        ((string= kw "MULTIPOLYGON")
         (if (emptyp) (make-multipolygon '())
             (make-multipolygon (nth-value 0 (%parse-wkt-group s paren)))))
        (t (error 'geos-error
                  :message (format nil "Unsupported WKT geometry type: ~A" kw)))))))

;;; --------------------------------------------------------------------------
;;; GEOS read/write wrappers + RAII
;;; --------------------------------------------------------------------------

(defun %read-wkt (ctx wkt)
  "Parse WKT into a GEOS geometry pointer (caller must destroy it)."
  (let ((g (%geos-wktreader-read (geos-ctx-handle ctx) (geos-ctx-reader ctx) wkt)))
    (when (cffi:null-pointer-p g)
      (error 'geos-error :message (or *geos-last-error*
                                      (format nil "GEOS could not parse WKT: ~A" wkt))))
    g))

(defun %write-wkt (ctx geom)
  "Write a GEOS geometry pointer to a Lisp WKT string (frees the C buffer)."
  (let ((cstr (%geos-wktwriter-write (geos-ctx-handle ctx) (geos-ctx-writer ctx) geom)))
    (when (cffi:null-pointer-p cstr)
      (error 'geos-error :message (or *geos-last-error* "GEOS WKT write failed")))
    (unwind-protect (cffi:foreign-string-to-lisp cstr)
      (%geos-free (geos-ctx-handle ctx) cstr))))

(defun geometry->geos (ctx g)
  "Build a GEOS geometry pointer from VG geometry G (caller destroys it)."
  (%read-wkt ctx (geometry->wkt g)))

(defun geos->geometry (ctx geom)
  "Convert a GEOS geometry pointer back into a VG geometry."
  (wkt->geometry (%write-wkt ctx geom)))

(defmacro with-geos-geom ((var ctx geometry) &body body)
  "Bind VAR to a GEOS geometry built from VG GEOMETRY for BODY; destroy it after."
  (let ((c (gensym "CTX")))
    `(let* ((,c ,ctx)
            (,var (geometry->geos ,c ,geometry)))
       (unwind-protect (progn ,@body)
         (%geos-geom-destroy (geos-ctx-handle ,c) ,var)))))

(defmacro with-geos-geoms (bindings &body body)
  "Nest WITH-GEOS-GEOM over BINDINGS (each (var ctx geometry))."
  (if (null bindings)
      `(progn ,@body)
      `(with-geos-geom ,(first bindings)
         (with-geos-geoms ,(rest bindings) ,@body))))
