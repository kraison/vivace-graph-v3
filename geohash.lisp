(in-package :graph-db)

;;; Geohash encoding for the spatial index.
;;;
;;; Public, general-purpose (no domain knowledge).  A geohash maps a (lat, lon)
;;; point to a base-32 string whose prefixes are nested bounding cells -- so an
;;; ordered store (our skip list) can answer window/proximity queries by prefix
;;; range scans.  Longitude bits and latitude bits are interleaved, longitude
;;; first; every 5 bits become one base-32 character.
;;;
;;; The chosen alphabet omits a, i, l, o to avoid ambiguity (standard geohash).

(alexandria:define-constant +geohash-base32+ "0123456789bcdefghjkmnpqrstuvwxyz"
  :test 'string=)

(defun geohash-encode (lat lon &optional (precision 12))
  "Encode (LAT, LON) to a PRECISION-character geohash string."
  (declare (type real lat lon) (type (integer 1 22) precision))
  (let ((lat-lo -90d0) (lat-hi 90d0) (lon-lo -180d0) (lon-hi 180d0)
        (even-bit t) (bit 0) (idx 0)
        (chars (make-array precision :element-type 'character :fill-pointer 0)))
    (loop while (< (fill-pointer chars) precision) do
      (if even-bit
          (let ((mid (/ (+ lon-lo lon-hi) 2)))
            (if (>= lon mid)
                (progn (setf idx (logior (ash idx 1) 1)) (setf lon-lo mid))
                (progn (setf idx (ash idx 1)) (setf lon-hi mid))))
          (let ((mid (/ (+ lat-lo lat-hi) 2)))
            (if (>= lat mid)
                (progn (setf idx (logior (ash idx 1) 1)) (setf lat-lo mid))
                (progn (setf idx (ash idx 1)) (setf lat-hi mid)))))
      (setf even-bit (not even-bit))
      (when (= (incf bit) 5)
        (vector-push (char +geohash-base32+ idx) chars)
        (setf bit 0 idx 0)))
    (coerce chars 'simple-string)))

(defun geohash-bbox (hash)
  "Bounding cell of HASH as (values min-lon min-lat max-lon max-lat)."
  (let ((lat-lo -90d0) (lat-hi 90d0) (lon-lo -180d0) (lon-hi 180d0) (even-bit t))
    (loop for c across hash
          for cd = (position (char-downcase c) +geohash-base32+) do
            (unless cd (error "Invalid geohash character ~C in ~S" c hash))
            (dolist (mask '(16 8 4 2 1))
              (if even-bit
                  (let ((mid (/ (+ lon-lo lon-hi) 2)))
                    (if (plusp (logand cd mask)) (setf lon-lo mid) (setf lon-hi mid)))
                  (let ((mid (/ (+ lat-lo lat-hi) 2)))
                    (if (plusp (logand cd mask)) (setf lat-lo mid) (setf lat-hi mid))))
              (setf even-bit (not even-bit))))
    (values lon-lo lat-lo lon-hi lat-hi)))

(defun geohash-decode (hash)
  "Center of HASH's cell as (values lat lon)."
  (multiple-value-bind (min-lon min-lat max-lon max-lat) (geohash-bbox hash)
    (values (/ (+ min-lat max-lat) 2) (/ (+ min-lon max-lon) 2))))

(defun geohash-cell-size (precision)
  "Cell dimensions at PRECISION as (values lon-width lat-height) in degrees."
  (let ((lon-bits (ceiling (* 5 precision) 2))
        (lat-bits (floor (* 5 precision) 2)))
    (values (/ 360d0 (expt 2 lon-bits))
            (/ 180d0 (expt 2 lat-bits)))))

(defun geohash-prefix-range (cell)
  "Half-open key range (values START END) such that any full geohash with prefix
CELL sorts in [START, END).  END appends a character just above the alphabet so
the range scan over an ordered store captures the whole cell."
  (values cell (concatenate 'string cell (string (code-char (1+ (char-code #\z)))))))

(defun %covering-precision (dlon dlat max-cells)
  "Finest precision whose grid covers a DLON x DLAT degree box in <= MAX-CELLS."
  (let ((best 1))
    (loop for p from 1 to 12 do
      (multiple-value-bind (lw lh) (geohash-cell-size p)
        (let ((ncells (* (+ 1 (ceiling dlon lw)) (+ 1 (ceiling dlat lh)))))
          (if (<= ncells max-cells) (setf best p) (return)))))
    best))

(defun geohash-covering (min-lon min-lat max-lon max-lat
                         &key precision (max-cells 256))
  "List of distinct geohash cells (strings) covering the given bounding box.
Used to turn a map viewport into a set of prefix range scans.  PRECISION is
chosen adaptively to stay under MAX-CELLS unless given explicitly."
  (let* ((dlon (max 0d0 (- max-lon min-lon)))
         (dlat (max 0d0 (- max-lat min-lat)))
         (p (or precision (%covering-precision dlon dlat max-cells)))
         (seen (make-hash-table :test 'equal))
         (cells '()))
    (multiple-value-bind (lw lh) (geohash-cell-size p)
      (let ((nlon (+ 1 (ceiling dlon lw)))
            (nlat (+ 1 (ceiling dlat lh))))
        (dotimes (i (1+ nlon))
          (let ((lon (min max-lon (+ min-lon (* i lw)))))
            (dotimes (j (1+ nlat))
              (let* ((lat (min max-lat (+ min-lat (* j lh))))
                     (cell (geohash-encode lat lon p)))
                (unless (gethash cell seen)
                  (setf (gethash cell seen) t)
                  (push cell cells))))))))
    cells))
