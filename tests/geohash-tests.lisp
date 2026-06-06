;;;; Tests for geohash encoding (geohash.lisp).

(in-package #:graph-db/test)

(def-suite geohash-suite
  :description "Geohash encode/decode, cell geometry, and bbox covering."
  :in graph-db-suite)

(in-suite geohash-suite)

(test canonical-encode
  "The Wikipedia reference coordinate encodes to the known geohash."
  (is (string= "u4pruydqqvj" (geohash-encode 57.64911d0 10.40744d0 11))))

(test precision-length
  (dolist (p '(1 5 8 12))
    (is (= p (length (geohash-encode 50.0d0 23.0d0 p))))))

(test prefix-nesting
  "A coarser geohash is a prefix of a finer one for the same point."
  (let ((fine (geohash-encode 49.2020584d0 37.1724312d0 12)))
    (dolist (p '(1 4 7 10))
      (is (string= (geohash-encode 49.2020584d0 37.1724312d0 p)
                   (subseq fine 0 p))))))

(test bbox-contains-point
  "The decoded cell bounding box contains the original point."
  (dolist (pt '((49.2020584d0 37.1724312d0)
                (50.0263233d0 23.7182919d0)
                (-33.85d0 151.21d0)))
    (let ((lat (first pt)) (lon (second pt)))
      (multiple-value-bind (mnx mny mxx mxy)
          (geohash-bbox (geohash-encode lat lon 10))
        (is (<= mnx lon mxx))
        (is (<= mny lat mxy))))))

(test decode-center-near-point
  "Decoding returns a center within the cell half-extent of the point."
  (let* ((lat 49.2020584d0) (lon 37.1724312d0)
         (h (geohash-encode lat lon 9)))
    (multiple-value-bind (mnx mny mxx mxy) (geohash-bbox h)
      (multiple-value-bind (clat clon) (geohash-decode h)
        (is (<= (abs (- clat lat)) (/ (- mxy mny) 2)))
        (is (<= (abs (- clon lon)) (/ (- mxx mnx) 2)))))))

(test cell-size-values
  (multiple-value-bind (lw lh) (geohash-cell-size 1)
    (is (= lw 45d0)) (is (= lh 45d0)))
  (multiple-value-bind (lw lh) (geohash-cell-size 2)
    (is (= lw 11.25d0)) (is (= lh 5.625d0))))

(test prefix-range-contains-hash
  "A point's full geohash sorts within the prefix range of any coarser cell."
  (let* ((full (geohash-encode 49.2020584d0 37.1724312d0 12))
         (cell (subseq full 0 6)))
    (multiple-value-bind (start end) (geohash-prefix-range cell)
      (is (and (string>= full start) (string< full end))))))

(test covering-intersects-and-includes-center
  "Every covering cell intersects the query box, and the box's center cell is
included."
  (let* ((min-lon 37.16d0) (min-lat 49.19d0) (max-lon 37.19d0) (max-lat 49.21d0)
         (cells (geohash-covering min-lon min-lat max-lon max-lat :max-cells 256))
         (center-cell (let ((p (length (first cells))))
                        (geohash-encode (/ (+ min-lat max-lat) 2)
                                        (/ (+ min-lon max-lon) 2) p))))
    (is (plusp (length cells)))
    (is (member center-cell cells :test #'string=))
    (dolist (c cells)
      (multiple-value-bind (cnx cny cxx cxy) (geohash-bbox c)
        (is (and (<= cnx max-lon) (>= cxx min-lon)
                 (<= cny max-lat) (>= cxy min-lat))
            "cell ~S does not intersect the query box" c)))))

(test covering-tiny-box
  "A degenerate (point) box yields at least one covering cell."
  (is (plusp (length (geohash-covering 37.1724d0 49.2020d0 37.1724d0 49.2020d0)))))
