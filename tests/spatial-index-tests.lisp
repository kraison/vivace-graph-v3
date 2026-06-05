;;;; Tests for the geohash spatial index (spatial-index.lisp).

(in-package #:graph-db/test)

(def-suite spatial-index-suite
  :description "Geohash spatial index insert/query/remove/persistence."
  :in graph-db-suite)

(in-suite spatial-index-suite)

;;; Coordinates from the demining dataset (Kharkiv Oblast EO finds).
(defparameter *eo-a* '(37.1724312d0 49.2020584d0))   ; lon lat
(defparameter *eo-b* '(37.1773283d0 49.2036314d0))   ; ~400 m from A
(defparameter *far*  '(23.7182919d0 50.0263233d0))   ; Lviv Oblast, ~1000 km

(defun pt (lonlat) (make-point (first lonlat) (second lonlat)))

(test insert-and-query-bbox
  "A window over the AO returns the local finds as candidates, not distant ones."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7)))
      (spatial-index-insert idx 1 (pt *eo-a*))
      (spatial-index-insert idx 2 (pt *eo-b*))
      (spatial-index-insert idx 3 (pt *far*))
      (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
        (is (member 1 cands))
        (is (member 2 cands))
        (is (not (member 3 cands)))))))

(test query-radius-and-refine
  "Radius query is a prefilter; geodesic-distance gives the exact answer."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 9)))
      (spatial-index-insert idx 1 (pt *eo-a*))   ; the center itself
      (spatial-index-insert idx 2 (pt *eo-b*))   ; ~400 m away
      (spatial-index-insert idx 3 (pt *far*))    ; ~1000 km away
      (let* ((lat (second *eo-a*)) (lon (first *eo-a*))
             (cands (spatial-index-query-radius idx lat lon 600d0))
             (within (remove-if-not
                      (lambda (nid)
                        (<= (geodesic-distance
                             lat lon
                             (second (ecase nid (1 *eo-a*) (2 *eo-b*) (3 *far*)))
                             (first (ecase nid (1 *eo-a*) (2 *eo-b*) (3 *far*))))
                            600d0))
                      cands)))
        (is (member 1 cands) "self must be a candidate")
        (is (not (member 3 cands)) "distant point must be filtered by bbox")
        (is (member 1 within))
        (is (member 2 within))
        (is (not (member 3 within)))))))

(test polygon-occupies-cells
  "A task-area polygon is a candidate for windows that overlap it, not others."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (aoi (make-polygon '(((37.170 49.200) (37.180 49.200)
                                (37.180 49.206) (37.170 49.206)
                                (37.170 49.200))))))
      (spatial-index-insert idx 10 aoi)
      (is (member 10 (spatial-index-query-bbox idx 37.172d0 49.201d0 37.174d0 49.203d0)))
      (is (not (member 10 (spatial-index-query-bbox idx 23.70d0 50.00d0 23.75d0 50.05d0)))))))

(test multipolygon-spans-parts
  "Both parts of a multipolygon are reachable; a gap between them is not."
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (mp (make-multipolygon '((((37.10 49.10) (37.11 49.10) (37.11 49.11) (37.10 49.11) (37.10 49.10)))
                                   (((37.50 49.50) (37.51 49.50) (37.51 49.51) (37.50 49.51) (37.50 49.50)))))))
      (spatial-index-insert idx 7 mp)
      (is (member 7 (spatial-index-query-bbox idx 37.10d0 49.10d0 37.11d0 49.11d0)))   ; part A
      (is (member 7 (spatial-index-query-bbox idx 37.50d0 49.50d0 37.51d0 49.51d0)))   ; part B
      (is (not (member 7 (spatial-index-query-bbox idx 37.30d0 49.30d0 37.31d0 49.31d0))))))) ; gap

(test remove-clears-entries
  (with-temp-memory (heap)
    (let ((idx (make-spatial-index heap :precision 7))
          (g (pt *eo-a*)))
      (spatial-index-insert idx 5 g)
      (is (member 5 (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
      (spatial-index-remove idx 5 g)
      (is (not (member 5 (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))))))

(test persistence-reopen-from-disk
  "An index reopened from its on-disk heap at its root address still answers."
  (with-temp-directory (dir)
    (let ((path (namestring (merge-pathnames "spx-heap.dat" dir)))
          (addr nil))
      (let* ((heap (create-memory path (* 1024 1024 16)))
             (idx (make-spatial-index heap :precision 7)))
        (unwind-protect
             (progn
               (spatial-index-insert idx 1 (pt *eo-a*))
               (spatial-index-insert idx 2 (pt *far*))
               (setf addr (spatial-index-address idx)))
          (close-memory heap)))
      (let ((heap (open-memory path)))
        (unwind-protect
             (let ((idx (open-spatial-index heap addr :precision 7)))
               (let ((cands (spatial-index-query-bbox idx 37.16d0 49.19d0 37.19d0 49.21d0)))
                 (is (member 1 cands))
                 (is (not (member 2 cands)))))
          (close-memory heap))))))
