;;;; GRAPH-STRESS-SUITE
;;;;
;;;; Scale tests for the graph model layer:
;;;;   - large vertex population (3 types, type-index counts)
;;;;   - large edge population (adjacency counts per hub)
;;;;   - prolog is-a query over a large vertex set
;;;;   - graph reopen: all vertices and edges survive close + open

(in-package #:graph-db/stress-test)

(def-suite graph-stress-suite
  :description "Scale tests for the graph model layer."
  :in stress-suite)

(in-suite graph-stress-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: large vertex population
;;;
;;; Insert N vertices split across 3 types.  Verify:
;;;   - map-vertices :vertex-type count matches per-type insert count
;;;   - total across all types = N
;;; ---------------------------------------------------------------------------

(test large-vertex-population
  "N vertices across 3 types; per-type and total counts must match inserts."
  (let* ((n-per-type (scale 2000 200))
         (n-total    (* 3 n-per-type)))
    (with-stress-graph (g)
      (let ((start (get-internal-real-time)))
        (with-transaction ()
          (dotimes (i n-per-type)
            (make-s-item   :value i :label "item")
            (make-s-widget :name (format nil "w~D" i) :score i)
            (make-s-thing  :tag i)))
        (record-throughput "large-vertex-insert" n-total
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second))))
      (flet ((count-type (type)
               (length (map-vertices #'identity g
                                     :collect-p t
                                     :vertex-type type))))
        (is (= n-per-type (count-type 's-item))
            "s-item count: expected ~D got ~D" n-per-type (count-type 's-item))
        (is (= n-per-type (count-type 's-widget))
            "s-widget count: expected ~D got ~D" n-per-type (count-type 's-widget))
        (is (= n-per-type (count-type 's-thing))
            "s-thing count: expected ~D got ~D" n-per-type (count-type 's-thing))
        (is (= n-total (+ (count-type 's-item)
                          (count-type 's-widget)
                          (count-type 's-thing)))
            "Total vertex count mismatch")))))

;;; ---------------------------------------------------------------------------
;;; Test 2: large edge population
;;;
;;; Create H hub vertices and K target vertices per hub; insert 1 edge from
;;; each hub to each of its K targets.  Verify outgoing-edges count per hub.
;;; ---------------------------------------------------------------------------

(test large-edge-population
  "H hubs × K edges each; outgoing-edges must return K for every hub."
  (let* ((h (scale 50  5))
         (k (scale 200 20)))
    (with-stress-graph (g)
      (let ((hub-ids    (make-array h))
            (target-ids (make-array (* h k))))
        (with-transaction ()
          (dotimes (i h)
            (setf (aref hub-ids i) (id (make-s-item :value i :label "hub"))))
          (dotimes (i (* h k))
            (setf (aref target-ids i)
                  (id (make-s-item :value i :label "target")))))
        (let ((start (get-internal-real-time)))
          (dotimes (i h)
            (with-transaction ()
              (let ((hub (lookup-vertex (aref hub-ids i))))
                (dotimes (j k)
                  (make-s-link :from hub
                               :to   (lookup-vertex (aref target-ids (+ (* i k) j))))))))
          (record-throughput "large-edge-insert" (* h k)
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second))))
        (is (= (* h k)
               (length (map-edges #'identity g :collect-p t :edge-type 's-link)))
            "Total edge count mismatch")
        (is (loop for i below h
                  always (= k (length (outgoing-edges (lookup-vertex (aref hub-ids i))))))
            "Some hub has wrong outgoing-edges count")))))

;;; ---------------------------------------------------------------------------
;;; Test 3: Prolog is-a query over a large vertex set
;;;
;;; Insert N s-item vertices then query (is-a ?x s-item) with select-flat.
;;; The result count must equal N.
;;; ---------------------------------------------------------------------------

(test prolog-is-a-large-dataset
  "select-flat (is-a ?x s-item) over N vertices must return N results."
  (let ((n (scale 500 50)))
    (with-stress-graph (g)
      (with-transaction ()
        (dotimes (i n)
          (make-s-item :value i :label "prolog-item")))
      (let* ((start   (get-internal-real-time))
             (results (select-flat (?x) (is-a ?x s-item)))
             (elapsed (/ (- (get-internal-real-time) start)
                         (float internal-time-units-per-second))))
        (record-throughput "prolog-is-a-query" n elapsed)
        (is (= n (length results))
            "is-a query returned ~D results; expected ~D" (length results) n)))))

;;; ---------------------------------------------------------------------------
;;; Test 4: graph reopen (persistence round-trip)
;;;
;;; Insert N vertices and E edges, close the graph cleanly, reopen it, and
;;; verify that all data is still present.
;;; ---------------------------------------------------------------------------

(test reopen-large-graph
  "N vertices + E edges survive close + reopen unchanged."
  (let* ((n (scale 500 50))
         (e (scale 200 20)))
    (with-temp-directory (dir)
      (let ((v-ids (make-array n))
            (e-ids (make-array e)))
        ;; Populate and close.
        (let ((g (make-graph *stress-graph-name* (namestring dir)
                             :buffer-pool-size 2000)))
          (unwind-protect
               (let ((*graph* g))
                 (with-transaction ()
                   (dotimes (i n)
                     (setf (aref v-ids i)
                           (id (make-s-item :value i :label "persist")))))
                 (with-transaction ()
                   (dotimes (i e)
                     (setf (aref e-ids i)
                           (id (make-s-link :from (lookup-vertex (aref v-ids (mod i n)))
                                            :to   (lookup-vertex (aref v-ids (mod (* i 2) n)))))))))
            (close-graph g :snapshot-p nil)))
        ;; Reopen and verify.
        (let ((g2 (graph-db::open-graph *stress-graph-name* (namestring dir))))
          (unwind-protect
               (let ((*graph* g2))
                 (is (= n (length (map-vertices #'identity g2 :collect-p t
                                                :vertex-type 's-item)))
                     "Vertex count mismatch after reopen: expected ~D" n)
                 (is (= e (length (map-edges #'identity g2 :collect-p t
                                             :edge-type 's-link)))
                     "Edge count mismatch after reopen: expected ~D" e)
                 ;; Spot-check 10 random vertex IDs.
                 (let ((sample (loop repeat (min 10 n)
                                     collect (aref v-ids (random n)))))
                   (is (every (lambda (vid) (not (null (lookup-vertex vid)))) sample)
                       "Some vertices not found by ID after reopen")))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))
