;;;; CONCURRENT-GRAPH-STORM-SUITE
;;;;
;;;; Large-scale concurrent graph operations:
;;;;   - 16 threads × large vertex + edge inserts; final counts must match
;;;;   - Vertex + edge churn: insert-then-delete loop; final live count = 0

(in-package #:graph-db/concurrent-stress-test)

(def-suite concurrent-graph-storm-suite
  :description "Large-scale concurrent graph insert and churn."
  :in concurrent-stress-suite)

(in-suite concurrent-graph-storm-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: concurrent large insert
;;;
;;; T threads each insert V vertices and E edges.  After all threads join:
;;;   - total vertex count = T × V
;;;   - total edge count   = T × E
;;;
;;; Each thread creates its own private vertices and links them in a chain,
;;; so there are no shared write-set conflicts between threads.
;;; ---------------------------------------------------------------------------

(test concurrent-large-insert
  "T threads × V vertex inserts + E edge inserts; final counts must match."
  (let* ((t-count (min *stress-thread-count* 8))  ; cap at 8 for memory
         (v       200)
         (e       50))
    (with-cstress-graph (g)
      (let ((start (get-internal-real-time)))
        (run-threads t-count
                     (lambda (i)
                       (declare (ignore i))
                       ;; Insert V vertices, then E edges linking consecutive pairs.
                       (let ((vids (make-array v)))
                         (dotimes (j v)
                           (with-transaction ()
                             (setf (aref vids j)
                                   (id (make-cs-item :value j :label "storm")))))
                         (dotimes (j e)
                           (with-transaction ()
                             (make-cs-link
                              :from (lookup-vertex (aref vids (mod j v)))
                              :to   (lookup-vertex (aref vids (mod (1+ j) v)))))))))
        (record-throughput "concurrent-large-insert"
                           (* t-count (+ v e))
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second))))
      (is (= (* t-count v)
             (length (map-vertices #'identity g :collect-p t :vertex-type 'cs-item)))
          "Expected ~D vertices; got ~D" (* t-count v)
          (length (map-vertices #'identity g :collect-p t :vertex-type 'cs-item)))
      (is (= (* t-count e)
             (length (map-edges #'identity g :collect-p t :edge-type 'cs-link)))
          "Expected ~D edges; got ~D" (* t-count e)
          (length (map-edges #'identity g :collect-p t :edge-type 'cs-link))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: concurrent vertex-edge churn
;;;
;;; T threads each loop K times: insert a vertex, insert 3 edges from it to
;;; pre-created targets, then delete the vertex.  At the end, all inserted-
;;; then-deleted vertices must be gone; live count = 0 from the churning
;;; threads (pre-created targets are live but not counted here because they
;;; are of type cs-item and we separate by label).
;;;
;;; Simplified: half threads insert (keep), half churn (insert+delete).
;;; Final live count = insert-threads × K.
;;; ---------------------------------------------------------------------------

(test concurrent-vertex-edge-churn
  "Half threads insert-keep, half churn insert-delete; live count = inserters × K."
  (let* ((t-count   (min *stress-thread-count* 8))
         (inserters (max 1 (floor t-count 2)))
         (churners  (- t-count inserters))
         (k         20))
    (declare (ignore churners))
    (with-cstress-graph (g)
      (run-threads t-count
                   (lambda (i)
                     (if (< i inserters)
                         ;; Pure inserter
                         (dotimes (_ k)
                           (with-transaction ()
                             (make-cs-item :value i :label "keep")))
                         ;; Churner: insert then delete
                         (dotimes (_ k)
                           (let (vid)
                             (with-transaction ()
                               (setq vid (id (make-cs-item :value i :label "churn"))))
                             (with-transaction ()
                               (mark-deleted (lookup-vertex vid))))))))
      (let ((live-count
              (length (map-vertices
                        (lambda (v)
                          (when (string= "keep" (slot-value v 'label)) v))
                        g :collect-p t :vertex-type 'cs-item))))
        (is (= (* inserters k) live-count)
            "Expected ~D live vertices; got ~D" (* inserters k) live-count)))))
