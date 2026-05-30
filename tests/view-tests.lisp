;;;; Tests for map and map-reduce views (views.lisp).
;;;;
;;;; Views are defined against a *live* graph, and are maintained as nodes are
;;;; saved -- so each test defines its views (via DEFINE-TEST-VIEWS) on the
;;;; fresh WITH-TEST-GRAPH graph *before* inserting data.  Reuses the schema
;;;; (g-person, g-likes) defined in graph-tests.lisp.

(in-package #:graph-db/test)

(defun define-test-views ()
  "Define the views used by this suite against the current *graph*."
  ;; A simple map view: persons indexed by name.
  (def-view people-by-name :lessp (g-person :graph-db-integration-test)
    (:map (lambda (p)
            (when (slot-value p 'name)
              (yield (slot-value p 'name) nil)))))
  ;; A map-reduce view: number of likes received, keyed by target id.
  (def-view likes-received :greaterp (g-likes :graph-db-integration-test)
    (:map (lambda (e) (yield (string-id (to e)) 1)))
    (:reduce (lambda (keys vals) (declare (ignore keys)) (apply #'+ vals)))))

(def-suite view-suite
  :description "map and map-reduce views."
  :in graph-db-suite)

(in-suite view-suite)

(test map-view-lookup-by-key
  "A map view returns, for a key, the ids of the nodes that yielded it."
  (with-test-graph (g)
    (define-test-views)
    (let (alice-id)
      (with-transaction ()
        (setq alice-id (id (make-g-person :name "Alice")))
        (make-g-person :name "Bob"))
      (let ((hits (invoke-graph-view 'g-person 'people-by-name :key "Alice")))
        (is (= 1 (length hits)))
        (is (string= "Alice" (cdr (assoc :key (first hits)))))
        (is (equalp alice-id (cdr (assoc :id (first hits)))))))))

(test map-view-missing-key
  (with-test-graph (g)
    (define-test-views)
    (with-transaction () (make-g-person :name "Alice"))
    (is (null (invoke-graph-view 'g-person 'people-by-name :key "Nobody")))))

(test map-view-reflects-new-nodes
  "Nodes inserted after the view exists are indexed incrementally."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction () (make-g-person :name "Zed"))
    (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Zed"))))
    (with-transaction () (make-g-person :name "Zed"))
    (is (= 2 (length (invoke-graph-view 'g-person 'people-by-name :key "Zed"))))))

(test reduce-view-sums-per-key
  "A map-reduce view aggregates values per key (likes received per target)."
  (with-test-graph (g)
    (define-test-views)
    (let (pie-id cake-id)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B"))
              (c (make-g-person :name "C"))
              (pie (make-g-person :name "Pie"))
              (cake (make-g-person :name "Cake")))
          (setq pie-id (id pie) cake-id (id cake))
          (make-g-likes :from a :to pie)
          (make-g-likes :from b :to pie)
          (make-g-likes :from c :to cake)))
      (let ((counts (map-reduced-view (lambda (key id value)
                                        (declare (ignore id))
                                        (cons key value))
                                      'g-likes 'likes-received
                                      :collect-p t)))
        (is (= 2 (cdr (assoc (string-id pie-id) counts :test #'string=))))
        (is (= 1 (cdr (assoc (string-id cake-id) counts :test #'string=))))))))
