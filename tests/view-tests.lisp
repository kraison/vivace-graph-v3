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
  ;; The same map view sorted :greaterp (descending) -- exercises the
  ;; greaterp + :key / range path (issue #18).
  (def-view people-by-name-desc :greaterp (g-person :graph-db-integration-test)
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

;;; ---------------------------------------------------------------------------
;;; :greaterp (descending) map views  --  regression for issue #18
;;; ---------------------------------------------------------------------------

(test greaterp-map-view-lookup-by-key
  "Regression for issue #18: a :greaterp map view returns the matching node for
a :key lookup (it previously returned nothing because the per-key range bounds
were not reversed for descending order)."
  (with-test-graph (g)
    (define-test-views)
    (let (bob-id)
      (with-transaction ()
        (make-g-person :name "Alice")
        (setq bob-id (id (make-g-person :name "Bob")))
        (make-g-person :name "Carol"))
      (let ((hits (invoke-graph-view 'g-person 'people-by-name-desc :key "Bob")))
        (is (= 1 (length hits)) "greaterp view :key must find the node (issue #18)")
        (is (string= "Bob" (cdr (assoc :key (first hits)))))
        (is (equalp bob-id (cdr (assoc :id (first hits))))))
      ;; and a key with no entry still yields nothing
      (is (null (invoke-graph-view 'g-person 'people-by-name-desc :key "Nobody"))))))

(test greaterp-vs-lessp-scan-order
  "A full scan of a :greaterp view is descending by key; the :lessp view ascends."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (make-g-person :name "Alice")
      (make-g-person :name "Bob")
      (make-g-person :name "Carol"))
    (flet ((keys (view)
             (mapcar (lambda (h) (cdr (assoc :key h)))
                     (invoke-graph-view 'g-person view))))
      (is (equal '("Alice" "Bob" "Carol") (keys 'people-by-name)))
      (is (equal '("Carol" "Bob" "Alice") (keys 'people-by-name-desc))))))

(test greaterp-map-view-range
  "A :start-key/:end-key range on a :greaterp view returns the descending slice
(start = high key, end = low key)."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (dolist (n '("a" "b" "c" "d" "e")) (make-g-person :name n)))
    (let ((slice (mapcar (lambda (h) (cdr (assoc :key h)))
                         (invoke-graph-view 'g-person 'people-by-name-desc
                                            :start-key "d" :end-key "b"))))
      (is (equal '("d" "c" "b") slice)
          "descending slice from d down to b should be (d c b); got ~S" slice))))

;;; ---------------------------------------------------------------------------
;;; View maintenance: updates, deletions, paging, delete-view, persistence
;;; ---------------------------------------------------------------------------

(test view-reflects-slot-update
  "Updating a node's indexed slot moves it in the view: the old key loses the
entry and the new key gains it (update-in-views: remove + re-add)."
  (with-test-graph (g)
    (define-test-views)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "Alice"))))
      (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Alice"))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'name) "Alicia")
          (save v)))
      (is (null (invoke-graph-view 'g-person 'people-by-name :key "Alice"))
          "old key should no longer be indexed")
      (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Alicia")))
          "new key should be indexed"))))

(test view-reflects-deletion
  "Deleting a node removes it from the view (remove-from-views)."
  (with-test-graph (g)
    (define-test-views)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "Gone"))))
      (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "Gone"))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (is (null (invoke-graph-view 'g-person 'people-by-name :key "Gone"))
          "deleted node should not appear in the view"))))

(test map-view-count-and-skip-paging
  "A view scan honours :count (limit) and :skip (offset), and they compose."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (dolist (n '("a" "b" "c" "d" "e")) (make-g-person :name n)))
    (flet ((ks (&rest args)
             (mapcar (lambda (h) (cdr (assoc :key h)))
                     (apply #'invoke-graph-view 'g-person 'people-by-name args))))
      (is (equal '("a" "b") (ks :count 2)) ":count limits results")
      (is (equal '("c" "d" "e") (ks :skip 2)) ":skip offsets results")
      (is (equal '("b" "c") (ks :skip 1 :count 2)) ":skip + :count page"))))

(test delete-view-then-invoke-signals
  "After delete-view the view is gone and invoking it signals invalid-view-error."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction () (make-g-person :name "X"))
    (is (= 1 (length (invoke-graph-view 'g-person 'people-by-name :key "X"))))
    (delete-view g 'g-person 'people-by-name)
    (signals graph-db::invalid-view-error
      (invoke-graph-view 'g-person 'people-by-name :key "X"))))

(test invoke-nonexistent-view-signals
  "Invoking a view that was never defined signals invalid-view-error."
  (with-test-graph (g)
    (define-test-views)
    (signals graph-db::invalid-view-error
      (invoke-graph-view 'g-person 'no-such-view :key "X"))))

(test views-persist-across-reopen
  "A view's definition and index survive close-graph + open-graph: it is
restored (restore-views) and remains queryable without re-defining it."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) id)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (define-test-views)
          (with-transaction ()
            (setq id (id (make-g-person :name "Persisted")))
            (make-g-person :name "Other")))
        (close-graph g :snapshot-p nil))
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((hits (invoke-graph-view 'g-person 'people-by-name :key "Persisted")))
                 (is (= 1 (length hits)) "view should be restored and queryable")
                 (is (equalp id (cdr (assoc :id (first hits))))))
               (is (= 2 (length (invoke-graph-view 'g-person 'people-by-name)))
                   "restored view sees all indexed nodes"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test reduced-view-count-limits-groups
  "map-reduced-view honours :count, limiting the number of reduced groups."
  (with-test-graph (g)
    (define-test-views)
    (with-transaction ()
      (let ((p1 (make-g-person :name "P1"))
            (p2 (make-g-person :name "P2"))
            (p3 (make-g-person :name "P3"))
            (liker (make-g-person :name "Liker")))
        (make-g-likes :from liker :to p1)
        (make-g-likes :from liker :to p2)
        (make-g-likes :from liker :to p3)))
    (let ((all (map-reduced-view (lambda (k id v) (declare (ignore id v)) k)
                                 'g-likes 'likes-received :collect-p t))
          (two (map-reduced-view (lambda (k id v) (declare (ignore id v)) k)
                                 'g-likes 'likes-received :count 2 :collect-p t)))
      (is (= 3 (length all)) "three liked targets -> three groups")
      (is (= 2 (length two)) ":count 2 limits to two groups"))))
