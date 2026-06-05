;;;; Tests for the index-backed spatial queries (spatial-query.lisp): the Lisp
;;;; API (find-nodes-within / find-nodes-near) and the Prolog functors
;;;; (find-within/2, find-near/4).  Reuses the GEO-PLACE vertex + NODE-GEOMETRY
;;;; method defined in spatial-hook-tests.lisp.

(in-package #:graph-db/test)

(def-suite spatial-query-suite
  :description "Index-backed spatial queries: find-nodes-within/near + functors."
  :in graph-db-suite)

(in-suite spatial-query-suite)

;; Three places: A and B are ~400 m apart in Kharkiv Oblast; FAR is in Lviv.
(defparameter *q-a*   '(37.1724312d0 49.2020584d0))   ; lon lat
(defparameter *q-b*   '(37.1773283d0 49.2036314d0))
(defparameter *q-far* '(23.7182919d0 50.0263233d0))

(defmacro with-three-places ((g ida idb idfar) &body body)
  `(with-test-graph (,g)
     (let (,ida ,idb ,idfar)
       (with-transaction ()
         (setq ,ida   (id (make-geo-place :loc (make-point (first *q-a*) (second *q-a*))))
               ,idb   (id (make-geo-place :loc (make-point (first *q-b*) (second *q-b*))))
               ,idfar (id (make-geo-place :loc (make-point (first *q-far*) (second *q-far*))))))
       ;; LOCALLY so a test body may open with (declare (ignore ...)).
       (locally ,@body))))

(defun id-set (nodes)
  (mapcar #'id nodes))

(defun has-id-p (id nodes)
  (member id (id-set nodes) :test 'equalp))

;;; ---- Lisp API ----------------------------------------------------------

(test find-nodes-near-lisp
  "find-nodes-near returns local nodes within radius (nearest first), not distant."
  (with-three-places (g ida idb idfar)
    (let* ((hits (find-nodes-near (second *q-a*) (first *q-a*) 600d0 :graph g))
           (nodes (mapcar #'car hits)))
      (is (has-id-p ida nodes))
      (is (has-id-p idb nodes))
      (is (not (has-id-p idfar nodes)))
      ;; distances are sorted ascending; the self-node (A) is nearest
      (is (equalp ida (id (car (first hits)))))
      (is (every (lambda (nd) (<= (cdr nd) 600d0)) hits)))))

(test find-nodes-near-tight-radius
  "A 100 m radius excludes the ~400 m neighbour."
  (with-three-places (g ida idb idfar)
    (declare (ignore idfar))
    (let ((nodes (mapcar #'car (find-nodes-near (second *q-a*) (first *q-a*) 100d0 :graph g))))
      (is (has-id-p ida nodes))
      (is (not (has-id-p idb nodes))))))

(test find-nodes-within-lisp
  "find-nodes-within returns nodes inside the task-area polygon only."
  (with-three-places (g ida idb idfar)
    (let* ((aoi (make-polygon '(((37.170 49.200) (37.180 49.200)
                                 (37.180 49.206) (37.170 49.206)
                                 (37.170 49.200)))))
           (nodes (find-nodes-within aoi :graph g)))
      (is (has-id-p ida nodes))
      (is (has-id-p idb nodes))
      (is (not (has-id-p idfar nodes))))))

(test deleted-nodes-excluded
  "A deleted node is not returned by spatial queries."
  (with-three-places (g ida idb idfar)
    (declare (ignore idb idfar))
    (is (has-id-p ida (mapcar #'car (find-nodes-near (second *q-a*) (first *q-a*) 600d0 :graph g))))
    (with-transaction () (mark-deleted (lookup-vertex ida)))
    (is (not (has-id-p ida (mapcar #'car (find-nodes-near (second *q-a*) (first *q-a*) 600d0 :graph g)))))))

;;; ---- Prolog functors ---------------------------------------------------

(test find-near-functor
  "find-near/4 yields the local nodes in a query, composing with is-a."
  (with-three-places (g ida idb idfar)
    (declare (ignore g))
    (let ((ids (id-set (select-flat (?n)
                         (is-a ?n geo-place)
                         (find-near ?n 49.2020584d0 37.1724312d0 600d0)))))
      (is (member ida ids :test 'equalp))
      (is (member idb ids :test 'equalp))
      (is (not (member idfar ids :test 'equalp))))))

(test find-within-functor
  "find-within/2 yields nodes inside an area (area built via the is/2 escape)."
  (with-three-places (g ida idb idfar)
    (declare (ignore g))
    (let ((ids (id-set (select-flat (?n)
                         (is ?area (make-polygon '(((37.170d0 49.200d0) (37.180d0 49.200d0)
                                                    (37.180d0 49.206d0) (37.170d0 49.206d0)
                                                    (37.170d0 49.200d0)))))
                         (find-within ?n ?area)))))
      (is (member ida ids :test 'equalp))
      (is (member idb ids :test 'equalp))
      (is (not (member idfar ids :test 'equalp))))))
