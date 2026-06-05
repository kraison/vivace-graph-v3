;;;; Unit tests for the transaction write path (transactions.lisp,
;;;; primitive-node.lisp) -- the copy/save/update/delete machinery and its error
;;;; conditions and revision handling.  This is the code MVCC will rewrite, so
;;;; it targets the branches (update vs create vs delete, the guards) directly.
;;;;
;;;; Reuses the g-person / g-knows schema from graph-tests.lisp.

(in-package #:graph-db/test)

(def-suite write-path-suite
  :description "Transaction write path: copy/save/update/delete, revisions, guards."
  :in graph-db-suite)

(in-suite write-path-suite)

;;; ---------------------------------------------------------------------------
;;; Guards / error conditions
;;; ---------------------------------------------------------------------------

(test save-outside-transaction-signals
  "SAVE with no transaction in progress signals no-transaction-in-progress."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "X"))))
      (signals graph-db:no-transaction-in-progress
        (save (lookup-vertex id))))))

(test save-non-copy-signals-modifying-non-copy
  "SAVE of a node that was not produced by COPY in this transaction signals
modifying-non-copy."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "X"))))
      (with-transaction ()
        (signals graph-db::modifying-non-copy
          (save (lookup-vertex id)))))))

(test re-deleting-vertex-signals
  "mark-deleted on an already-deleted vertex signals vertex-already-deleted-error."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "X"))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (signals graph-db::vertex-already-deleted-error
        (mark-deleted (lookup-vertex id))))))

(test re-deleting-edge-signals
  "mark-deleted on an already-deleted edge signals edge-already-deleted-error."
  (with-test-graph (g)
    (let (eid a b)
      (with-transaction ()
        (setq a (make-g-person :name "A") b (make-g-person :name "B"))
        (setq eid (id (make-g-knows :from a :to b))))
      (with-transaction () (mark-deleted (lookup-edge eid)))
      (signals graph-db::edge-already-deleted-error
        (mark-deleted (lookup-edge eid))))))

;;; ---------------------------------------------------------------------------
;;; Update / revision / copy semantics
;;; ---------------------------------------------------------------------------

(test update-via-copy-increments-revision
  "Updating through copy+save bumps the node's revision and persists the new
slot value."
  (with-test-graph (g)
    (let (id r0)
      (with-transaction () (setq id (id (make-g-person :name "before" :age 1))))
      (setq r0 (graph-db::revision (lookup-vertex id)))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'name) "after")
          (save v)))
      (let ((v (lookup-vertex id)))
        (is (string= "after" (slot-value v 'name)))
        (is (= (1+ r0) (graph-db::revision v))
            "revision should increment by exactly 1 (was ~A)" r0)))))

(test copy-without-save-leaves-node-unchanged
  "Mutating a COPY but not SAVEing it records no write; the stored node is
unchanged."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "orig" :age 5))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (slot-value v 'name) "scratch")))   ; deliberately no SAVE
      (is (string= "orig" (slot-value (lookup-vertex id) 'name))
          "an unsaved copy must not affect the stored node"))))

(test update-edge-slot-persists
  "Updating an edge data slot through copy+save persists."
  (with-test-graph (g)
    (let (eid a b)
      (with-transaction ()
        (setq a (make-g-person :name "A") b (make-g-person :name "B"))
        (setq eid (id (make-g-knows :from a :to b :since "2020"))))
      (with-transaction ()
        (let ((e (copy (lookup-edge eid))))
          (setf (slot-value e 'since) "2021")
          (save e)))
      (is (string= "2021" (slot-value (lookup-edge eid) 'since))))))

;;; ---------------------------------------------------------------------------
;;; Delete semantics
;;; ---------------------------------------------------------------------------

(test delete-edge-removes-from-scan
  "A deleted edge disappears from a live map-edges scan."
  (with-test-graph (g)
    (let (eid a b)
      (with-transaction ()
        (setq a (make-g-person :name "A") b (make-g-person :name "B"))
        (setq eid (id (make-g-knows :from a :to b))))
      (with-transaction () (mark-deleted (lookup-edge eid)))
      (is (null (map-edges #'identity g :collect-p t))
          "deleted edge should not appear in a live scan"))))

(test generated-lookup-filters-deleted
  "lookup-<type> hides deleted nodes unless :include-deleted-p is set."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "X"))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (is (null (lookup-g-person id)) "lookup-g-person should hide a deleted node")
      (let ((d (lookup-g-person id :include-deleted-p t)))
        (is-true d ":include-deleted-p should return the deleted node")
        (when d (is-true (deleted-p d)))))))

;;; ---------------------------------------------------------------------------
;;; Mixed transaction (create + update + delete together)
;;; ---------------------------------------------------------------------------

(test mixed-transaction-applies-all
  "A single transaction that creates one node, updates an existing one, and
deletes another applies all three write kinds atomically."
  (with-test-graph (g)
    (let (keep-id del-id)
      (with-transaction ()
        (setq keep-id (id (make-g-person :name "keep" :age 1)))
        (setq del-id (id (make-g-person :name "del" :age 2))))
      (with-transaction ()
        (make-g-person :name "new" :age 3)                       ; create
        (let ((v (copy (lookup-vertex keep-id))))                ; update
          (setf (slot-value v 'age) 99)
          (save v))
        (mark-deleted (lookup-vertex del-id)))                   ; delete
      (is (equal '("keep" "new")
                 (sort (mapcar (lambda (v) (slot-value v 'name))
                               (map-vertices #'identity g :collect-p t
                                                        :vertex-type 'g-person))
                       #'string<)))
      (is (= 99 (slot-value (lookup-vertex keep-id) 'age))))))
