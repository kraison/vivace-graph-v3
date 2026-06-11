;;;; Tests for the datastore-MUTATING Prolog predicates (prolog-functors.lisp):
;;;;   - retract/1  : soft-deletes a bound vertex (mark-deleted)
;;;;   - retract/3  : soft-deletes matching edges via three index paths
;;;;                  (vev: from+to bound; ve-out: from bound; ve-in: to bound)
;;;;                  and signals on a fully-unbound (?from ?to) call
;;;;   - trigger/1  : evaluates an arbitrary Lisp form for its side effect
;;;;                  (the only current way to CREATE nodes from within Prolog,
;;;;                   since there is no native insert/assert predicate)
;;;;
;;;; Reuses the g-person / g-knows schema from graph-tests.lisp.  Queries run
;;;; against the dynamically-bound *graph* from WITH-TEST-GRAPH.  retract calls
;;;; mark-deleted, which self-wraps a transaction, so no outer with-transaction
;;;; is needed around the do-query.

(in-package #:graph-db/test)

(def-suite prolog-mutation-suite
  :description "Datastore-mutating Prolog predicates: retract/1, retract/3, trigger/1."
  :in graph-db-suite)

(in-suite prolog-mutation-suite)

;;; A special var so a (trigger ...) form -- which is EVALed in the null lexical
;;; environment -- can reach a counter from the test.
(defparameter *prolog-mut-counter* 0)

(defun person-names ()
  "Sorted list of the names of all live g-person vertices."
  (sort (mapcar (lambda (p) (slot-value p 'name))
                (select-flat (?p) (is-a ?p g-person)))
        #'string<))

(defun knows-pairs ()
  "List of (from-name . to-name) for every live g-knows edge."
  (mapcar (lambda (pair)
            (cons (slot-value (first pair) 'name)
                  (slot-value (second pair) 'name)))
          (select (:flat nil) (?a ?b) (g-knows ?a ?b))))

;;; ---------------------------------------------------------------------------
;;; retract/1 -- soft-delete a vertex matched inside a query
;;; ---------------------------------------------------------------------------

(test retract-1-soft-deletes-the-matched-vertex
  "retract/1 marks just the matched vertex deleted; the rest survive and the
deleted one disappears from is-a scans."
  (with-test-graph (g)
    (let (bid)
      (with-transaction ()
        (make-g-person :name "A")
        (setq bid (id (make-g-person :name "B")))
        (make-g-person :name "C"))
      ;; retract only B, located by name within the query
      (do-query (is-a ?p g-person)
                (node-slot-value ?p name "B")
                (retract ?p))
      (is (equal '("A" "C") (person-names))
          "only B should have been retracted")
      ;; B is gone from queries; if still resolvable by id it is flagged deleted
      (let ((b (lookup-vertex bid)))
        (is-true (or (null b) (deleted-p b))
                 "retracted vertex must be nil or deleted-p")))))

(test retract-1-can-delete-every-match
  "retract/1 driven by a bare type scan removes all vertices of that type."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-person :name "C"))
    (do-query (is-a ?p g-person) (retract ?p))
    (is (null (select-flat (?p) (is-a ?p g-person)))
        "all g-person vertices should have been retracted")))

;;; ---------------------------------------------------------------------------
;;; retract/3 -- three index paths + the unbound-args guard
;;; ---------------------------------------------------------------------------

(test retract-3-vev-deletes-only-the-named-edge
  "With from AND to bound, retract/3 (vev path) deletes exactly that one edge."
  (with-test-graph (g)
    (let (a b c)
      (with-transaction ()
        (setq a (make-g-person :name "A")
              b (make-g-person :name "B")
              c (make-g-person :name "C"))
        (make-g-knows :from a :to b)
        (make-g-knows :from a :to c))
      (do-query (is-a ?a g-person) (node-slot-value ?a name "A")
                (is-a ?b g-person) (node-slot-value ?b name "B")
                (retract g-knows ?a ?b))
      (is (equal '(("A" . "C")) (knows-pairs))
          "only the A->B edge should be gone; A->C must survive"))))

(test retract-3-ve-out-deletes-all-edges-from-source
  "With only from bound (to a var), retract/3 (ve-out path) deletes every edge
out of that vertex, leaving unrelated edges intact."
  (with-test-graph (g)
    (let (a b c)
      (with-transaction ()
        (setq a (make-g-person :name "A")
              b (make-g-person :name "B")
              c (make-g-person :name "C"))
        (make-g-knows :from a :to b)
        (make-g-knows :from a :to c)
        (make-g-knows :from b :to c))
      (do-query (is-a ?a g-person) (node-slot-value ?a name "A")
                (retract g-knows ?a ?y))
      (is (equal '(("B" . "C")) (knows-pairs))
          "both edges out of A should be gone; B->C must survive"))))

(test retract-3-ve-in-deletes-all-edges-into-target
  "With only to bound (from a var), retract/3 (ve-in path) deletes every edge
into that vertex, leaving unrelated edges intact."
  (with-test-graph (g)
    (let (a b c)
      (with-transaction ()
        (setq a (make-g-person :name "A")
              b (make-g-person :name "B")
              c (make-g-person :name "C"))
        (make-g-knows :from a :to c)
        (make-g-knows :from b :to c)
        (make-g-knows :from a :to b))
      (do-query (is-a ?c g-person) (node-slot-value ?c name "C")
                (retract g-knows ?x ?c))
      (is (equal '(("A" . "B")) (knows-pairs))
          "both edges into C should be gone; A->B must survive"))))

(test retract-3-refuses-fully-unbound-edge
  "retract/3 cowardly refuses (signals) when neither from nor to is bound."
  (with-test-graph (g)
    (with-transaction ()
      (let ((a (make-g-person :name "A"))
            (b (make-g-person :name "B")))
        (make-g-knows :from a :to b)))
    (signals error
      (do-query (retract g-knows ?x ?y)))
    ;; the edge must still be present after the refused retract
    (is (equal '(("A" . "B")) (knows-pairs)))))

;;; ---------------------------------------------------------------------------
;;; trigger/1 -- Lisp side effects from within a query
;;; ---------------------------------------------------------------------------

(test trigger-1-runs-once-per-solution
  "trigger/1 evaluates its Lisp form for each solution of the preceding goals."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-person :name "C"))
    (setf *prolog-mut-counter* 0)
    (do-query (is-a ?p g-person)
              (trigger (incf *prolog-mut-counter*)))
    (is (= 3 *prolog-mut-counter*)
        "trigger should have fired once per g-person")))

(test trigger-1-can-create-a-node
  "trigger/1 + a Lisp constructor is the current way to CREATE persistent nodes
from Prolog (there is no native insert predicate)."
  (with-test-graph (g)
    (is (null (select-flat (?p) (is-a ?p g-person))))
    (do-query (trigger (with-transaction ()
                         (make-g-person :name "Trig" :age 5))))
    (let ((people (select-flat (?p) (is-a ?p g-person))))
      (is (= 1 (length people)) "trigger should have created exactly one vertex")
      (is (string= "Trig" (slot-value (first people) 'name)))
      (is (= 5 (slot-value (first people) 'age))))))
