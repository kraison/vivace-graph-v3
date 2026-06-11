;;;; ACID-ATOMICITY-SUITE
;;;;
;;;; Verifies that transactions are all-or-nothing:
;;;;   - A non-local exit rolls back everything the transaction touched.
;;;;   - Retries on conflict re-execute the body and read fresh state.

(in-package #:graph-db/acid-test)

(def-suite acid-atomicity-suite
  :description "Transaction atomicity: rollback and retry correctness."
  :in acid-suite)

(in-suite acid-atomicity-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: rollback on exception
;;;
;;; Signal an error inside with-transaction.  The vertex created before the
;;; error must not appear in the lhash or type index after the unwind.
;;; ---------------------------------------------------------------------------

(test rollback-on-exception
  "Error inside with-transaction must roll back; created vertex must not survive."
  (with-acid-graph (g)
    (let (vid)
      (handler-case
          (with-transaction ()
            (let ((v (make-ac-item :value 42 :label "should-vanish")))
              (setq vid (id v))
              (error "simulated error")))
        (error () nil))
      (is (null (lookup-vertex vid))
          "Rolled-back vertex must not be in lhash")
      (is (zerop (length (map-vertices #'identity g :collect-p t :vertex-type 'ac-item)))
          "Type index must show no vertices after rollback"))))

;;; ---------------------------------------------------------------------------
;;; Test 2: rollback mid multi-write
;;;
;;; A transaction creates vertex V1, creates edge E1 from V1 to an existing
;;; vertex V0, and modifies V0's slot — then signals an error.  After unwind:
;;;   - V1 is not in the lhash
;;;   - E1 is not in the lhash
;;;   - V0's slot retains its original value
;;; ---------------------------------------------------------------------------

(test rollback-mid-multi-write
  "Rollback of a multi-write transaction: vertex, edge, and slot update all undone."
  (with-acid-graph (g)
    (let (v0-id v1-id e1-id)
      ;; Pre-populate V0 with value=7
      (with-transaction ()
        (setq v0-id (id (make-ac-item :value 7 :label "anchor"))))
      ;; Now the failing transaction
      (handler-case
          (with-transaction ()
            (let* ((v0   (copy (lookup-vertex v0-id)))
                   (v1   (make-ac-item :value 99 :label "ephemeral"))
                   (e1   (make-ac-link :from v1 :to v0)))
              (setq v1-id (id v1)
                    e1-id (id e1))
              (setf (slot-value v0 'value) 999)
              (save v0)
              (error "simulated crash")))
        (error () nil))
      ;; V0 must retain original value
      (is (= 7 (slot-value (lookup-vertex v0-id) 'value))
          "V0 slot must not reflect the rolled-back modification")
      ;; V1 must not exist
      (is (null (lookup-vertex v1-id))
          "Rolled-back vertex V1 must not be in lhash")
      ;; E1 must not exist
      (is (null (map-edges #'identity g :collect-p t))
          "Rolled-back edge E1 must not be in edge table"))))

;;; ---------------------------------------------------------------------------
;;; Test 3: retry reads fresh values
;;;
;;; T1 commits vertex V (0→42) while T2 is already in-flight (has read V=0
;;; and intends to write V+1).  T2 conflicts with T1 and retries.  The retry
;;; must read V=42 from the updated graph cache, not the stale snapshot.
;;; ---------------------------------------------------------------------------

(test retry-reads-fresh-values
  "Retry after conflict must read the committer's new value, not the stale snapshot."
  (with-acid-graph (g)
    (let (vid)
      (with-transaction ()
        (setq vid (id (make-ac-item :value 0 :label "counter"))))
      (let ((t2-read    (make-semaphore))
            (t1-done    (make-semaphore))
            (first-run  t)
            last-seen)
        ;; T1: waits for T2 to signal it has read V=0, then commits V=42
        (make-thread
         (lambda ()
           (let ((*graph* g))
             (wait-on-semaphore t2-read)
             (with-transaction ()
               (let* ((item (copy (lookup-vertex vid))))
                 (setf (slot-value item 'value) 42)
                 (save item)))
             (signal-semaphore t1-done)))
         :name "acid-t1")
        ;; T2: reads V, coordinates with T1, writes V+1 → conflicts → retries
        (with-transaction ()
          (let* ((item (copy (lookup-vertex vid)))
                 (val  (slot-value item 'value)))
            (when first-run
              (setq first-run nil)
              (signal-semaphore t2-read)
              (wait-on-semaphore t1-done))
            (setq last-seen val)
            (setf (slot-value item 'value) (1+ val))
            (save item)))
        (is (= 42 last-seen)
            "Retry must have read T1's committed value (42), not stale 0")
        (is (= 43 (slot-value (lookup-vertex vid) 'value))
            "Final value must be T2's retry write (42+1=43)")))))

;;; ---------------------------------------------------------------------------
;;; Test 4: no partial write-set visible
;;;
;;; A single transaction creates 50 vertices but errors after the 25th.  None
;;; of the vertices (including the 25 that ran before the error) should appear
;;; in the lhash or type index.
;;; ---------------------------------------------------------------------------

(test no-partial-write-set-visible
  "Error mid-transaction rolls back all creates; no partial state is committed."
  (with-acid-graph (g)
    (handler-case
        (with-transaction ()
          (dotimes (i 50)
            (make-ac-item :value i :label "partial")
            (when (= i 24)
              (error "abort halfway"))))
      (error () nil))
    (is (zerop (length (map-vertices #'identity g :collect-p t :vertex-type 'ac-item)))
        "No vertices must exist after mid-transaction rollback")))
