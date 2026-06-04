;;;; ACID-DURABILITY-SUITE
;;;;
;;;; Verifies that committed data survives close/reopen cycles and that the
;;;; .dirty crash sentinel is set and checked correctly.  The crash-after-
;;;; partial-apply test uses *after-apply-tx-writes-hook* to simulate a process
;;;; death between the lhash write and the view update, then verifies that
;;;; open-graph's automatic recover-transactions call makes the graph consistent.

(in-package #:graph-db/acid-test)

(def-suite acid-durability-suite
  :description "Durability: crash sentinel, close/reopen, and WAL recovery."
  :in acid-suite)

(in-suite acid-durability-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: dirty flag detected on reopen
;;;
;;; make-graph writes a .dirty marker.  While the graph is still open (not
;;; closed), calling open-graph on the same path must signal an error — the
;;; system correctly refuses to open an unclean graph.
;;; ---------------------------------------------------------------------------

(test dirty-flag-detected-on-reopen
  "open-graph must signal an error when a .dirty marker is present."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          g)
      (unwind-protect
           (progn
             (setq g (make-graph *acid-graph-name* path :buffer-pool-size 1000))
             ;; Graph is open; .dirty exists.  Attempting to open the same path
             ;; must fail.
             (let ((*graph* g))
               (signals error
                 (open-graph *acid-graph-name* path))))
        (when g
          (ignore-errors (close-graph g :snapshot-p nil)))
        (collect-garbage)))))

;;; ---------------------------------------------------------------------------
;;; Test 2: data survives clean close and reopen
;;;
;;; Commit a vertex and an edge, close the graph cleanly, then reopen it.
;;; Both the vertex (with its slot values) and the edge (with its adjacency
;;; information) must be present and correct after the round-trip.
;;; ---------------------------------------------------------------------------

(test data-survives-close-reopen
  "Committed data (vertex slot values, edge adjacency) survives close-graph + open-graph."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          src-id dst-id)
      ;; --- populate and close ---
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (let ((src (make-ac-item :value 42 :label "src"))
                  (dst (make-ac-item :value 7  :label "dst")))
              (setq src-id (id src)
                    dst-id (id dst))
              (make-ac-link :from src :to dst)))
          (close-graph g :snapshot-p nil)))
      ;; --- reopen and verify ---
      (let ((g2 (open-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               ;; Vertex slot values
               (let ((src (lookup-vertex src-id)))
                 (is-true src "Source vertex must be present after reopen")
                 (is (= 42 (slot-value src 'value)) "src.value must be 42")
                 (is (string= "src" (slot-value src 'label)) "src.label must be \"src\""))
               (let ((dst (lookup-vertex dst-id)))
                 (is-true dst "Destination vertex must be present after reopen")
                 (is (= 7 (slot-value dst 'value)) "dst.value must be 7"))
               ;; Edge adjacency
               (is (= 1 (length (map-edges #'identity g2
                                           :collect-p t :edge-type 'ac-link)))
                   "Edge must survive reopen"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Test 3: crash after partial apply — recovery makes graph consistent
;;;
;;; *after-apply-tx-writes-hook* is set to signal an error after apply-tx-writes
;;; runs (lhash updated, heap written, type-index updated) but before
;;; apply-tx-writes-to-views (view index not updated).  The .txn file is left
;;; on disk because cleanup-transaction sees state :committing (not :committed)
;;; and calls remove-transaction instead of mark-as-committed.
;;;
;;; The graph is then closed cleanly (flushing on-disk structures, removing
;;; .dirty).  open-graph automatically calls recover-transactions, which replays
;;; the .txn file: apply-tx-writes is idempotent (duplicate-key-error handled),
;;; and apply-tx-writes-to-views finally updates the view.
;;;
;;; After recovery the vertex must be in the lhash AND the view must be
;;; consistent (vertex appears in the view).
;;; ---------------------------------------------------------------------------

(test crash-after-partial-apply
  "Recovery via recover-transactions makes a partial-apply consistent: lhash and views agree."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          vid)
      ;; --- Phase 1: create graph, arm the crash hook, commit ---
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (define-acid-views)
               ;; Set the hook: fires once after apply-tx-writes, before apply-tx-writes-to-views
               (setf *after-apply-tx-writes-hook*
                     (lambda () (error "simulated crash after lhash write")))
               ;; The transaction will succeed at the lhash level but error at the view level.
               ;; The error propagates to the caller; the .txn file is NOT deleted.
               (handler-case
                   (with-transaction ()
                     (let ((v (make-ac-item :value 77 :label "durable")))
                       (setq vid (id v))))
                 (error () nil))
               ;; Verify the hook self-cleared
               (is (null *after-apply-tx-writes-hook*)
                   "Hook must have self-cleared after firing"))
          ;; Close the graph cleanly: flushes on-disk structures, removes .dirty
          ;; The .txn file in tx/ is NOT affected by close-graph.
          (ignore-errors (close-graph g :snapshot-p nil))
          ;; Clear the hook in case of unexpected error paths
          (setf *after-apply-tx-writes-hook* nil)))
      ;; --- Phase 2: open-graph runs recover-transactions automatically ---
      (let ((g2 (open-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               ;; The vertex must be in the lhash (was there before crash, and replay is idempotent)
               (let ((v (lookup-vertex vid)))
                 (is-true v "Vertex must be in lhash after recovery")
                 (when v
                   (is (= 77 (slot-value v 'value))
                       "Vertex slot value must be 77 after recovery")))
               ;; The view must now be consistent (replay applied apply-tx-writes-to-views)
               (let ((view-hits (map-view #'list 'ac-item 'ac-item-by-value
                                         :graph g2
                                         :collect-p t
                                         :start-key 77 :end-key 77)))
                 (is (= 1 (length view-hits))
                     "View must contain the recovered vertex (got ~D entries)" (length view-hits))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Test 4: an UPDATE's .txn log carries the NEW data, so recovery restores it.
;;;
;;; update-node serializes the write from the copy's BYTES; mutating a slot
;;; updates DATA but not BYTES, so without refreshing bytes the logged write
;;; carried the OLD value.  The live graph hid this (apply-tx-write
;;; re-serializes its own copy), but replaying the .txn during recovery would
;;; restore the stale value.  Crash mid-update (after apply-tx-writes, before
;;; the .txn is cleaned), then recover and confirm the NEW value survives.
;;; ---------------------------------------------------------------------------

(test update-survives-crash-recovery
  "Recovery replays an update's .txn with the NEW slot values, not the stale
copy bytes (regression for update-node serializing a copy's pre-modification
bytes)."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          vid)
      ;; --- Phase 1: commit an insert cleanly, then crash mid-UPDATE ---
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (setq vid (id (make-ac-item :value 1 :label "before"))))
               ;; Crash after apply-tx-writes so the update's .txn is left
               ;; pending for recovery to replay.
               (setf *after-apply-tx-writes-hook*
                     (lambda () (error "simulated crash after lhash write")))
               (handler-case
                   (with-transaction ()
                     (let ((v (copy (lookup-vertex vid))))
                       (setf (slot-value v 'value) 2)
                       (setf (slot-value v 'label) "after")
                       (save v)))
                 (error () nil))
               (is (null *after-apply-tx-writes-hook*)
                   "Hook must have self-cleared after firing"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (setf *after-apply-tx-writes-hook* nil)))
      ;; --- Phase 2: reopen -> recover-transactions replays the update .txn ---
      (let ((g2 (open-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((v (lookup-vertex vid)))
                 (is-true v "Updated vertex must be present after recovery")
                 (when v
                   (is (= 2 (slot-value v 'value))
                       "Recovered update must hold the NEW value 2, not the stale 1")
                   (is (string= "after" (slot-value v 'label))
                       "Recovered update must hold the NEW label"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
