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
