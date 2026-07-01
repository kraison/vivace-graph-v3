;;;; Durable Lamport clock (peer replication, Branch B / PT-8).
;;;;
;;;; A peer-graph's logical clock must be monotonic ACROSS restarts: if it reset
;;;; to 0 after a crash, the replica's post-restart authored ops would get tiny
;;;; stamps and silently lose every LWW race (safety data dropped).  So the
;;;; counter is persisted on every advance and reloaded on open, and advanced to
;;;; MAX(local, received) whenever an op is applied.  Single-image test on a
;;;; device-role peer-graph (which mints a stamp per local commit); the multi-
;;;; writer conflict behaviour that consumes these stamps is tested by the
;;;; process-based peer harnesses.  Reuses the g-person schema from graph-tests.

(in-package #:graph-db/test)

(def-suite peer-lamport-suite
  :description "Durable, monotonic Lamport clock (PT-8)."
  :in graph-db-suite)

(in-suite peer-lamport-suite)

(defparameter *lamport-origin*
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 3)
  "A fixed device origin id for the lamport test.")

(test lamport-clock-persists-and-recovers
  "The Lamport counter advances on each authored commit, is persisted, survives a
close/reopen (never resets), jumps forward on OBSERVE, and never moves backward."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      ;; --- mint via commits; confirm advance + on-disk persistence ---
      (let ((g (make-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (is (= 0 (graph-db::lamport-counter g)) "fresh peer-graph starts at 0")
          (dotimes (i 3) (with-transaction () (make-g-person :name "l")))
          (is (= 3 (graph-db::lamport-counter g)) "three commits advance to 3")
          (is (= 3 (graph-db::load-lamport-counter g)) "the counter is on disk"))
        (close-graph g :snapshot-p nil))
      ;; --- reopen: recovers, does not reset to 0 ---
      (let ((g (open-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0)))
        (unwind-protect
             (let ((*graph* g))
               (is (= 3 (graph-db::lamport-counter g)) "reopen recovers the counter")
               ;; observe a higher received stamp -> jump; next mint is after it
               (graph-db::peer-observe-lamport g 10)
               (is (= 10 (graph-db::lamport-counter g)) "observe jumps forward")
               (with-transaction () (make-g-person :name "l"))
               (is (= 11 (graph-db::lamport-counter g)) "next mint is after the observed stamp")
               ;; a lower received stamp is a no-op (monotonic)
               (graph-db::peer-observe-lamport g 5)
               (is (= 11 (graph-db::lamport-counter g)) "a lower observe never moves it back"))
          (close-graph g :snapshot-p nil))))))
