;;;; Replication test -- MASTER process.
;;;;
;;;; Driven by run-replication-test.sh.  Reads its configuration from the
;;;; environment (REPL_MASTER_DIR, REPL_PORT, REPL_WORK), creates a master graph
;;;; (which auto-starts replication), and runs the scenario:
;;;;
;;;;   TX1  insert two vertices + an edge   -- committed BEFORE the slave
;;;;        connects, so the slave must CATCH UP from the log.
;;;;   TX2  update a vertex's slot          -- committed AFTER the slave
;;;;   TX3  delete the other vertex            connects, so these stream LIVE.
;;;;
;;;; Coordination with the slave is via flag files under REPL_WORK.

(require :asdf)
;; Bootstrap quicklisp if the running Lisp didn't auto-load it (CCL/ECL on some
;; hosts don't); harmless when ql is already present (e.g. SBCL).
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
;; Bind *standard-output*/*error-output* to a real file around quickload: ECL's
;; first build shells out (osicat C wrappers) via run-program, which needs a
;; stream backed by a real file handle -- under the harness's stdout redirect it
;; has none, giving COMPILE-FILE-ERROR.
(with-open-file (s (merge-pathnames "build.log"
                                    (or (uiop:getenv "REPL_WORK") #p"/tmp/"))
                   :direction :output
                   :if-exists :append :if-does-not-exist :create)
  (let ((*standard-output* s) (*error-output* s))
    (ql:quickload :graph-db :silent t)))
(in-package :graph-db)
(log:config :error)

(defun mflag (name) (format nil "~A/~A" (uiop:getenv "REPL_WORK") name))
(defun write-flag (name) (with-open-file (s (mflag name) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                           (format s "~A" name)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (mflag name)) (return t))
    (sleep 0.1)))
(defun mexit (code) #+sbcl (sb-ext:exit :code code)
                    #+ccl (ccl:quit code)
                    #+ecl (ext:quit code)
                    #-(or sbcl ccl ecl) (uiop:quit code))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (let* ((dir  (uiop:getenv "REPL_MASTER_DIR"))
           (port (parse-integer (uiop:getenv "REPL_PORT")))
           (g (make-graph :repl-test-app dir :master-p t
                          :replication-port port :replication-key "test-secret"
                          :buffer-pool-size 1000)))
      (let ((*graph* g))
        ;; TX1: catch-up payload (committed before the slave exists).  Includes
        ;; two geometry places -- one in the slave's AO (Kharkiv), one outside it
        ;; (Lviv) -- so the slave's subset filter + replicated spatial index can
        ;; be checked on the catch-up path.
        (with-transaction ()
          (let ((a (make-r-person :name "Alice" :age 30))
                (b (make-r-person :name "Bob"   :age 25)))
            (make-r-knows :from a :to b :since "2020"))
          (make-r-place :label "Kharkiv site" :location (make-point 37.1724d0 49.2020d0))
          (make-r-place :label "Lviv site"    :location (make-point 23.7183d0 50.0263d0)))
        (format t "~&MASTER: TX1 committed (2 persons + 1 edge + 2 places)~%") (finish-output)
        (write-flag "ready")
        ;; wait for the slave to connect + verify catch-up
        (unless (wait-flag "connected")
          (format t "~&MASTER: timed out waiting for slave to connect~%")
          (mexit 1))
        ;; TX2 + TX3: live payload (streamed to the connected slave)
        (flet ((by-name (n)
                 (first (remove-if-not
                         (lambda (x) (string= n (slot-value x 'name)))
                         (map-vertices #'identity g :collect-p t
                                                  :vertex-type 'r-person)))))
          (with-transaction ()
            (let ((v (copy (by-name "Alice"))))
              (setf (slot-value v 'name) "Alice2")
              (save v)))
          (format t "~&MASTER: TX2 committed (Alice -> Alice2)~%") (finish-output)
          ;; A LIVE in-AO place (ordered before the age bumps, so once the slave
          ;; observes the final age=33 state it has also applied this place).
          (with-transaction ()
            (make-r-place :label "Kharkiv live" :location (make-point 37.1730d0 49.2030d0)))
          (format t "~&MASTER: live place committed (Kharkiv live)~%") (finish-output)
          ;; MVCC: update Alice2 repeatedly so the slave builds a real multi-
          ;; version prev-pointer chain (the 2nd+ update's old-node carries a
          ;; non-zero prev-pointer -- a MASTER heap address that must NOT be
          ;; copied into the slave's chain; the slave re-derives it locally).
          (dotimes (i 3)
            (with-transaction ()
              (let ((v (copy (by-name "Alice2"))))
                (setf (slot-value v 'age) (+ 31 i))   ; -> 31, 32, 33
                (save v))))
          (format t "~&MASTER: TX2b committed (Alice2 age -> 33 over 3 updates)~%")
          (finish-output)
          (mark-deleted (by-name "Bob"))    ; mark-deleted self-wraps a txn
          (format t "~&MASTER: TX3 committed (deleted Bob)~%") (finish-output))
        (write-flag "phase2done"))
      ;; stay alive until the slave is done (or 60s), then close cleanly
      (wait-flag "slave-done")
      (close-graph g :snapshot-p nil)
      (format t "~&MASTER: closed~%") (finish-output)
      (mexit 0))
  (error (c)
    (format t "~&MASTER ERROR: ~A~%" c) (finish-output)
    (mexit 1)))
