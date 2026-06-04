;;;; Replication test -- SLAVE process.
;;;;
;;;; Driven by run-replication-test.sh.  Reads REPL_SLAVE_DIR, REPL_PORT and
;;;; REPL_WORK from the environment, connects to the master, and verifies two
;;;; phases:
;;;;
;;;;   CATCH-UP: the pre-connect TX1 (2 vertices + 1 edge) replays in full,
;;;;             with EXACTLY 2 vertices and 1 edge (no double-apply).
;;;;   LIVE:     the post-connect update + delete stream through, leaving
;;;;             exactly 1 live vertex named "Alice2" and 0 edges (deleting
;;;;             Bob removes the incident edge).
;;;;
;;;; Exits 0 on success, 1 on any failed check or timeout.

(require :asdf)
;; Bootstrap quicklisp if the running Lisp didn't auto-load it (CCL/ECL on some
;; hosts don't); harmless when ql is already present (e.g. SBCL).
(unless (find-package :ql)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(ql:quickload :graph-db :silent t)
(in-package :graph-db)
(log:config :error)

(defun sflag (name) (format nil "~A/~A" (uiop:getenv "REPL_WORK") name))
(defun write-flag (name) (with-open-file (s (sflag name) :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                           (format s "~A" name)))
(defun wait-flag (name &optional (timeout 60))
  (dotimes (i (* timeout 10) nil)
    (when (probe-file (sflag name)) (return t))
    (sleep 0.1)))
(defun sexit (code) #+sbcl (sb-ext:exit :code code)
                    #+ccl (ccl:quit code)
                    #+ecl (ext:quit code)
                    #-(or sbcl ccl ecl) (uiop:quit code))

(defvar *fails* 0)
(defun check (ok fmt &rest args)
  (if ok
      (format t "~&  ok   ~?~%" fmt args)
      (progn (incf *fails*) (format t "~&  FAIL ~?~%" fmt args)))
  (finish-output))

(defun wait-until (predicate &optional (timeout 30))
  (dotimes (i (* timeout 10) (funcall predicate))
    (when (funcall predicate) (return t))
    (sleep 0.1)))

(load (merge-pathnames "schema.lisp" *load-pathname*))

(handler-case
    (progn
      (unless (wait-flag "ready")
        (format t "~&SLAVE: master never became ready~%") (sexit 1))
      (let* ((dir  (uiop:getenv "REPL_SLAVE_DIR"))
             (port (parse-integer (uiop:getenv "REPL_PORT")))
             (g (make-graph :repl-test-app dir :slave-p t :master-host "localhost"
                            :replication-port port :replication-key "test-secret"
                            :buffer-pool-size 1000)))
        (let ((*graph* g))
          (flet ((live ()  (map-vertices #'identity g :collect-p t :vertex-type 'r-person))
                 (edges () (map-edges #'identity g :collect-p t)))
            ;; --- Phase 1: catch up TX1 ---
            (wait-until (lambda () (and (= 2 (length (live))) (= 1 (length (edges))))))
            (check (= 2 (length (live))) "catch-up: 2 vertices (got ~D)" (length (live)))
            (check (= 1 (length (edges))) "catch-up: 1 edge (got ~D)" (length (edges)))
            (let ((alice (first (remove-if-not
                                 (lambda (v) (string= "Alice" (slot-value v 'name)))
                                 (live)))))
              (check alice "catch-up: vertex named Alice present")
              (when alice
                (check (= 30 (slot-value alice 'age)) "catch-up: Alice age 30")
                (check (= 1 (length (outgoing-edges alice)))
                       "catch-up: Alice has 1 outgoing edge (got ~D)"
                       (length (outgoing-edges alice)))))
            ;; signal master to send the live update + delete
            (write-flag "connected")
            (unless (wait-flag "phase2done")
              (check nil "master never finished phase 2"))
            ;; --- Phase 2: live update + delete ---
            (wait-until (lambda ()
                          (let ((lv (live)))
                            (and (= 1 (length lv))
                                 (string= "Alice2" (slot-value (first lv) 'name))))))
            (check (= 1 (length (live))) "live: exactly 1 live vertex (got ~D)" (length (live)))
            (let ((v (first (live))))
              (when v
                (check (string= "Alice2" (slot-value v 'name))
                       "live: update propagated (name=~S)" (slot-value v 'name))))
            (check (= 0 (length (edges)))
                   "live: edge gone after deleting Bob (got ~D)" (length (edges)))))
        (close-graph g :snapshot-p nil))
      (write-flag "slave-done")
      (if (zerop *fails*)
          (progn (format t "~&SLAVE: PASS~%") (finish-output) (sexit 0))
          (progn (format t "~&SLAVE: FAIL (~D failed checks)~%" *fails*)
                 (finish-output) (sexit 1))))
  (error (c)
    (format t "~&SLAVE ERROR: ~A~%" c) (finish-output)
    (ignore-errors (write-flag "slave-done"))
    (sexit 1)))
