;;;; Restoring a snapshot file as a set of recovery transactions

(in-package :graph-db)

(defvar *restore-objects-per-transaction* 10)

(defun restore-sharp-paren-reader (stream subcharacter arg)
  (declare (ignore subcharacter arg))
  (let ((contents (read-delimited-list #\) stream)))
    (coerce contents '(simple-array (unsigned-byte 8) (*)))))

(defparameter *restore-readtable*
  (let ((*readtable* (copy-readtable)))
    (local-time:enable-read-macros)
    (set-dispatch-macro-character #\# #\( 'restore-sharp-paren-reader)
    *readtable*))

(defun read-n-sexps (stream n)
  "Read N s-expressions from STREAM. Returns a list of s-expressions
as the primary value, and non-nil as the secondary value at EOF."
  (let ((sexps '()))
    (dotimes (i n (nreverse sexps))
      (let ((sexp (read stream nil stream)))
        (when (eq sexp stream)
          (return (values (nreverse sexps) :eof)))
        (push sexp sexps)))))

(defun call-for-snapshot-sexps (fun file sexp-count)
  "Call FUN with a list of SEXP-COUNT s-expressions from FILE, until
  EOF in the file. The final list may have fewer than SEXP-COUNT
  expressions."
  (with-open-file (stream file)
    (loop
       (multiple-value-bind (sexps eofp)
           (read-n-sexps stream sexp-count)
         (when sexps
           (funcall fun sexps))
         (when eofp
           (return))))))

(defmacro do-snapshot-sexps ((var file &optional (count 10)) &body body)
  `(call-for-snapshot-sexps (lambda (,var) ,@body)
                            ,file
                            ,count))

(defun recreate-graph (graph snapshot-file &key package-name)
  (let ((*package* (find-package package-name))
        (*readtable* *restore-readtable*)
        (*graph* graph)
        (count 0)
        (tx-id (load-highest-transaction-id graph))
        (start-time (get-universal-time)))
    (do-snapshot-sexps (plists snapshot-file *restore-objects-per-transaction*)
      (let ((*transaction* (make-instance 'restore-transaction
                                          :transaction-id (incf tx-id))))
        (dolist (plist plists)
          (when (zerop (mod (incf count) 100))
            (format t "~A RESTORED ~A NODES~%" (current-thread) count))
          (ecase (car plist)
            (:v
             (apply 'make-vertex (rest plist)))
            (:e
             (apply 'make-edge (rest plist)))
            (:last-txn-id)
            (otherwise
             (log:error "RESTORE: Unknown input: ~S" plist))))
        (apply-transaction *transaction* graph)))
    (persist-highest-transaction-id (incf tx-id) graph)
    (let ((elapsed-time (- (get-universal-time) start-time)))
      (dbg "RESTORE TOOK ~A SECONDS" elapsed-time)
      (values graph :count count :elapsed-time elapsed-time))))

