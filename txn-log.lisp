(in-package :graph-db)

(defvar *current-transaction* nil)
(defvar *next-txn-id* (make-array 1 :element-type 'sb-ext:word :initial-element 0))

(defstruct (transaction
             (:print-function
              (lambda (tx stream depth)
                (declare (ignore depth))
                (format stream "#<TX ~S IN ~S>"
                        (tx-id tx) (tx-thread tx))))
             (:conc-name tx-))
  (id (gen-id))
  (queue nil)
  (rollback nil)
  (mailbox (sb-concurrency:make-mailbox))
  (thread (current-thread))
  (store nil)
  (locks nil))

(defun get-txn-id ()
  (multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
    (values sec msec (sb-ext:atomic-incf (aref *next-txn-id* 0)))))

(defun snapshot-file-txn-id (snapshot-file)
  "Return the highest txn id associated with SNAPSHOT-FILE."
  ;; If this is too coarse, the snapshot process could store a fine
  ;; txn-id as part of the snapshot data.
  (values (universal-to-unix-time (file-write-date snapshot-file)) 0 0))

(defmethod init-txn-log ((graph graph))
  (with-recursive-lock-held ((txn-lock graph))
    (let ((file (loop
                   for x from 0
                   for file = (format nil "~A/txn-log/~D.~D"
                                      (location graph)
                                      (timestamp-to-unix (now))
                                      x)
                   until (null (probe-file file))
                   finally (return file))))
      (ensure-directories-exist file)
      (setf (txn-file graph)
            file
            (txn-log graph)
            (open file :direction :output :if-does-not-exist :create)))))

(defmethod close-txn-log ((graph graph))
  (setf (txn-file graph) nil)
  (when (and (streamp (txn-log graph))
             (open-stream-p (txn-log graph)))
    (close (txn-log graph))))

(defmethod log-txn :around ((graph graph) action node)
  (with-recursive-lock-held ((txn-lock graph))
    (call-next-method)
    (force-output (txn-log graph))))

(defmethod log-txn ((graph graph) (action symbol) (vertex vertex))
  ;;(multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
  ;;"(~S ~S ~S ~S :V ~S ~S :ID ~S :REVISION ~S :DELETED-P ~S)~%"
  (multiple-value-bind (sec msec id) (get-txn-id)
    (let ((*print-pretty* nil))
      (let ((txn (list sec msec id action (type-of vertex) (data vertex) (id vertex)
                       (revision vertex) (deleted-p vertex))))
        (format (txn-log graph) "~S" txn)))))

(defmethod log-txn ((graph graph) (action symbol) (edge edge))
  ;;(multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
  ;;"(~S ~S ~S ~S :E ~S ~S ~S ~S ~S :ID ~S :REVISION ~S :DELETED-P ~S)~%"
  (multiple-value-bind (sec msec id) (get-txn-id)
    (let ((*print-pretty* nil))
      (let ((txn (list sec msec id action (type-of edge) (from edge) (to edge) (weight edge)
                       (data edge) (id edge) (revision edge) (deleted-p edge))))
        (format (txn-log graph) "~S" txn)))))

(defmethod snapshot ((graph graph) &key closing-graph-p include-deleted-p)
  (let ((count nil))
    (with-recursive-lock-held ((txn-lock graph))
      (let ((problems (check-data-integrity graph
                                            :include-deleted-p
                                            include-deleted-p)))
        (if problems
            (return-from snapshot
              (values :data-integrity-issues
                      problems))
            (progn
              (close-txn-log graph)
              (multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
                (let ((snap-file (format nil "~A/txn-log/snap-~D.~6,'0D"
                                         (location graph) sec msec)))
                  (setq count (backup graph
                                      snap-file
                                      :include-deleted-p include-deleted-p))))
              (unless closing-graph-p
                (init-txn-log graph))
              count))))))

(defun find-newest-snapshot (dir)
  (let ((file (first (sort
                      (remove-if-not (lambda (file)
                                       (cl-ppcre:scan "^snap-"
                                                      (file-namestring file)))
                                     (cl-fad:list-directory dir))
                      '> :key 'file-write-date))))
    (when file
      (values file (file-write-date file)))))

(defun find-txn-logs (dir date)
  (sort
   (remove-if (lambda (file)
                (or (cl-ppcre:scan "^snap-" (file-namestring file))
                    (< (file-write-date file) date)))
              (cl-fad:list-directory dir))
   '< :key 'file-write-date))

(defun execute-tx-action (action tx)
  (let* ((type (first tx))
         (subtype (cond ((subtypep type 'vertex) :vertex)
                        ((subtypep type 'edge) :edge)
                        (t (error "Unknown graph type ~A" type)))))
    (case subtype
      (:vertex
       (destructuring-bind (type data id revision deleted-p)
           tx
         (setf id (transform-to-byte-vector id))
         (case action
           (:add
            (make-vertex type data
                         :id id
                         :revision revision
                         :deleted-p deleted-p))
           (:delete
            (let ((vertex (lookup-vertex id)))
              (if vertex
                  (mark-deleted vertex)
                  (log:error "DELETE on unknown vertex ~A" id))))
           (:modify
            (let ((vertex (lookup-vertex id)))
              (if vertex
                  (let ((new-vertex (copy vertex)))
                    (setf (data new-vertex) data)
                    (save new-vertex))
                  (log:error "MODIFY on unknown vertex ~A" id)))))))
      (:edge
       (destructuring-bind (type from to weight data id revision deleted-p)
           tx
         (setf id (transform-to-byte-vector id))
         (case action
           (:add
            (setf from (transform-to-byte-vector from))
            (setf to (transform-to-byte-vector to))
            (make-edge type from to weight data
                       :id id
                       :revision revision
                       :deleted-p deleted-p))
           (:delete
            (let ((edge (lookup-edge id)))
              (if edge
                  (mark-deleted edge)
                  (log:error "DELETE on unknown edge ~A" id))))
           (:modify
            (let ((edge (lookup-edge id)))
            (if edge
                (let ((new-edge (copy edge)))
                  (setf (weight new-edge) weight)
                  (setf (data new-edge) data)
                  (save new-edge))
                (log:error "MODIFY on unknown edge ~A" id))))))))))

(defmethod replay-txn-file ((graph graph) file)
  (let ((*graph* graph)
        (last-txn-id nil))
    (let ((*readtable* (copy-readtable)))
      (local-time:enable-read-macros)
      (with-open-file (in file)
        (do ((plist (read in nil :eof) (read in nil :eof)))
            ((eq plist :eof) last-txn-id)
          ;;(let ((action (caddr plist)) (plist (cdddr plist)))
          (let ((action (cadddr plist)) (plist (cddddr plist)))
            (%%unsafe-execute-tx-action action plist))
          (setf last-txn-id (subseq plist 0 3)))))))

(defmethod replay ((graph graph) txn-dir package-name)
  (let ((last-txn-id nil))
    (multiple-value-bind (snapshot date) (find-newest-snapshot txn-dir)
      (if snapshot
          (progn
            (restore graph snapshot :package-name package-name)
            (setf last-txn-id (snapshot-file-txn-id snapshot)))
          (setq date 0))
      (dolist (txn-log-file (find-txn-logs txn-dir date))
        (dbg "Restoring txn-log file ~A" txn-log-file)
        (setf last-txn-id (replay-txn-file graph txn-log-file)))
      (dbg "Generating graph views.")
      (map nil
           (lambda (pair)
             (destructuring-bind (class-name . view-name) pair
               (regenerate-view graph class-name view-name)))
           (all-views graph))
      (dbg "Checking data integrity.")
      (values (or (check-data-integrity graph)
                  graph)
              last-txn-id))))

(defmethod execute-tx ((graph graph) fn timeout max-tries)
  ;; FIXME: implement
  )

(defmacro with-graph-transaction ((store &key timeout (max-tries 10))
                                  &body body)
  (with-gensyms (atomic-op)
    `(let ((,atomic-op (lambda () ,@body)))
       (cond ((and (transaction-p *current-transaction*)
                   (equal (graph-name (tx-store *current-transaction*))
                          (graph-name ,store)))
              (funcall ,atomic-op))
             ((transaction-p *current-transaction*)
              (error 'transaction-error
                     :reason
                     "Transactions cannot currently span multiple graphs."))
             (t
              (execute-tx ,store ,atomic-op ,timeout ,max-tries))))))

