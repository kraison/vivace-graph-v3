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

(defmethod snapshot ((graph graph))
  (let ((count nil))
    (with-recursive-lock-held ((txn-lock graph))
      (let ((problems (check-data-integrity graph)))
        (if problems
            (return-from snapshot
              (values :data-integrity-issues
                      problems))
            (progn
              (close-txn-log graph)
              (multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
                (let ((snap-file (format nil "~A/txn-log/snap-~D.~6,'0D"
                                         (location graph) sec msec)))
                  (setq count (backup graph snap-file))))
              (init-txn-log graph)
              count))))))

(defun find-newest-snapshot (dir)
  (let ((file (first (sort
                      (remove-if-not (lambda (file)
                                       (cl-ppcre:scan "^snap-"
                                                      (file-namestring file)))
                                     (cl-fad:list-directory dir))
                      '> :key 'file-write-date))))
    (values file (file-write-date file))))

(defun find-txn-logs (dir date)
  (sort
   (remove-if (lambda (file)
                (or (cl-ppcre:scan "^snap-" (file-namestring file))
                    (< (file-write-date file) date)))
              (cl-fad:list-directory dir))
   '< :key 'file-write-date))

(defun execute-tx-action (action plist)
  (let* ((subtype (cond ((subtypep (car plist) 'vertex) :vertex)
                        ((subtypep (car plist) 'edge) :edge)
                        (t (error "Unknown graph type ~A" (car plist))))))
    (case action
      (:add
       (case subtype
         (:vertex
          (setf (nth 2 plist) (transform-to-byte-vector (nth 2 plist)))
          (make-vertex (nth 0 plist)
                       (nth 1 plist)
                       :id (nth 2 plist)
                       :revision (nth 3 plist)
                       :deleted-p (nth 4 plist)))
         (:edge
          (setf (nth 1 plist) (transform-to-byte-vector (nth 1 plist)))
          (setf (nth 2 plist) (transform-to-byte-vector (nth 2 plist)))
          (setf (nth 5 plist) (transform-to-byte-vector (nth 5 plist)))
          (make-edge (nth 0 plist)
                     (nth 1 plist)
                     (nth 2 plist)
                     (nth 3 plist)
                     (nth 4 plist)
                     :id (nth 5 plist)
                     :revision (nth 6 plist)
                     :deleted-p (nth 7 plist)))))
      (:delete
       (case subtype
         (:vertex
          (let ((vertex (lookup-vertex (transform-to-byte-vector
                                        (nth 2 plist)))))
            (if vertex
                (mark-deleted vertex)
                (log:error "DELETE on unknown vertex ~A" (nth 2 plist)))))
         (:edge
          (let ((edge (lookup-vertex (transform-to-byte-vector
                                      (nth 5 plist)))))
            (if edge
                (mark-deleted edge)
                (log:error "DELETE on unknown edge ~A" (nth 5 plist)))))))
      (:modify
       (case subtype
         (:vertex
          (let ((vertex (lookup-vertex (transform-to-byte-vector
                                        (nth 2 plist)))))
            (if vertex
                (let ((new-vertex (copy vertex)))
                  (setf (data new-vertex) (nth 1 plist))
                  (save new-vertex))
                (log:error "MODIFY on unknown vertex ~A" (nth 2 plist)))))
         (:edge
          (let ((edge (lookup-edge (transform-to-byte-vector
                                    (nth 5 plist)))))
            (if edge
                (let ((new-edge (copy edge)))
                  (setf (weight new-edge) (nth 3 plist))
                  (setf (data new-edge) (nth 4 plist))
                  (save new-edge))
                (log:error "MODIFY on unknown edge ~A" (nth 5 plist)))))))
      (otherwise
       (dbg "Unknown input: ~S" plist)
       (log:error "Unknown input: ~S" plist)))))

(defmethod replay-txn-file ((graph graph) file)
  (let ((*graph* graph))
    (let ((*readtable* (copy-readtable)))
      (local-time:enable-read-macros)
      (with-open-file (in file)
        (do ((plist (read in nil :eof) (read in nil :eof)))
            ((eq plist :eof))
          ;;(let ((action (caddr plist)) (plist (cdddr plist)))
          (let ((action (cadddr plist)) (plist (cddddr plist)))
            (%%unsafe-execute-tx-action action plist)))))))

(defmethod replay ((graph graph) txn-dir package-name)
  (multiple-value-bind (snapshot date) (find-newest-snapshot txn-dir)
    (restore graph snapshot :package-name package-name)
    (dolist (txn-log-file (find-txn-logs txn-dir date))
      (replay-txn-file graph txn-log-file))
    (dbg "Generating graph views.")
    (map nil
         (lambda (pair)
           (destructuring-bind (class-name . view-name) pair
             (regenerate-view graph class-name view-name)))
         (all-views graph))
    (dbg "Checking data integrity.")
    (or (check-data-integrity graph)
        graph)))

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

