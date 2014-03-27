(in-package :graph-db)

(defvar *stop-repl-listener* nil)
(defvar *slave-session* nil)

;;; Catching EPIPE: ugh. But I don't think there's a better way on
;;; SBCL
(defun broken-pipe-error-p (condition)
  (equalp (last (simple-condition-format-arguments condition))
          '("Broken pipe")))

(deftype broken-pipe-error ()
  '(satisfies broken-pipe-error-p))

(defstruct (replication-session
             (:conc-name repl-)
             (:print-function
              (lambda (session s d)
                (declare (ignore d))
                (let ((*print-pretty* nil))
                  (format s "#<REPLICATION-SESSION ~S>" (repl-host session))))))
  host thread txn-id queue stream socket)

(defmethod start-replication ((graph graph) &key package)
  (declare (ignore package))
  ;;noop
  )

(defmethod stop-replication ((graph graph))
  ;;noop
  )

(defmethod remove-slave ((graph master-graph) thread)
  (with-recursive-lock-held ((slaves-lock graph))
    (setf (slaves graph) (remove thread (slaves graph)))))

(defmethod add-slave ((graph master-graph) thread)
  (with-recursive-lock-held ((slaves-lock graph))
    (pushnew thread (slaves graph))))

(defmethod end-slave-session ((graph master-graph) &key abort)
  (when (replication-session-p *slave-session*)
    (when (and (streamp (repl-stream *slave-session*))
               (open-stream-p (repl-stream *slave-session*))
               (not abort))
      (handler-case
          (force-output (repl-stream *slave-session*))
        (broken-pipe-error () nil))
      (close (repl-stream *slave-session*) :abort abort)))
  (remove-slave graph (current-thread)))

(defmethod auth-slave ((graph master-graph))
  (format (repl-stream *slave-session*) "~S~%" (graph-name graph))
  (force-output (repl-stream *slave-session*))
  (let ((auth-key (read (repl-stream *slave-session*))))
    (log:debug "GOT AUTH-KEY ~S" auth-key)
    (if (equal auth-key (replication-key graph))
        (progn
          (format (repl-stream *slave-session*) ":OK~%")
          (force-output (repl-stream *slave-session*)))
        (progn
          (format (repl-stream *slave-session*) ":INCORRECT-AUTH-KEY~%")
          (end-slave-session graph)
          (error 'slave-auth-error
                 :reason (format nil "Bad key ~A" auth-key)
                 :host (when (repl-socket *slave-session*)
                         (usocket:get-peer-name (repl-socket *slave-session*))))))))

(defmethod stream-txns-to-slave ((graph master-graph) secs msecs id)
  (setf (repl-txn-id *slave-session*) (list secs msecs id))
  (handler-case
      (let ((files (find-txn-logs (directory-namestring (txn-log graph))
                                  (1- secs)))
            (*print-pretty* nil))
        (dolist (file files)
          (log:debug "Reading txn file ~A for slave ~A" file (repl-host *slave-session*))
          (with-open-file (txn-in file :direction :input)
            (log:debug "OPENED ~A AS ~A" file txn-in)
            (do ((txn (read txn-in nil :eof) (read txn-in nil :eof)))
                ((eql txn :eof))
              (log:debug "READ ~S" txn)
              (when (or (> (first txn) secs)
                        (and (>= (first txn) secs)
                             (> (second txn) msecs))
                        (and (>= (first txn) secs)
                             (>= (second txn) msecs)
                             (> (third txn) id)))
                (log:debug "SENDING ~S TO SLAVE" txn)
                (format (repl-stream *slave-session*) "~S~%" txn)
                (force-output (repl-stream *slave-session*))
                (let ((response (read (repl-stream *slave-session*))))
                  (unless (eql response :ack)
                    (error "Bad response from slave ~A: ~A"
                           *slave-session* response)))
                (setf (repl-txn-id *slave-session*)
                      (subseq txn 0 3)))))))
    (broken-pipe-error ()
      (log:error "broken pipe on slave ~A" *slave-session*)
      (end-slave-session graph :abort t))
    (error (c)
      (log:error "cannot stream txns to slave: ~A" c)
      (end-slave-session graph :abort t)
      (error "streaming error: ~A" c)))
  (log:debug "DONE STREAMING TO SLAVE ~A" *slave-session*))

(defmethod repl-slave-loop ((graph master-graph) package)
  (declare (ignore package))
  (let ((txn-id (read (repl-stream *slave-session*)))
        (*print-pretty* nil))
    (log:debug "READ TXN-ID: '~S'" txn-id)
    (unless (and (consp txn-id) (= (length txn-id) 3) (every 'integerp txn-id))
      (log:error "READ BAD TXN-ID: ~A" txn-id)
      (format (repl-stream *slave-session*) ":BAD-TXN-ID~%")
      (end-slave-session graph)
      (return-from repl-slave-loop :bad-txn-id))
    (handler-case
        (let ((*readtable* (copy-readtable)))
          (local-time:enable-read-macros)
          (stream-txns-to-slave graph
                                (first txn-id) (second txn-id) (third txn-id))
          (loop until (stop-replication-p graph)
             do
             (let ((current-txn-file (txn-file graph)) (stop-p nil))
               (with-open-file (txn-in current-txn-file)
                 (do ((txn (read txn-in nil :eof) (read txn-in nil :eof)))
                     ((or stop-p
                          (stop-replication-p graph)
                          (not (open-stream-p (repl-stream *slave-session*)))))
                   (cond ((and (eql txn :eof)
                               (equal current-txn-file (txn-file graph)))
                          (let* ((sock (repl-socket *slave-session*))
                                 (stream (repl-stream *slave-session*))
                                 (ready (usocket:wait-for-input sock
                                                                :timeout 0.1
                                                                :ready-only t)))
                            (when ready
                              (let ((response (read stream)))
                                (case response
                                  (:quit
                                   (log:debug "GOT :QUIT FROM SLAVE")
                                   (end-slave-session graph)
                                   (return-from repl-slave-loop :slave-quit))
                                  (t
                                   (error "Bad response from slave ~A: ~A"
                                          *slave-session* response)))))))
                         ((eql txn :eof)
                          (sb-ext:wait-for (and (txn-file graph)
                                                (probe-file (txn-file graph))))
                          (setq stop-p t))
                         (t
                          (when
                              (or
                               (> (first txn)
                                  (first (repl-txn-id *slave-session*)))
                               (and (>= (first txn)
                                        (first (repl-txn-id *slave-session*)))
                                    (> (second txn)
                                       (second (repl-txn-id *slave-session*))))
                               (and (>= (first txn)
                                        (first (repl-txn-id *slave-session*)))
                                    (>= (second txn)
                                        (second (repl-txn-id *slave-session*)))
                                    (> (third txn)
                                       (third (repl-txn-id *slave-session*)))))
                            (log:debug "SENDING ~S TO SLAVE" txn)
                            (format (repl-stream *slave-session*) "~S~%" txn)
                            (force-output (repl-stream *slave-session*))
                            (let ((response (read (repl-stream *slave-session*))))
                              (case response
                                (:ack
                                 (setf (repl-txn-id *slave-session*)
                                       (subseq txn 0 3)))
                                (:quit
                                 (log:debug "GOT :QUIT FROM SLAVE")
                                 (end-slave-session graph)
                                 (return-from repl-slave-loop :slave-quit))
                                (t
                                 (error "Bad response from slave ~A: ~A"
                                        *slave-session* response))))))))))))
      (sb-int:closed-stream-error (c)
        (log:error "~A got ~A. Quitting" *slave-session* c)
        (format (repl-stream *slave-session*) ":STREAM-ERROR~%")
        (end-slave-session graph)
        (return-from repl-slave-loop :closed-stream-error))
      (error (c)
        (log:error "~A got ~A. Quitting" *slave-session* c)
        (format (repl-stream *slave-session*) ":INTERNAL-ERROR~%")
        (end-slave-session graph)
        (return-from repl-slave-loop :error)))))

(defmethod repl-accept-handler ((graph master-graph) socket)
  (make-thread
   (lambda ()
     (let ((*package* (find-package :graph-db)))
       (let (*slave-session* (*print-pretty* nil))
         (log:debug "IN REPL-ACCEPT-HANDLER FOR ~A" socket)
         (handler-case
             (let ((stream (usocket:socket-stream socket)))
               (setq *slave-session* (make-replication-session
                                      :host (usocket:get-peer-name socket)
                                      :thread (current-thread)
                                      :stream stream
                                      :socket socket
                                      :queue (sb-concurrency:make-queue)))
               (force-output (repl-stream *slave-session*))
               (auth-slave graph)
               (add-slave graph (current-thread))
               (repl-slave-loop graph :graph-db))
           (broken-pipe-error ()
             (log:error "Broken pipe on slave ~A. Killing session."
                        *slave-session*)
             (end-slave-session graph :abort t)
             (setq *slave-session* nil))
           (error (c)
             (log:error "REPL-ACCEPT-HANDLER for ~A got error: ~A. Killing session." *slave-session* c)
             (end-slave-session graph)
             (setq *slave-session* nil))
           (:no-error (r)
             (log:info "Slave session ~A ending: ~A" *slave-session* r)
             (end-slave-session graph))))))
   :name (format nil "repl-slave ~A handler" socket)))

(defmethod start-repl-listener (graph package &optional (address usocket:*wildcard-host*))
  (let ((*package* (find-package (or package :graph-db))))
    (let ((port (replication-port graph)))
      (log:info "Starting replication tcp listener on port ~A" port)
      (setf (stop-replication-p graph) nil)
      (usocket:with-server-socket (listener (usocket:socket-listen address port :reuse-address t))
        (loop until (stop-replication-p graph)
           do
           (handler-case
               (when (usocket:wait-for-input listener :ready-only t :timeout 1)
                 (let ((client-connection (usocket:socket-accept listener)))
                   (handler-case
                       (repl-accept-handler graph client-connection)
                     (usocket:connection-aborted-error ())
                     (usocket:socket-error (c)
                       (log:error "Listener got error on ~A: ~A" listener c)))))
             (error (c)
               (log:error "UNHANDLED ERROR OF TYPE ~A IN REPL LISTENER: ~A" (type-of c) c)))))
      (log:info "Shutting down repl tcp listener on port ~A" port))))

(defmethod start-replication ((graph master-graph) &key (package :graph-db))
  (let ((listener (sb-thread:make-thread 'start-repl-listener
                                         :name (format nil "~A-replication-thread" (graph-name graph))
                                         :arguments (list graph package))))
    (setf (replication-listener graph) listener)))

(defmethod stop-replication ((graph master-graph))
  (setf (stop-replication-p graph) t)
  (when (and (threadp (replication-listener graph))
             (thread-alive-p (replication-listener graph)))
    (join-thread (replication-listener graph)))
  (dolist (thread (slaves graph))
    (when (and (threadp thread)
               (thread-alive-p thread))
      (join-thread thread))))

(defmethod read-last-txn-id ((graph slave-graph))
  (let ((file (format nil "~A/slave-pos.sexp" (location graph))))
    (if (probe-file file)
        (with-open-file (in file)
          (read in nil '(0 0 0)))
        '(0 0 0))))

(defmethod write-last-txn-id ((graph slave-graph))
  (let ((file (format nil "~A/slave-pos.sexp" (location graph))))
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (if (master-txn-id graph)
          (write (master-txn-id graph) :stream out)
          (write '(0 0 0) :stream out)))))

(defmethod repl-slave-loop ((graph slave-graph) package)
  (let ((*package* (find-package (or package :graph-db))))
    (unwind-protect
         (progn
           ;; connect to server
           (setf (slave-socket graph)
                 (usocket:socket-connect (master-host graph) (replication-port graph)))
           (unless (open-stream-p (usocket:socket-stream (slave-socket graph)))
             (error "Unable to connect to master ~A" (master-host graph)))
           (let ((graph-name (read (usocket:socket-stream (slave-socket graph)))))
             (unless (eql graph-name (graph-name graph))
               (error "Connected to master with different graph name: ~A" graph-name))
             ;; send auth key
             (format (usocket:socket-stream (slave-socket graph)) "~S~%" (replication-key graph))
             (force-output (usocket:socket-stream (slave-socket graph)))
             (let ((response (read (usocket:socket-stream (slave-socket graph)))))
               (unless (eql response :ok)
                 (log:error "Slave auth error: ~A" response)
                 (error "Slave auth error: ~A" response)))
             ;; send txn-id
             (format (usocket:socket-stream (slave-socket graph)) "~S~%" (master-txn-id graph))
             (force-output (usocket:socket-stream (slave-socket graph)))
             ;; start receiving updates
             (let ((*readtable* (copy-readtable))
                   (*graph* graph))
               (local-time:enable-read-macros)
               (loop until (stop-replication-p graph) do
                    (handler-case
                        (when (usocket:wait-for-input (slave-socket graph) :timeout 1 :ready-only t)
                          (let ((txn (read (usocket:socket-stream (slave-socket graph)))))
                            ;; FIXME: verify txn structure
                            (let ((action (cadddr txn)) (plist (cddddr txn)))
                              (let ((*print-pretty* nil))
                                (log:debug "SLAVE GOT ~S" plist))
                              (execute-tx-action action plist)
                              (setf (master-txn-id graph) (subseq txn 0 3))
                              (write-last-txn-id graph)
                              (format (usocket:socket-stream (slave-socket graph)) ":ACK~%")
                              (force-output (usocket:socket-stream (slave-socket graph))))))
                      (error (c)
                        (log:error "Replication error for ~A/~A: ~A"
                                   (master-host graph) (graph-name graph) c)
                        ;; FIXME: send admin a message
                        (return))))
               (log:info "Stopping replication from ~A for graph ~A"
                         (master-host graph) (graph-name graph)))))
      (ignore-errors
        (when (and (slave-socket graph)
                   (streamp (usocket:socket-stream (slave-socket graph)))
                   (open-stream-p (usocket:socket-stream (slave-socket graph))))
          (format (usocket:socket-stream (slave-socket graph)) ":QUIT~%")
          (force-output (usocket:socket-stream (slave-socket graph)))
          (usocket:socket-close (slave-socket graph)))))))

(defmethod start-replication ((graph slave-graph) &key package)
  ;; Read last recorded txn-id
  (setf (master-txn-id graph) (read-last-txn-id graph)
        (stop-replication-p graph) nil)
  (setf (slave-thread graph)
        (sb-thread:make-thread 'repl-slave-loop
                               :arguments (list graph package)
                               :name (format nil "~A-repl-slave-thread" (graph-name graph)))))

(defmethod stop-replication ((graph slave-graph))
  (setf (stop-replication-p graph) t)
  (when (and (threadp (slave-thread graph))
             (thread-alive-p (slave-thread graph)))
    (join-thread (slave-thread graph))))
