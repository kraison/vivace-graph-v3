(in-package :graph-db)

;;;
;;; Transaction replication logs exhaustively partition the set of all
;;; transactions applied to the graph. Therefore if transaction log L1
;;; starts with transaction id 10 and transaction log L2 starts with
;;; transaction id 20, log L1 has a maximum transaction id range of 10
;;; through 20.
;;;
;;; The goal of log streaming is to catch a slave up with all the
;;; transactions applied since some particular transaction id ID.
;;;
;;; Streaming logs to a slave, therefore, means locating the log with
;;; a range that overlaps the desired transaction and streaming it
;;; starting from that transaction, then streaming all subsequent logs
;;; in their entirety.
;;;

(defun all-replication-logs (graph)
  (let ((files
         (directory
          (merge-pathnames "replication-*.log"
                           (persistent-transaction-directory graph)))))
    (sort files #'string< :key 'pathname-name)))

(defun parse-replication-log-name (replication-log)
  ;; File format is "replication-<hex transaction id>.log"
  (let ((name (pathname-name replication-log)))
    (when (search "replication-" name)
      (let ((start (length "replication-")))
        (parse-integer name :start start :radix 16)))))


(defun minimum-transaction-id (replication-log)
  (or (parse-replication-log-name replication-log)
      ;; FIXME: Could slurp out the first tx-header packet from the
      ;; file
      (error "Cannot determine minimum transaction id of ~A"
             replication-log)))

(defun replication-log-ranges (replication-logs)
  (let* ((starts (mapcar 'minimum-transaction-id replication-logs))
         (ends (append (mapcar '1- (rest starts))
                       (list most-positive-fixnum))))
    (mapcar 'cons starts ends )))

(defun ranges-overlap-p (range1 range2)
  (destructuring-bind (start1 . end1)
      range1
    (destructuring-bind (start2 . end2)
        range2
      (and (<= start1 end2)
           (<= start2 end1)))))

(defun applicable-replication-logs (transaction-id graph)
  "Return a list of replication log pathnames that may have records at
  and past TRANSACTION-ID."
  (let* ((logs (all-replication-logs graph))
         (ranges (replication-log-ranges logs))
         (target (cons transaction-id most-positive-fixnum)))
    (loop for log in logs
         for range in ranges
         when (ranges-overlap-p range target)
         collect log)))

(defun read-stream-packet (stream)
  (read-uint64-sized-vector stream))

(defun stream-all-packets (stream socket)
  (loop
     (let ((packet (read-stream-packet stream)))
       (unless packet
         (return))
       (write-packet packet socket))))

(defun stream-replication-log (socket replication-log minimum-transaction-id)
  "Stream the transactions of REPLICATION-LOG having a transaction-id
  equal to or higher than MINIMUM-TRANSACTION-ID to SOCKET."
  ;; Transactions in a replication log are in monotonically increasing
  ;; transaction id order, so as soon as a valid transaction id is
  ;; seen, the remainder of the file can be streamed without
  ;; inspection.
  (with-open-file (stream replication-log :element-type '(unsigned-byte 8))
    (loop
       (let ((packet (read-stream-packet stream)))
         (unless packet
           (return))
         (let* ((tx-header (deserialize-tx-header-vector packet))
                (position (file-position stream)))
           (when (<= minimum-transaction-id (transaction-id tx-header))
             (write-packet packet socket)
             (return))
           (file-position stream (+ position (write-size tx-header))))))
    (stream-all-packets stream socket)))

(defun stream-entire-replication-log (socket replication-log)
  "Like STREAM-REPLICATION-LOG, but does not filter transaction id."
  (with-open-file (stream replication-log :element-type '(unsigned-byte 8))
    (stream-all-packets stream socket)))

(defgeneric stream-logged-transactions (graph session)
  (:method (graph session)
    (let* ((minimum-transaction-id (1+ (highest-transaction-id session)))
           (logs (applicable-replication-logs minimum-transaction-id
                                              graph))
           (socket (socket session)))
      (log:debug "Streaming ~A to ~A" logs session)
      (when logs
        (stream-replication-log socket (first logs) minimum-transaction-id))
      (map nil
           (lambda (log) (stream-entire-replication-log socket log))
           (rest logs)))))
