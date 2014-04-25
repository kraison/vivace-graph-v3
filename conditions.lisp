(in-package :graph-db)

(define-condition slave-auth-error (error)
  ((reason :initarg :reason)
   (host :initarg :host))
  (:report (lambda (error stream)
             (with-slots (reason host) error
               (format stream "Slave auth error ~A: ~A." host reason)))))

(define-condition transaction-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason) error
               (format stream "Transaction error: ~A." reason)))))

(define-condition serialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Serialization failed for ~a because of ~a."
                       instance reason)))))

(define-condition deserialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Deserialization failed for ~a because of ~a."
                       instance reason)))))

(define-condition stale-revision-error (error)
  ((instance :initarg :instance)
   (current-revision :initarg :current-revision))
  (:report (lambda (error stream)
             (with-slots (instance current-revision) error
               (format stream "Attempt to update stale revision ~S of ~S."
                       instance current-revision)))))

(define-condition duplicate-key-error (error)
  ((instance :initarg :instance)
   (key :initarg :key))
  (:report (lambda (error stream)
             (with-slots (instance key) error
               (format stream "Duplicate key ~S in ~S."
                       key instance)))))

(define-condition nonexistent-key-error (error)
  ((instance :initarg :instance)
   (key :initarg :key))
  (:report (lambda (error stream)
             (with-slots (instance key) error
               (format stream "Nonexistent key ~S in ~S."
                       key instance)))))

(define-condition node-already-deleted-error (error)
  ((node
    :initarg :node))
  (:report (lambda (error stream)
             (with-slots (node) error
               (format stream "Node ~A already deleted" node)))))

(define-condition vertex-already-deleted-error (node-already-deleted-error)
  ())

(define-condition edge-already-deleted-error (node-already-deleted-error)
  ())
