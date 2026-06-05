;; Layer 1: Custom Exception/Condition Types
;; File: src/conditions.lisp
;; Purpose: Define all custom condition types used throughout VivaceGraph for structured
;;          error handling. Each condition carries context about what failed, enabling
;;          user code to implement targeted recovery strategies.
;;
;; Design: Conditions use Lisp's standard define-condition macro. Each condition
;;         specifies slots (data fields) and a :report method (formatted message).
;;         Inheritance is used minimally (only node-already-deleted chain).
;;
;; Context: These 11 condition types are raised by Layers 2-7 when errors occur.
;;          User code catches them via handler-case, inspects slots for context,
;;          and implements recovery (retry, backoff, logging, etc.).
;;
;; Integration points:
;;   - Exported from package.lisp (check which ones!)
;;   - Raised by Layer 2-7 code (utilities, storage, transactions, views, etc.)
;;   - Caught by user code via handler-case
;;
;; Issues documented in this file:
;;   - "Slave" terminology is deprecated (Line 3)
;;   - Empty subclasses for vertex/edge (Lines 62-66) add no functionality
;;   - Flat hierarchy lacks intermediate error categories
;;   - transaction-error (Line 10) is too generic
;;   - Slot naming inconsistent across conditions
;;   - No docstrings on slots or report format
;;
;; Testing: No tests yet. Critical path: verify all conditions can be created,
;;          report correctly, and are caught by handler-case.
;;
;; Performance: Negligible. Condition creation/signaling is compile-time + exception flow.
;;              No runtime overhead in normal execution (only when errors occur).

(in-package :graph-db)

;; ==============================================================================
;; Section 1: Replication Errors
;; ==============================================================================

(define-condition slave-auth-error (error)
  ;; Condition type: slave-auth-error
  ;;
  ;; Purpose: Raised when replication authentication fails (slave/replica cannot
  ;;          authenticate to master/primary).
  ;;
  ;; ** ! **  NOMENCLATURE ISSUE: "Slave" terminology is deprecated in industry
  ;;     (replaced by "replica", "secondary", "follower", "standby")
  ;;     Code uses "slave" throughout; affects public API
  ;;     Risk: May be renamed in future, breaking user code that catches this
  ;;     Phase 3 (API stability) will lock this name in (unless renamed before)
  ;;
  ;; When raised: Layer 7 (replication) attempts to authenticate to remote replica.
  ;;              Connection established but credentials rejected.
  ;;              Network/protocol error also possible but different condition.
  ;;
  ;; Related conditions: None directly; part of replication error family (currently)
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (start-replication remote-host)
  ;;     (slave-auth-error (e)
  ;;       (format t "Cannot authenticate to ~A: ~A~%"
  ;;               (slave-auth-error-host e)
  ;;               (slave-auth-error-reason e))))
  ;;
  ((reason :initarg :reason)
   ;; Slot: reason
   ;; Type: String (expected)
   ;; Purpose: Human-readable explanation of why authentication failed
   ;; Examples: "invalid token", "credentials rejected", "timeout during handshake"
   ;; Set by: Layer 7 code when raising condition
   ;; Accessed by: User code in handler (via slave-auth-error-reason accessor)
   ;; Note: No type checking; could be any type if user raises manually
   
   (host :initarg :host))
   ;; Slot: host
   ;; Type: String (expected) — hostname or IP of replica
   ;; Purpose: Identifies which replica failed to authenticate
   ;; Examples: "db2.example.com", "192.168.1.5", "replica-node-3"
   ;; Set by: Layer 7 code when raising condition
   ;; Accessed by: User code in handler (via slave-auth-error-host accessor)
   ;; Note: Used for logging/alerting which replica is problematic
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for human-readable output
             ;; Called by: Lisp when printing exception (e.g., REPL, error message)
             ;; Purpose: Provides friendly error message
             ;;
             ;; Format string: "Slave auth error ~A: ~A."
             ;;   ~A = (slave-auth-error-host error)   [host]
             ;;   ~A = (slave-auth-error-reason error) [reason]
             ;;
             ;; Example output: "Slave auth error db2.example.com: invalid token."
             ;;
             ;; Note: "Slave" terminology appears in output message (hardcoded!)
             ;;       If terminology changes, must update this string
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only (no shared state).
             ;;
             (with-slots (reason host) error
               ;; with-slots: Binds reason and host variables from error object slots
               ;; Purpose: Cleaner syntax for accessing multiple slots
               ;; Alternative: (slot-value error 'reason), (slot-value error 'host)
               ;;
               (format stream "Slave auth error ~A: ~A." host reason)))))
               ;; format: Writes formatted string to stream
               ;; stream: Output destination (connected to error display mechanism)
               ;; Arguments:
               ;;   "Slave auth error ~A: ~A." = format string
               ;;   host                        = first ~A replacement
               ;;   reason                      = second ~A replacement
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================
;; Section 2: Transaction Errors
;; ==============================================================================

(define-condition transaction-error (error)
  ;; Condition type: transaction-error
  ;;
  ;; Purpose: Raised when transaction fails (generic, covers multiple scenarios).
  ;;
  ;; ** ! **  DESIGN ISSUE: Too generic. Covers:
  ;;     - Deadlock detected
  ;;     - Transaction timeout
  ;;     - Insufficient locks available
  ;;     - Constraint violation
  ;;     - Resource exhaustion
  ;;
  ;; Problem: User code cannot differentiate causes; must parse reason string
  ;; Better: Specialized subtypes (deadlock-error, timeout-error, resource-error, etc.)
  ;;
  ;; When raised: Layer 3 detects transaction constraint violation.
  ;;              Layer 3-4 detects deadlock, timeout, or lock exhaustion.
  ;;
  ;; Related conditions: None directly; could use parent category (would help users)
  ;;
  ;; Usage pattern (suboptimal):
  ;;   (handler-case
  ;;     (with-transaction ...)
  ;;     (transaction-error (e)
  ;;       ;; Must parse reason string to decide recovery!
  ;;       (if (string-match "deadlock" (transaction-error-reason e))
  ;;         (retry-immediately)
  ;;         (exponential-backoff))))
  ;;
  ((reason :initarg :reason))
  ;; Slot: reason
  ;; Type: String (expected)
  ;; Purpose: Describes what went wrong in transaction
  ;; Examples: "deadlock detected", "timeout after 30 seconds", "lock limit exceeded"
  ;; Set by: Layer 3 code when raising condition
  ;; Accessed by: User code in handler (via transaction-error-reason accessor)
  ;; Note: Only context provided; users rely on string parsing to differentiate errors
  ;;       This forces brittle code (string matching vs. type discrimination)
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Simple, user-friendly error message
             ;;
             ;; Format string: "Transaction error: ~A."
             ;;   ~A = (transaction-error-reason error)
             ;;
             ;; Example output: "Transaction error: deadlock detected."
             ;;
             ;; Note: Very generic; doesn't differentiate cause
             ;;       User must read reason field to understand what happened
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (reason) error
               ;; with-slots: Binds reason variable from error object slot
               ;;
               (format stream "Transaction error: ~A." reason)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================
;; Section 3: Storage Format Errors (Serialization/Deserialization)
;; ==============================================================================

(define-condition serialization-error (error)
  ;; Condition type: serialization-error
  ;;
  ;; Purpose: Raised when converting Lisp object to binary format fails.
  ;;
  ;; Symmetry: Paired with deserialization-error (opposite operation).
  ;;           Both have instance + reason slots (consistent structure).
  ;;
  ;; When raised: Layer 4 serializer encounters object it cannot encode.
  ;;              Type code not found for object type.
  ;;              Buffer space exhausted during serialization.
  ;;              Data size exceeds configured limits.
  ;;
  ;; Related conditions: deserialization-error (symmetric pair)
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (with-transaction
  ;;       (make-vertex graph type-name data))  ;; serializes vertex
  ;;     (serialization-error (e)
  ;;       (log-error "Cannot store ~A: ~A"
  ;;                  (serialization-error-instance e)
  ;;                  (serialization-error-reason e))))
  ;;
  ((instance :initarg :instance)
   ;; Slot: instance
   ;; Type: Any (Lisp object)
   ;; Purpose: Identifies what object failed to serialize
   ;; Examples: <VERTEX v-123>, <EDGE e-456>, custom object instance
   ;; Set by: Layer 4 code when raising condition (pass the object that failed)
   ;; Accessed by: User code in handler (via serialization-error-instance accessor)
   ;; Note: Object may be partially serialized or in inconsistent state
   ;;       User should NOT attempt to modify object in handler
   
   (reason :initarg :reason))
   ;; Slot: reason
   ;; Type: String (expected)
   ;; Purpose: Explains why serialization failed
   ;; Examples: "unsupported type", "buffer overflow", "size limit exceeded"
   ;; Set by: Layer 4 code when raising condition
   ;; Accessed by: User code in handler (via serialization-error-reason accessor)
   ;; Note: Same pattern as transaction-error (only slot for context)
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Provides debugging information
             ;;
             ;; Format string: "Serialization failed for ~a because of ~a."
             ;;   ~a = (serialization-error-instance error) [object that failed]
             ;;   ~a = (serialization-error-reason error)   [why it failed]
             ;;
             ;; Example output: "Serialization failed for <VERTEX v-123> because of unsupported type."
             ;;
             ;; Note: Uses ~a (atom) instead of ~A (pretty-print); may affect readability
             ;;       Depends on object's print representation
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (instance reason) error
               ;; with-slots: Binds instance and reason variables from error object slots
               ;;
               (format stream "Serialization failed for ~a because of ~a."
                       instance reason)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================

(define-condition deserialization-error (error)
  ;; Condition type: deserialization-error
  ;;
  ;; Purpose: Raised when converting binary data back to Lisp object fails.
  ;;
  ;; Symmetry: Paired with serialization-error (opposite operation).
  ;;           Both have instance + reason slots (consistent structure).
  ;;           Both report methods have same format.
  ;;
  ;; When raised: Layer 4 deserializer reads unknown type code.
  ;;              Checksums don't match (data corruption detected).
  ;;              Unexpected end-of-file during deserialization.
  ;;              Type mismatch (expected vertex, found edge).
  ;;
  ;; Related conditions: serialization-error (symmetric pair)
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (lookup-vertex graph vertex-id)  ;; deserializes from disk
  ;;     (deserialization-error (e)
  ;;       (log-error "Data corruption for ~A: ~A"
  ;;                  (deserialization-error-instance e)
  ;;                  (deserialization-error-reason e))
  ;;       (notify-admin "Corrupted data detected")))
  ;;
  ((instance :initarg :instance)
   ;; Slot: instance
   ;; Type: Partially-deserialized object or type identifier
   ;; Purpose: Identifies what was being deserialized when error occurred
   ;; Examples: <VERTEX v-123> (partial), "type-code-255" (unknown type)
   ;; Set by: Layer 4 code when raising condition
   ;; Accessed by: User code in handler (via deserialization-error-instance accessor)
   ;; Note: Object may be incomplete/corrupted; user should NOT rely on its fields
   ;;       Safe to log/display but not to use for further operations
   
   (reason :initarg :reason))
   ;; Slot: reason
   ;; Type: String (expected)
   ;; Purpose: Explains why deserialization failed
   ;; Examples: "corrupted data", "unknown type code", "unexpected end-of-file"
   ;; Set by: Layer 4 code when raising condition
   ;; Accessed by: User code in handler (via deserialization-error-reason accessor)
   ;; Note: Indicates data integrity problem; should trigger data recovery procedures
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Provides debugging information
             ;;
             ;; Format string: "Deserialization failed for ~a because of ~a."
             ;;   ~a = (deserialization-error-instance error) [partial data/type]
             ;;   ~a = (deserialization-error-reason error)   [why it failed]
             ;;
             ;; Example output: "Deserialization failed for type-code-255 because of unknown type code."
             ;;
             ;; Note: Mirrors serialization-error report format (symmetry)
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (instance reason) error
               ;; with-slots: Binds instance and reason variables from error object slots
               ;;
               (format stream "Deserialization failed for ~a because of ~a."
                       instance reason)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================
;; Section 4: Concurrency Control Errors
;; ==============================================================================

(define-condition stale-revision-error (error)
  ;; Condition type: stale-revision-error
  ;;
  ;; Purpose: Raised when optimistic locking conflict detected (revision mismatch).
  ;;
  ;; Context: VivaceGraph uses optimistic locking for concurrency:
  ;;   1. Read phase: Load object with revision number (e.g., revision=5)
  ;;   2. Work phase: Modify object locally
  ;;   3. Write phase: Check current revision in DB
  ;;   4. If current ≠ expected: another transaction updated object
  ;;   5. Signal stale-revision-error
  ;;   6. User retries transaction
  ;;
  ;; When raised: Layer 3 (transactions) detects revision conflict.
  ;;              update-node called with stale expected revision.
  ;;              Current revision in DB differs from provided revision.
  ;;
  ;; Related conditions: None directly; could use parent category (concurrency-error)
  ;;
  ;; Usage pattern (standard MVCC retry):
  ;;   (defun resilient-update (graph vertex-id new-data)
  ;;     (handler-case
  ;;       (with-transaction
  ;;         (let ((node (lookup-vertex graph vertex-id)))
  ;;           (update-node *transaction* node new-data)))
  ;;       (stale-revision-error (e)
  ;;         (format t "Conflict: ~A has revision ~A~%"
  ;;                 (stale-revision-error-instance e)
  ;;                 (stale-revision-error-current-revision e))
  ;;         ;; Retry: current transaction fails, must retry from top
  ;;         (resilient-update graph vertex-id new-data))))
  ;;
  ((instance :initarg :instance)
   ;; Slot: instance
   ;; Type: Vertex or Edge object
   ;; Purpose: Identifies which object has revision conflict
   ;; Examples: <VERTEX v-123>, <EDGE e-456>
   ;; Set by: Layer 3 code when raising condition (pass the node being updated)
   ;; Accessed by: User code in handler (via stale-revision-error-instance accessor)
   ;; Note: Object is at stale revision (not current); retry should re-read from DB
   
   (current-revision :initarg :current-revision))
   ;; Slot: current-revision
   ;; Type: Integer (expected) — actual revision number in DB
   ;; Purpose: Shows what the actual revision is (compared to expected)
   ;; Examples: 5, 42, 1000
   ;; Set by: Layer 3 code when raising condition (pass actual revision from DB)
   ;; Accessed by: User code in handler (via stale-revision-error-current-revision accessor)
   ;; Note: User's expected revision differs from this value
   ;;       Comparison: user had X, DB has current-revision
   ;;       Indicates concurrent update happened between user's read and write
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Shows conflict details
             ;;
             ;; Format string: "Attempt to update stale revision ~S of ~S."
             ;;   ~S = (stale-revision-error-current-revision error) [actual revision]
             ;;   ~S = (stale-revision-error-instance error)         [object]
             ;;
             ;; Example output: "Attempt to update stale revision 5 of <VERTEX v-123>."
             ;;
             ;; Note: Uses ~S (S-expression, quoted) not ~A (atom)
             ;;       Shows revision as literal value, instance as readable representation
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (instance current-revision) error
               ;; with-slots: Binds instance and current-revision variables from error object slots
               ;;
               (format stream "Attempt to update stale revision ~S of ~S."
                       instance current-revision)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================
;; Section 5: Index/Hash Table Constraint Errors
;; ==============================================================================

(define-condition duplicate-key-error (error)
  ;; Condition type: duplicate-key-error
  ;;
  ;; Purpose: Raised when inserting key that already exists in index/hash-table.
  ;;
  ;; Symmetry: Paired with nonexistent-key-error (opposite scenario).
  ;;           Both have instance + key slots (consistent structure).
  ;;
  ;; When raised: Layer 2 tries to insert vertex with duplicate UUID.
  ;;              Layer 2 tries to register type name that already exists.
  ;;              Hash-table or skip-list detects key collision.
  ;;
  ;; Related conditions: nonexistent-key-error (symmetric pair)
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (def-vertex person ((name :type string)))  ;; registers type
  ;;     (duplicate-key-error (e)
  ;;       (format t "Type already exists: ~A~%"
  ;;               (duplicate-key-error-key e))))
  ;;
  ((instance :initarg :instance)
   ;; Slot: instance
   ;; Type: String (expected) — hash-table name, index name, or schema name
   ;; Purpose: Identifies which collection has the duplicate key
   ;; Examples: "vertex-index", "edge-index", "type-registry"
   ;; Set by: Layer 2 code when raising condition
   ;; Accessed by: User code in handler (via duplicate-key-error-instance accessor)
   ;; Note: Could be any type if user raises manually; no type checking
   
   (key :initarg :key))
   ;; Slot: key
   ;; Type: Any — the key that's already present
   ;; Purpose: Identifies the duplicate key value
   ;; Examples: "v-123", "person", "alice-bob-knows"
   ;; Set by: Layer 2 code when raising condition (pass the duplicate key)
   ;; Accessed by: User code in handler (via duplicate-key-error-key accessor)
   ;; Note: Key format depends on context (UUID, type name, string, etc.)
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Shows which key is duplicate
             ;;
             ;; Format string: "Duplicate key ~S in ~S."
             ;;   ~S = (duplicate-key-error-key error)      [duplicate key]
             ;;   ~S = (duplicate-key-error-instance error) [collection]
             ;;
             ;; Example output: "Duplicate key person in type-registry."
             ;;
             ;; Note: Uses ~S (S-expression); shows key as literal value
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (instance key) error
               ;; with-slots: Binds instance and key variables from error object slots
               ;;
               (format stream "Duplicate key ~S in ~S."
                       key instance)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================

(define-condition nonexistent-key-error (error)
  ;; Condition type: nonexistent-key-error
  ;;
  ;; Purpose: Raised when key not found in index/hash-table (lookup failure).
  ;;
  ;; Symmetry: Paired with duplicate-key-error (opposite scenario).
  ;;           Both have instance + key slots (consistent structure).
  ;;           Report format mirrors duplicate-key-error.
  ;;
  ;; When raised: Layer 2 tries to look up vertex that doesn't exist.
  ;;              Layer 2 tries to find type by name that's not registered.
  ;;              Hash-table or skip-list lookup returns nil.
  ;;
  ;; Related conditions: duplicate-key-error (symmetric pair)
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (lookup-vertex graph "v-nonexistent")
  ;;     (nonexistent-key-error (e)
  ;;       (format t "Vertex not found: ~A~%"
  ;;               (nonexistent-key-error-key e))))
  ;;
  ((instance :initarg :instance)
   ;; Slot: instance
   ;; Type: String (expected) — hash-table name, index name, or schema name
   ;; Purpose: Identifies which collection doesn't have the key
   ;; Examples: "vertex-index", "edge-index", "type-registry"
   ;; Set by: Layer 2 code when raising condition
   ;; Accessed by: User code in handler (via nonexistent-key-error-instance accessor)
   ;; Note: Could be any type if user raises manually; no type checking
   
   (key :initarg :key))
   ;; Slot: key
   ;; Type: Any — the key that's missing
   ;; Purpose: Identifies what was being looked up
   ;; Examples: "v-123", "person", "alice-bob-knows"
   ;; Set by: Layer 2 code when raising condition (pass the missing key)
   ;; Accessed by: User code in handler (via nonexistent-key-error-key accessor)
   ;; Note: Key format depends on context (UUID, type name, string, etc.)
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Shows which key is missing
             ;;
             ;; Format string: "Nonexistent key ~S in ~S."
             ;;   ~S = (nonexistent-key-error-key error)      [missing key]
             ;;   ~S = (nonexistent-key-error-instance error) [collection]
             ;;
             ;; Example output: "Nonexistent key v-123 in vertex-index."
             ;;
             ;; Note: Uses ~S (S-expression); shows key as literal value
             ;;       Mirrors duplicate-key-error report format (symmetry)
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (instance key) error
               ;; with-slots: Binds instance and key variables from error object slots
               ;;
               (format stream "Nonexistent key ~S in ~S."
                       key instance)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================
;; Section 6: Soft-Delete Errors (Node Access After Deletion)
;; ==============================================================================

(define-condition node-already-deleted-error (error)
  ;; Condition type: node-already-deleted-error
  ;;
  ;; Purpose: Raised when attempting to access soft-deleted node (vertex or edge).
  ;;
  ;; Context: VivaceGraph uses soft-delete (mark deleted, don't remove immediately):
  ;;   1. User calls delete-vertex or delete-edge
  ;;   2. Node marked with deleted-p = true (not physically removed)
  ;;   3. Later lookup of deleted node checks deleted-p flag
  ;;   4. If true: signal node-already-deleted-error (or specialized subtype)
  ;;   5. Allows recovery, audit trails, undo operations
  ;;
  ;; When raised: Layer 2 tries to read deleted vertex: lookup-vertex returns deleted node.
  ;;              Layer 2 tries to read deleted edge: lookup-edge returns deleted node.
  ;;              Any operation accesses deleted-p = true.
  ;;
  ;; Inheritance: Parent of vertex-already-deleted-error and edge-already-deleted-error.
  ;;              Allows catching "any deleted node" or "specific deleted type".
  ;;
  ;; Related conditions: vertex-already-deleted-error (child)
  ;;                     edge-already-deleted-error (child)
  ;;
  ;; Usage pattern (generic):
  ;;   (handler-case
  ;;     (lookup-vertex graph "v-123")  ;; deleted
  ;;     (node-already-deleted-error (e)
  ;;       (format t "Deleted: ~A~%" (node e))))
  ;;
  ;; Usage pattern (specific):
  ;;   (handler-case
  ;;     (traverse graph start-vertex)
  ;;     (vertex-already-deleted-error (e)
  ;;       (format t "Start vertex deleted"))
  ;;     (edge-already-deleted-error (e)
  ;;       (format t "Edge deleted")))
  ;;
  ((node :initarg :node))
  ;; Slot: node
  ;; Type: Vertex or Edge object (partially-loaded)
  ;; Purpose: Identifies which node was deleted
  ;; Examples: <VERTEX v-123>, <EDGE e-456>
  ;; Set by: Layer 2 code when raising condition (pass the deleted node)
  ;; Accessed by: User code in handler (via node-already-deleted-error-node accessor)
  ;;              Can also use child accessors: (vertex-already-deleted-error-node e)
  ;; Note: Node object is in deleted state (deleted-p = true)
  ;;       Safe to inspect but not to modify
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Shows which node is deleted
             ;;
             ;; Format string: "Node ~A already deleted"
             ;;   ~A = (node-already-deleted-error-node error)
             ;;
             ;; Example output: "Node <VERTEX v-123> already deleted"
             ;;
             ;; Note: No period at end (different from other report methods)
             ;;       Style inconsistency (minor)
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (node) error
               ;; with-slots: Binds node variable from error object slot
               ;;
               (format stream "Node ~A already deleted" node)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================

(define-condition vertex-already-deleted-error (node-already-deleted-error)
  ;; Condition type: vertex-already-deleted-error
  ;;
  ;; Purpose: Specialization of node-already-deleted-error for deleted vertices.
  ;;
  ;; Inheritance: Parent is node-already-deleted-error.
  ;;              Inherits node slot from parent.
  ;;              Inherits :report method from parent.
  ;;              Adds NO new slots or methods.
  ;;
  ;; ** ! **  DESIGN ISSUE: Empty specialization. Adds nothing new.
  ;;     Could be replaced with:
  ;;       (typep error 'node-already-deleted-error)
  ;;       (typep (node error) 'vertex)
  ;;     Question: Why separate subclass if no new behavior?
  ;;     Answer: Type discrimination (catch specific type vs. parent type).
  ;;     Issue: Maintenance burden; if parent changes, child must too.
  ;;     Pattern: Marker subclass (used for type-based dispatch only).
  ;;
  ;; When raised: Layer 2 tries to read deleted vertex: lookup-vertex returns deleted vertex.
  ;;              Any operation accesses vertex with deleted-p = true.
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (traverse graph start-vertex)
  ;;     (vertex-already-deleted-error (e)
  ;;       (format t "Start vertex was deleted: ~A~%"
  ;;               (vertex-already-deleted-error-node e))))
  ;;
  ())  ;; Empty body — NO additional slots or methods
       ;; Inherits everything from node-already-deleted-error:
       ;;   - node slot
       ;;   - :report method (formats "Node ... already deleted")
       ;;   - Automatically generated accessor: (vertex-already-deleted-error-node e)
  ;;
  ;; Thread-safety: Safe (inherits parent's safety properties).

;; ==============================================================================

(define-condition edge-already-deleted-error (node-already-deleted-error)
  ;; Condition type: edge-already-deleted-error
  ;;
  ;; Purpose: Specialization of node-already-deleted-error for deleted edges.
  ;;
  ;; Inheritance: Parent is node-already-deleted-error.
  ;;              Inherits node slot from parent.
  ;;              Inherits :report method from parent.
  ;;              Adds NO new slots or methods.
  ;;
  ;; ** ! **  DESIGN ISSUE: Same as vertex-already-deleted-error.
  ;;     Empty specialization with no added behavior.
  ;;     Serves only for type discrimination.
  ;;     See vertex-already-deleted-error analysis above.
  ;;
  ;; When raised: Layer 2 tries to read deleted edge: lookup-edge returns deleted edge.
  ;;              Any operation accesses edge with deleted-p = true.
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (traverse graph start-vertex)
  ;;     (edge-already-deleted-error (e)
  ;;       (format t "Edge was deleted: ~A~%"
  ;;               (edge-already-deleted-error-node e))))
  ;;
  ())  ;; Empty body — NO additional slots or methods
       ;; Inherits everything from node-already-deleted-error:
       ;;   - node slot
       ;;   - :report method (formats "Node ... already deleted")
       ;;   - Automatically generated accessor: (edge-already-deleted-error-node e)
  ;;
  ;; Thread-safety: Safe (inherits parent's safety properties).

;; ==============================================================================
;; Section 7: View Errors
;; ==============================================================================

(define-condition invalid-view-error (error)
  ;; Condition type: invalid-view-error
  ;;
  ;; Purpose: Raised when view lookup fails (view doesn't exist for type).
  ;;
  ;; Context: Views are materialized query results per vertex/edge type (Layer 6).
  ;;          Users can register named views for types:
  ;;            (register-view graph "person" "all-people" query-fn)
  ;;          Later operations query these views:
  ;;            (map-view graph "person" "all-people" fn)
  ;;          If view not registered: signal invalid-view-error.
  ;;
  ;; When raised: Layer 6 tries to access view that doesn't exist.
  ;;              User calls map-view with unknown view name.
  ;;              Unregistered view accessed.
  ;;
  ;; Related conditions: view-lock-error (different failure mode)
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (map-view graph "person" "all-people" fn)
  ;;     (invalid-view-error (e)
  ;;       (format t "View ~A/~A not found~%"
  ;;               (invalid-view-error-class-name e)
  ;;               (invalid-view-error-view-name e))))
  ;;
  ((class-name :initarg :class-name)
   ;; Slot: class-name
   ;; Type: String (expected) — vertex or edge type name
   ;; Purpose: Identifies which type the view was for
   ;; Examples: "person", "address", "knows"
   ;; Set by: Layer 6 code when raising condition
   ;; Accessed by: User code in handler (via invalid-view-error-class-name accessor)
   ;; Note: Could be any type if user raises manually; no type checking
   
   (view-name :initarg :view-name))
   ;; Slot: view-name
   ;; Type: String (expected) — view identifier
   ;; Purpose: Identifies which view doesn't exist
   ;; Examples: "all-people", "young-adults", "by-age"
   ;; Set by: Layer 6 code when raising condition
   ;; Accessed by: User code in handler (via invalid-view-error-view-name accessor)
   ;; Note: Could be any type if user raises manually; no type checking
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Shows missing view identity
             ;;
             ;; Format string: "No such graph view: ~A/~A"
             ;;   ~A = (invalid-view-error-class-name error) [type]
             ;;   ~A = (invalid-view-error-view-name error)   [view]
             ;;
             ;; Example output: "No such graph view: person/all-people"
             ;;
             ;; Note: Uses slash separator (class-name/view-name) for readability
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (class-name view-name) error
               ;; with-slots: Binds class-name and view-name variables from error object slots
               ;;
               (format stream
                       "No such graph view: ~A/~A"
                       class-name view-name)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================

(define-condition view-lock-error (error)
  ;; Condition type: view-lock-error
  ;;
  ;; Purpose: Raised when view locking operation fails (cannot acquire lock).
  ;;
  ;; Context: Views are cached query results that require locking for thread-safe access
  ;;          (Layer 6). Read/write locks protect view from concurrent modification.
  ;;          Lock acquisition can fail for multiple reasons.
  ;;
  ;; When raised: Layer 6 tries to acquire read lock on view, times out.
  ;;              Layer 6 tries to acquire write lock, deadlock detected.
  ;;              Lock already held by same thread (recursive lock issue).
  ;;              Lock pool exhausted (too many threads waiting).
  ;;
  ;; Related conditions: invalid-view-error (different failure mode)
  ;;
  ;; Usage pattern:
  ;;   (handler-case
  ;;     (with-read-locked-view-group graph class-name
  ;;       (map-view graph class-name view-name fn))
  ;;     (view-lock-error (e)
  ;;       (format t "Failed to lock view: ~A~%"
  ;;               (view-lock-error-message e))
  ;;       (retry-with-backoff)))
  ;;
  ((message :initarg :message))
  ;; Slot: message
  ;; Type: String (expected) — detailed error description
  ;; Purpose: Explains why lock acquisition failed
  ;; Examples: "timeout waiting for lock", "deadlock detected", "lock already held"
  ;; Set by: Layer 6 code when raising condition
  ;; Accessed by: User code in handler (via view-lock-error-message accessor)
  ;; Note: Only slot provided; only message carries context
  ;;       Less specific than other conditions (no object reference)
  
  (:report (lambda (error stream)
             ;; :report method: Formats condition for display
             ;; Called by: Lisp when printing exception
             ;; Purpose: Shows locking error details
             ;;
             ;; Format string: "View locking error: '~A'"
             ;;   ~A = (view-lock-error-message error)
             ;;
             ;; Example output: "View locking error: 'timeout waiting for lock'"
             ;;
             ;; Note: Wraps message in single quotes (unusual formatting)
             ;;       Different style from other report methods
             ;;
             ;; Thread-safety: Safe. Lambda captures error object only.
             ;;
             (with-slots (message) error
               ;; with-slots: Binds message variable from error object slot
               ;;
               (format stream
                       "View locking error: '~A'"
                       message)))))
               ;; format: Writes formatted string to stream
               ;; Side-effects: Writes to output stream (error message displayed)

;; ==============================================================================
;; End of conditions.lisp
;; ==============================================================================
;; Summary:
;;   - 11 condition types defined (all inherit from error)
;;   - 1 inheritance chain: node-already-deleted-error with 2 children
;;   - All have :report methods (user-friendly messages)
;;   - Most have 1-2 slots (instance, reason, key, node, message, etc.)
;;   - No docstrings on conditions themselves (defined above instead)
;;   - No tests yet; critical path is condition creation/catching
;;
;; Issues documented:
;;   - Line 3: "Slave" terminology deprecated (should be "replica")
;;   - Lines 62-66: Empty subclasses (marker pattern, but adds no functionality)
;;   - Lines 10-14: transaction-error too generic (covers too many cases)
;;   - Across file: Inconsistent slot naming (instance vs node vs message)
;;   - Across file: Flat hierarchy (no intermediate error categories)
;;   - No docstrings explaining when each condition is raised
;;
;; Design decisions:
;;   - Minimal inheritance: only node-deleted chain
;;   - No shared parent for related errors (storage, concurrency, etc.)
;;   - Slot naming chosen per context (no standardization)
;;   - :report methods use different formatting styles
;;
;; Phase 3 implications:
;;   - All 11 condition names will freeze
;;   - "Slave" terminology may need rename (breaking change)
;;   - Hierarchy cannot be changed after Phase 3 (backward compatibility)
;; ==============================================================================