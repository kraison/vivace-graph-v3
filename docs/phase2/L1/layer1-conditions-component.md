# Layer 1 Component Specification: conditions.lisp

**Component:** VivaceGraph Exception/Condition System  
**File:** src/conditions.lisp  
**Lines:** 84  
**Type:** Exception type definitions  
**Purpose:** Define 11 custom condition types for structured error handling across all layers  
**Scope:**
- ✓ Defines exception hierarchy (mostly flat, one specialization chain)
- ✓ Specifies slots and metadata for each condition type
- ✓ Provides error reporting (human-readable messages via :report methods)
- ✗ Does NOT implement error handling logic
- ✗ Does NOT determine when conditions are raised (Layer 2-7 responsibility)
- ✗ Does NOT define condition recovery mechanisms

## Conceptual Model

### Core Idea

**`conditions.lisp` is the error contract between VivaceGraph internals and external consumers.**

It serves two purposes:

1. **Error categorization:** Defines distinct exception types for different failure modes
2. **Error communication:** Specifies what information users receive when errors occur (via slots + report messages)

### Motivation

Without `conditions.lisp`:
- ❌ All errors would use generic `error` type (unhelpful)
- ❌ User code cannot distinguish "deadlock" from "timeout" from "resource exhausted"
- ❌ No structured access to error context (why it failed, what failed)
- ❌ Error messages would be unformatted strings

With `conditions.lisp`:
- ✅ Users catch specific condition types (e.g., `stale-revision-error`)
- ✅ Each condition carries context (instance, reason, revision number, etc.)
- ✅ Report methods provide user-friendly error messages
- ✅ Inheritance allows catching categories of errors

### Abstraction Boundary

**What it exposes:**
- 11 condition types (exception classes)
- Slots for each condition (instance, reason, key, node, etc.)
- Report methods (formatted error messages)
- Inheritance chain (node-already-deleted with 2 children)

**What it hides:**
- Error recovery mechanisms (user implements via handler-case)
- When conditions are raised (Layer 2-7 determines)
- How errors propagate (Lisp exception mechanism handles)

**What it should hide but currently doesn't:** ⚠️
- Overly broad conditions (transaction-error covers too many cases)
- Inconsistent slot naming (instance vs node vs message)

**What it should provide but doesn't:**
- Intermediate error categories (storage-error, concurrency-error, etc.)
- Specialized subtypes for transaction failures (deadlock, timeout, resource)

### Key Properties

Property | Value | Rationale
----------|-------|----------
**Completeness** | 🟡 Partial | Covers major error scenarios but missing some (graph-not-found, schema mismatch)
**Hierarchy** | 🟡 Flat | Only 1 inheritance chain; could benefit from intermediate categories
**Specificity** | 🟡 Mixed | Some conditions specific (stale-revision-error), others generic (transaction-error)
**Extensibility** | ✅ Good | Users can define subclasses for specialized handling
**Backward compatibility** | ⚠️ At risk | "Slave" terminology may be renamed; Phase 3 will freeze all condition names
**Thread-safety** | ✅ Safe | Exceptions are immutable once created (safe for multi-threaded handling)

## Interface Definition

### Operation 1: Define/Create a Condition Instance

**Signature:**

Create instance of any condition type with specified slots.

**Semantics:**

```lisp
; Example 1: slave-auth-error
(make-condition 'slave-auth-error
  :reason "invalid token"
  :host "db2.example.com")
→ <SLAVE-AUTH-ERROR instance>

; Example 2: stale-revision-error
(make-condition 'stale-revision-error
  :instance <VERTEX abc>
  :current-revision 5)
→ <STALE-REVISION-ERROR instance>

; Example 3: duplicate-key-error
(make-condition 'duplicate-key-error
  :instance "vertex-index"
  :key "v-123")
→ <DUPLICATE-KEY-ERROR instance>
```

**Returns:**
- Condition object of specified type

**Guarantee:**
- Condition object is created with all specified slots filled
- Condition is immutable (cannot be modified after creation)
- Can be caught with `handler-case` or `catch`

**Performance:**
- **Time:** O(1) instance creation
- **Space:** O(n) where n = number of slots

**Common patterns:**

```lisp
; Pattern A: Raise immediately (most common)
(error 'transaction-error :reason "deadlock detected")

; Pattern B: Create then raise
(let ((cond (make-condition 'stale-revision-error
              :instance node
              :current-revision actual-rev)))
  (signal cond))

; Pattern C: Create for testing
(let ((cond (make-condition 'invalid-view-error
              :class-name "person"
              :view-name "all-people")))
  (assert (invalid-view-error-p cond)))
```

### Operation 2: Signal/Raise a Condition

**Signature:**

Signal (raise) a condition of specified type, interrupting normal flow.

**Semantics:**

When condition is signaled:
1. Exception handling mechanism searches for matching handler
2. If handler found: handler code executes, normal flow interrupted
3. If no handler: stack unwinds, program terminates with error message

**Returns:**
- If caught: none (handler determines what happens next)
- If uncaught: program terminates

**Examples:**

```lisp
; Typical Layer 2-7 usage
(defun lookup-vertex (graph vertex-id)
  (let ((vertex (gethash vertex-id (graph-vertices graph))))
    (if vertex
      (if (deleted-p vertex)
        (error 'vertex-already-deleted-error :node vertex)  ;; Signal error
        vertex)
      (error 'nonexistent-key-error                         ;; Signal error
        :instance "vertex-index"
        :key vertex-id))))

; User code catches
(handler-case
  (lookup-vertex graph "v-123")
  (vertex-already-deleted-error (e)
    (format t "Vertex was deleted: ~A~%" (node e)))
  (nonexistent-key-error (e)
    (format t "Vertex not found: ~A~%" (key e))))
```

**Control flow:**

```
User code calls (lookup-vertex graph "v-123")
  ↓
Layer 2 code executes lookup
  ↓
Condition check: deleted-p = true
  ↓
(error 'vertex-already-deleted-error :node vertex)
  ↓
Exception handling searches for handler
  ↓
Matches (vertex-already-deleted-error (e) ...)
  ↓
Handler executes: (format t "Vertex was deleted: ~A~%" (node e))
  ↓
Returns from handler
  ↓
Normal flow continues after handler-case
```

### Operation 3: Catch/Handle a Condition

**Signature:**

Catch condition with matching handler; execute recovery code.

**Semantics:**

```lisp
(handler-case
  (risky-operation)
  (condition-type-1 (var)
    ;; Recovery code 1
    (do-something-about-error var))
  (condition-type-2 (var)
    ;; Recovery code 2
    (do-different-thing var)))
```

**Returns:**
- Return value of recovery code (if condition caught)
- Return value of `risky-operation` (if no error)

**Matching rules:**
1. First handler matching condition type wins
2. Inheritance: catching parent catches child
3. Example: catching `node-already-deleted-error` also catches `vertex-already-deleted-error`

**Examples:**

```lisp
; Example 1: Specific handling
(handler-case
  (update-node *transaction* node new-values)
  (stale-revision-error (e)
    (format t "Conflict: expected rev ~A, got ~A~%"
            node (current-revision e))
    (retry-transaction)))

; Example 2: Multiple handlers
(handler-case
  (make-vertex graph type-name data)
  (serialization-error (e)
    (log-error "Cannot serialize ~A: ~A" (instance e) (reason e)))
  (duplicate-key-error (e)
    (format t "Type already exists: ~A~%" (key e))))

; Example 3: Catch parent catches children
(handler-case
  (some-operation)
  (node-already-deleted-error (e)
    ;; Catches both vertex-already-deleted-error AND edge-already-deleted-error
    (format t "Node deleted: ~A~%" (node e))))
```

### Operation 4: Inspect Error Context via Slots

**Signature:**

Access condition slots to get error context information.

**Semantics:**

Each condition type has specific slots containing context about what failed.

**Common slots:**
- `reason` — human-readable explanation
- `instance` — object that failed (vertex, edge, index, etc.)
- `key` — key involved in error (for index operations)
- `node` — node involved in error (vertex or edge)
- `message` — detailed message string
- `host` — host involved (for replication errors)
- `current-revision` — actual revision number (for optimistic locking)
- `class-name` — type name (for view errors)
- `view-name` — view identifier (for view errors)

**Access patterns:**

```lisp
(handler-case
  (operation)
  (stale-revision-error (e)
    ;; Access slots via automatically-generated accessors
    (let ((instance (stale-revision-error-instance e))
          (current-rev (stale-revision-error-current-revision e)))
      (format t "Conflict: instance=~A, current-revision=~A~%"
              instance current-rev))))

(handler-case
  (operation)
  (duplicate-key-error (e)
    ;; Or via slot-value (more verbose)
    (let ((key (slot-value e 'key)))
      (format t "Duplicate: ~A~%" key))))
```

**Guarantee:**
- Slots are filled at condition creation time
- Cannot be modified after creation (immutable)
- Always safe to access in exception handler

**Performance:**
- **Time:** O(1) slot access (direct field read)
- **Space:** O(1) (no allocation)

## Variants / Specializations

VivaceGraph's condition system has **minimal specialization** — only one inheritance chain:

### Inheritance Tree

```
error (Lisp standard)
  ├─ slave-auth-error (Lines 3-8)
  │  └─ No children
  │
  ├─ transaction-error (Lines 10-14)
  │  └─ No children
  │
  ├─ serialization-error (Lines 16-22)
  │  └─ No children
  │
  ├─ deserialization-error (Lines 24-30)
  │  └─ No children
  │
  ├─ stale-revision-error (Lines 32-38)
  │  └─ No children
  │
  ├─ duplicate-key-error (Lines 40-46)
  │  └─ No children
  │
  ├─ nonexistent-key-error (Lines 48-54)
  │  └─ No children
  │
  ├─ node-already-deleted-error (Lines 56-60)
  │  ├─ vertex-already-deleted-error (Lines 62-63)
  │  └─ edge-already-deleted-error (Lines 65-66)
  │
  ├─ invalid-view-error (Lines 68-75)
  │  └─ No children
  │
  └─ view-lock-error (Lines 77-83)
     └─ No children
```

### Specialization Chain: node-already-deleted-error

**Parent:** error
**Children:** 
- `vertex-already-deleted-error` (empty specialization)
- `edge-already-deleted-error` (empty specialization)

**Characteristics:**

```lisp
(define-condition node-already-deleted-error (error)
  ((node :initarg :node))
  (:report (lambda (error stream)
             (format stream "Node ~A already deleted" (node error)))))

(define-condition vertex-already-deleted-error (node-already-deleted-error)
  ())  ;; Inherits node slot and :report from parent

(define-condition edge-already-deleted-error (node-already-deleted-error)
  ())  ;; Inherits node slot and :report from parent
```

**Purpose of specialization:**
- Type discrimination at runtime
- Allows specific handling for vertex vs. edge deletion
- Can catch parent to handle both

**Usage:**

```lisp
; Specific handling
(handler-case
  (traverse graph start-vertex)
  (vertex-already-deleted-error (e)
    (format t "Start vertex was deleted: ~A~%" (node e)))
  (edge-already-deleted-error (e)
    (format t "Edge was deleted: ~A~%" (node e))))

; Generic handling (catch parent)
(handler-case
  (traverse graph start-vertex)
  (node-already-deleted-error (e)
    (format t "A node was deleted: ~A~%" (node e))))
```

**Analysis of specialization:**
- ✅ Allows type-specific error handling
- ❌ Subclasses add no functionality (empty definitions)
- ⚠️ Could be achieved with slot (e.g., `(node-type 'vertex)` in parent)
- ⚠️ Maintenance burden: if parent changes, children must too

## Usage Patterns

### Pattern 1: Simple Error Catching

**Context:** Basic error handling for common failures.

**Example:**

```lisp
(defun safe-lookup (graph vertex-id)
  (handler-case
    (lookup-vertex graph vertex-id)
    (nonexistent-key-error ()
      (format t "Vertex not found~%")
      nil)
    (vertex-already-deleted-error ()
      (format t "Vertex was deleted~%")
      nil)))
```

**How it works:**
1. `lookup-vertex` attempts to find vertex
2. If `nonexistent-key-error` raised, first handler executes, returns nil
3. If `vertex-already-deleted-error` raised, second handler executes, returns nil
4. If no error, returns vertex

**Notes:**
- Ignores error context (doesn't access slots)
- Simple boolean logic (exists or not)
- Good for basic presence checks

### Pattern 2: Error Context Inspection

**Context:** Extracting information from error for logging/recovery.

**Example:**

```lisp
(defun resilient-update (graph vertex-id new-data)
  (handler-case
    (with-transaction
      (let ((node (lookup-vertex graph vertex-id)))
        (update-node *transaction* node new-data)))
    (stale-revision-error (e)
      (format t "Conflict detected: ~A has revision ~A~%"
              (stale-revision-error-instance e)
              (stale-revision-error-current-revision e))
      (exponential-backoff-and-retry graph vertex-id new-data))))
```

**How it works:**
1. Attempt update
2. If `stale-revision-error`, extract instance and current-revision
3. Log detailed error information
4. Implement retry strategy (exponential backoff)

**Pattern:** Inspect slots for recovery decision

### Pattern 3: Layered Error Handling

**Context:** Multiple layers of handlers; specific handling at different levels.

**Example:**

```lisp
; Low-level: Layer 2 catches and logs
(defun update-internal (node new-data)
  (handler-case
    (serialize-to-disk node new-data)
    (serialization-error (e)
      (log-error "Serialization failed for ~A: ~A"
                 (serialization-error-instance e)
                 (serialization-error-reason e))
      (error 'transaction-error
        :reason "Data serialization failed"))))

; Mid-level: Layer 3 catches transaction errors
(defun with-transaction-body (body)
  (handler-case
    (funcall body)
    (transaction-error (e)
      (rollback)
      (notify-client "Transaction failed: ~A"
                     (transaction-error-reason e)))))

; High-level: User code catches business logic errors
(defun user-operation (graph data)
  (handler-case
    (with-transaction
      (update-internal vertex data))
    (transaction-error (e)
      (prompt-user-retry))))
```

**Pattern:** Errors percolate up; each layer adds context/recovery

### Pattern 4: Catching Parent Types (Category Handling)

**Context:** Handling multiple related errors same way.

**Example:**

```lisp
; Catch all deletion-related errors
(defun safe-traverse (graph start-vertex)
  (handler-case
    (traverse graph start-vertex)
    (node-already-deleted-error (e)
      ;; Catches both vertex-already-deleted-error
      ;; AND edge-already-deleted-error
      (format t "Deleted node encountered: ~A~%"
              (node e))
      (return-from-traversal))))
```

**Advantage:**
- Single handler for related error types
- Simpler code (catch one type, not N types)
- Easier to extend (add new subtype, doesn't break existing handler)

**Pattern:** Inheritance provides error categorization

### Pattern 5: Mapping Conditions to Recovery Actions

**Context:** Different recovery strategy for each error type.

**Example:**

```lisp
(defun robust-graph-operation (graph op-fn)
  (handler-case
    (funcall op-fn graph)
    
    ; Replication errors → notify admin
    (slave-auth-error (e)
      (send-alert (format nil "Replication auth failed at ~A: ~A"
                          (slave-auth-error-host e)
                          (slave-auth-error-reason e))))
    
    ; Concurrency errors → retry
    (stale-revision-error (e)
      (retry-with-exponential-backoff op-fn graph))
    
    ; Lock errors → wait and retry
    (view-lock-error (e)
      (sleep 1)
      (retry-with-exponential-backoff op-fn graph))
    
    ; Resource errors → backoff heavily
    (duplicate-key-error (e)
      (sleep 10)
      (retry-with-exponential-backoff op-fn graph))
    
    ; Data format errors → fail hard
    (deserialization-error (e)
      (notify-admin "Data corruption detected")
      (error "Cannot continue"))))
```

**Pattern:** Error type → recovery strategy mapping

## Implementation Guide

### How Conditions Are Integrated

**Step 1: Definition** (conditions.lisp)

```lisp
(define-condition stale-revision-error (error)
  ((instance :initarg :instance)
   (current-revision :initarg :current-revision))
  (:report (lambda (error stream)
             (format stream "Attempt to update stale revision ~S of ~S."
                     (stale-revision-error-current-revision error)
                     (stale-revision-error-instance error)))))
```

**Step 2: Raising** (Layer 3 transactions)

```lisp
(defun update-node (transaction node new-values)
  (let ((expected-rev (revision node))
        (actual-rev (get-current-revision-from-db node)))
    (unless (= expected-rev actual-rev)
      (error 'stale-revision-error
        :instance node
        :current-revision actual-rev))))
```

**Step 3: Catching** (User code)

```lisp
(handler-case
  (with-transaction
    (update-node *transaction* node new-values))
  (stale-revision-error (e)
    (format t "Conflict: expected ~A, got ~A~%"
            node
            (stale-revision-error-current-revision e))
    (retry-transaction)))
```

### Condition Creation Flow

```
Step 1: Layer 2-7 detects error
  ├─ Checks precondition (e.g., revision mismatch)
  ├─ Determines error type (e.g., stale-revision-error)
  └─ Prepares error context (instance, current-revision)

Step 2: Signal condition
  (error 'stale-revision-error :instance node :current-revision actual)
  ├─ Lisp creates condition object with slots
  ├─ Searches for matching handler
  └─ If found: transfers control to handler

Step 3: Exception handler (user code)
  (handler-case ... (stale-revision-error (e) ...))
  ├─ Receives condition object 'e'
  ├─ Accesses slots: (stale-revision-error-instance e)
  ├─ Implements recovery: (retry-transaction)
  └─ Resumes normal flow after handler
```

### Automatic Accessor Generation

When `define-condition` creates a condition type, Lisp automatically generates accessor functions:

```lisp
; For this definition:
(define-condition stale-revision-error (error)
  ((instance :initarg :instance)
   (current-revision :initarg :current-revision)))

; Lisp generates these accessor functions:
(stale-revision-error-instance error)           ; get slot value
(stale-revision-error-current-revision error)   ; get slot value
(stale-revision-error-p object)                 ; type check predicate

; Usage in handler:
(handler-case
  (operation)
  (stale-revision-error (e)
    (let ((inst (stale-revision-error-instance e))
          (rev (stale-revision-error-current-revision e)))
      ...)))
```

## Performance Characteristics

### Compile-Time Cost

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Define condition | O(1) | Single class definition |
| Generate accessors | O(n) | n = number of slots (typically 1-3) |
| Total load | O(1) | Dominated by I/O, not computation |

### Runtime Cost

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Create condition | O(n) | n = number of slots to initialize |
| Signal condition | O(1) | Transfer control to handler |
| Access slot | O(1) | Direct field read via accessor |
| Catch condition | O(1) | Lookup handler in handler-case |
| Total exception flow | O(n) | n = stack frames to unwind (worst case) |

**Key insight:** Exception handling in Lisp is optimized; throwing/catching is fast compared to explicit error code paths.

### Memory Cost

| Item | Bytes | Notes |
|------|-------|-------|
| Condition object | ~64-256 | Depends on number of slots |
| Slot values | variable | Instance, reason, etc. |
| Handler frame | ~256 | Per handler-case |
| **Total per error** | ~512-512KB | Negligible; exceptions are uncommon |

## Constraints & Safety

### Safe Operations

| Operation | Safe? | Reason |
|-----------|-------|--------|
| Create condition | ✅ | Allocates fresh object; no shared state |
| Signal condition | ✅ | Exception mechanism is thread-safe |
| Catch condition | ✅ | Catches only in current thread |
| Access slots | ✅ | Immutable after creation |
| Inheritance dispatch | ✅ | Type system provides correctness |

### Unsafe Operations

| Operation | Safe? | Reason | Risk |
|-----------|-------|--------|------|
| Catch wrong type | ❌ | Handler doesn't match | Error propagates uncaught |
| Ignore slot value | ❌ | Lost error context | Recovery decisions wrong |
| Infinite retry loop | ❌ | No circuit breaker | System deadlock |
| Throwing in handler | ❌ | Masks original error | Debugging harder |

### Recommended Patterns

| Pattern | Rationale |
|---------|-----------|
| Specific handlers first | Specific cases before generic catch-all |
| Inspect error context | Use slots to make recovery decision |
| Log before recovery | Preserve error information for debugging |
| Limit retry depth | Prevent infinite loops |
| Circuit breaker on repeated failures | Prevent cascading failures |

## Edge Cases & Gotchas

### Gotcha 1: Catching Parent Catches All Children

**Problem:**

```lisp
(handler-case
  (operation)
  (error (e)  ;; Catches ALL errors, including system errors!
    (format t "Oops: ~A~%" e)))
```

**Issue:** Catching `error` (parent of all exceptions) catches:
- Your VivaceGraph conditions
- Lisp system errors (division by zero, undefined function, etc.)
- Third-party library errors

**Risk:** Masks programming errors (typos, logic bugs)

**Fix:**

```lisp
(handler-case
  (operation)
  ;; Catch only VivaceGraph conditions
  ((or transaction-error serialization-error stale-revision-error) (e)
    (format t "Graph error: ~A~%" e)))
```

### Gotcha 2: Slots May Contain Invalid Data

**Problem:**

```lisp
(handler-case
  (operation)
  (stale-revision-error (e)
    (let ((instance (stale-revision-error-instance e)))
      (setf (revision instance) new-revision)  ;; Try to fix it!
      )))
```

**Issue:** Instance is in error state; modifying it may cause further errors or corruption.

**Risk:** Attempting to "fix" object in exception handler

**Fix:** Don't modify instance; trigger retry or recovery instead.

### Gotcha 3: Empty Specializations Create Confusion

**Problem:**

```lisp
; vertex-already-deleted-error adds nothing new
(define-condition vertex-already-deleted-error (node-already-deleted-error)
  ())  ;; Empty!

; Why not just:
(typep error 'node-already-deleted-error)
(eq (type-of (node error)) 'vertex)
```

**Issue:** Unclear when to use specialized type vs. parent + type check.

**Risk:** Code inconsistency; some code uses vertex-already-deleted-error, some uses parent + check.

### Gotcha 4: Generic Conditions Are Unhelpful

**Problem:**

```lisp
; transaction-error covers too much
(error 'transaction-error :reason "deadlock detected")
(error 'transaction-error :reason "timeout")
(error 'transaction-error :reason "resource exhausted")

; User cannot differentiate
(handler-case
  (operation)
  (transaction-error (e)
    ;; What recovery strategy? Depends on reason!
    (if (string-equal (transaction-error-reason e) "deadlock")
      (retry-immediately)
      (exponential-backoff))))
```

**Issue:** User code must parse reason string to differentiate; error-prone.

**Better:** Specialized subtypes for each transaction failure mode.

### Gotcha 5: Error Message Format Fragility

**Problem:**

```lisp
; :report method is lambda
(:report (lambda (error stream)
           (format stream "Serialization failed for ~a because of ~a."
                   instance reason)))

; If format string changes, all error messages change
; User code parsing messages breaks if format changes
```

**Risk:** Changing error message format breaks user parsing logic.

**Mitigation:** Document that error message format is not part of public API; use slots instead.

### Gotcha 6: Inheritance Doesn't Help Without Intermediate Classes

**Problem:**

```
error
  ├─ slave-auth-error       (replication)
  ├─ transaction-error      (transactions)
  ├─ serialization-error    (storage)
  ├─ deserialization-error  (storage)
  ├─ stale-revision-error   (concurrency)
  ├─ duplicate-key-error    (indexing)
  ├─ nonexistent-key-error  (indexing)
  ├─ node-already-deleted-error (soft delete)
  ├─ invalid-view-error     (views)
  └─ view-lock-error        (views)
```

No way to catch all storage errors except listing both serialization + deserialization.

**Fix:** Create intermediate classes:

```
error
  ├─ replication-error
  │  └─ slave-auth-error
  ├─ storage-error
  │  ├─ serialization-error
  │  └─ deserialization-error
  ├─ concurrency-error
  │  ├─ stale-revision-error
  │  └─ view-lock-error
  ├─ index-error
  │  ├─ duplicate-key-error
  │  └─ nonexistent-key-error
  ├─ deletion-error
  │  └─ node-already-deleted-error
  │     ├─ vertex-already-deleted-error
  │     └─ edge-already-deleted-error
  └─ view-error
     └─ invalid-view-error
```

## Integration Context

### Upstream Dependencies

| Dependency | Purpose |
|-----------|---------|
| Lisp standard `define-condition` | Macro for creating conditions |
| Lisp standard `error` | Parent exception type |
| `:graph-db` package | Namespace for conditions |

**No external dependencies.** Conditions are pure Lisp standard features.

### Downstream Usage

| Consumer | What uses | Layer |
|----------|-----------|-------|
| package.lisp | Exports condition types | Layer 1 |
| utilities.lisp | Raises conditions on errors | Layer 1+ |
| Layer 2-7 files | Raise appropriate conditions | Layer 2-7 |
| User code | Catch conditions via handler-case | External |

### Layer Position

**Layer 1 (Infrastructure & Base Utilities)** is where conditions.lisp lives.

**Why Layer 1?** Conditions are referenced by all Layer 2-7 code. Must be defined before higher layers can compile.

## When to Use What

### Decision Table 1: Which Condition to Raise

| Scenario | Condition | Why |
|----------|-----------|-----|
| Vertex deleted, trying to read | `vertex-already-deleted-error` | Specific type discrimination |
| Edge deleted, trying to read | `edge-already-deleted-error` | Specific type discrimination |
| Key not found in index | `nonexistent-key-error` | Specific to lookup failures |
| Key already exists in index | `duplicate-key-error` | Specific to insert failures |
| Data cannot be serialized | `serialization-error` | Specific to storage failures |
| Data corrupted on disk | `deserialization-error` | Specific to load failures |
| Revision conflict | `stale-revision-error` | Specific to optimistic locking |
| Transaction times out | `transaction-error` | Generic (⚠️ too broad) |
| Cannot authenticate replica | `slave-auth-error` | Specific to replication |
| View doesn't exist | `invalid-view-error` | Specific to view lookup |
| Cannot acquire view lock | `view-lock-error` | Specific to view locking |

### Decision Table 2: How to Catch

| Need | Approach | Rationale |
|------|----------|-----------|
| Retry on conflict | Catch `stale-revision-error`, implement retry logic | Specific handling |
| Log all storage errors | Create intermediate `storage-error` class | Categorization |
| Notify user of failed update | Catch `transaction-error`, extract reason | Generic (currently) |
| Recover from deleted node | Catch `node-already-deleted-error` parent | Catch both vertex + edge |
| Distinguish vertex vs edge | Catch `vertex-already-deleted-error` vs `edge-already-deleted-error` | Specific types |
| Circuit breaker on failures | Catch `transaction-error`, count retries | Generic grouping |

### Decision Table 3: When to Define New Conditions

| Scenario | Action | Example |
|----------|--------|---------|
| New error scenario not covered | Define specialized condition | `transaction-deadlock-error` |
| Multiple similar errors | Create parent category | `storage-error` parent class |
| Need specific type checking | Use empty subclass | Already done for vertex/edge |
| Error needs context | Add slots | `reason`, `instance`, `key`, etc. |
| Error needs nice message | Add `:report` method | All conditions do this |

## Summary

| Aspect | Status | Details |
|--------|--------|---------|
| **Core purpose** | ✅ Clear | Define 11 exception types for error handling |
| **Completeness** | 🟡 Partial | Covers major scenarios; missing some (graph-not-found) |
| **Hierarchy** | 🟡 Flat | Only 1 inheritance chain; could benefit from intermediate classes |
| **Specificity** | 🟡 Mixed | Some specific (stale-revision), some generic (transaction-error) |
| **Naming** | 🟡 Issues | "Slave" terminology outdated; inconsistent slot names |
| **Clarity** | ⚠️ Limited | No docstrings explaining when/why each condition is raised |
| **Extensibility** | ✅ Good | Users can create subclasses for specialized handling |
| **Performance** | ✅ Excellent | Exception handling is fast in modern Lisp |
| **Thread safety** | ✅ Safe | Exception objects are immutable; safe for multi-threading |
| **Backward compatibility** | ⚠️ At risk | "Slave" may be renamed; Phase 3 will freeze all names |

### Key Insights

1. **Conditions provide structured error handling.** Each condition type carries context (instance, reason, key, etc.) enabling informed recovery decisions.

2. **Hierarchy matters.** Flat hierarchy forces users to catch all types individually. Intermediate classes (storage-error, concurrency-error, etc.) would improve usability.

3. **Specificity helps.** Specialized conditions (stale-revision-error, serialization-error) enable targeted recovery. Generic conditions (transaction-error) force users to parse reason strings.

4. **Empty specializations are questionable.** `vertex-already-deleted-error` and `edge-already-deleted-error` inherit everything; purpose could be achieved with slot-based discrimination.

5. **Slot naming should be consistent.** Using `instance` vs `node` vs `message` inconsistently makes generic error handlers harder to write.

6. **Nomenclature has implications.** "Slave-auth-error" may be renamed; Phase 3 will lock it in. Better to fix now.

7. **Transaction errors are too broad.** Deadlock, timeout, and resource exhaustion require different recovery; should be separate condition types.

8. **No documentation of when to raise.** Users don't know which condition to expect in which operation; docstrings would help.

9. **Performance is excellent.** Exception handling is lightweight; safe for frequent error scenarios.

10. **Integration with Phase 3 is critical.** Once these 11 condition names freeze, changing them requires major version bump. Better to design hierarchy properly now.

### Critical Decision: Phase 3 Readiness

**Before Phase 3 (API Stability), must:**

1. ☐ **Document when each condition is raised** (add docstrings)
2. ☐ **Decide on hierarchy** (add intermediate categories or keep flat)
3. ☐ **Rename `slave-auth-error`** to `replica-auth-error` (or deprecate)
4. ☐ **Specialize transaction-error** (create deadlock, timeout, resource subtypes)
5. ☐ **Standardize slot names** (instance, reason, context across types)

**SHOULD do:**

6. ☐ **Evaluate empty subclasses** (keep, extend, or remove vertex/edge specializations)
7. ☐ **Add missing conditions** (graph-not-found, schema-mismatch, etc.)

**NICE to have:**

8. ☐ **Create condition registry** (document mapping: error scenario → condition type)
9. ☐ **Create recovery guide** (document recovery strategy per condition type)

**Status:** Specification complete.  
**Blocking issues identified:** 0  
**Design improvements recommended:** 5 (nomenclature, hierarchy, specificity, documentation, slot naming)  
**Ready for Nivel 3 (Docstrings & Inline Comments):** YES  
**Ready for higher layers to use?** YES — Conditions work correctly; design improvements deferred.  
**Next action:** Create layer1-conditions-docstrings.lisp (Nivel 3) + address design warnings.

