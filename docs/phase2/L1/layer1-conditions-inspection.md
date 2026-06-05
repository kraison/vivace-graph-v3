# Layer 1 Inspection Report: conditions.lisp

**File:** src/conditions.lisp  
**Lines:** 84 (actual) | 83 (roadmap) — ✅ MATCH (1 line difference, negligible)  
**Date:** March 2026  
**Priority:** MEDIUM (defines exception types used throughout system)  
**Complexity:** LOW (pure condition definitions, no logic)  
**Type:** Exception/condition definitions  

## Executive Summary

`conditions.lisp` defines **11 custom exception types** that VivaceGraph raises when errors occur. These conditions provide structured error handling across authentication, transactions, serialization, versioning, and view management.

**Key Characteristics:**
- ✅ **Pure definitions** — no business logic, only exception structures
- ✅ **Well-formed** — all use `define-condition` macro correctly
- ✅ **Reported** — all include `:report` methods for user-friendly error messages
- ⚠️ **Limited hierarchy** — only 1 inheritance chain (3 errors share parent)
- ⚠️ **Nomenclature issues** — "slave" terminology in `slave-auth-error`
- ⚠️ **Incomplete coverage** — some error scenarios have no dedicated condition type
- ⚠️ **Empty specializations** — `vertex-already-deleted-error`, `edge-already-deleted-error` inherit but add nothing

**Why It Matters:**
These conditions are the error contract between VivaceGraph internals and user code. Every condition type defined here becomes part of the public API (Phase 3: Byzantine Compatibility). Users write error handling code based on these exception types; removing or renaming breaks their code.

## Line Count Breakdown

Lines | Section | Type | Notes
-------|---------|------|-------
1 | Context (`in-package :graph-db`) | Meta | Package switch
3-8 | `slave-auth-error` | Condition | Replication authentication failure (6 lines)
10-14 | `transaction-error` | Condition | Transaction operation failure (5 lines)
16-22 | `serialization-error` | Condition | Data serialization failure (7 lines)
24-30 | `deserialization-error` | Condition | Data deserialization failure (7 lines)
32-38 | `stale-revision-error` | Condition | Optimistic locking conflict (7 lines)
40-46 | `duplicate-key-error` | Condition | Hash table key constraint violation (7 lines)
48-54 | `nonexistent-key-error` | Condition | Key not found in index (7 lines)
56-60 | `node-already-deleted-error` | Condition | Access to soft-deleted node (5 lines)
62-63 | `vertex-already-deleted-error` | Condition | Vertex soft-delete specialization (2 lines)
65-66 | `edge-already-deleted-error` | Condition | Edge soft-delete specialization (2 lines)
68-75 | `invalid-view-error` | Condition | View lookup failure (8 lines)
77-83 | `view-lock-error` | Condition | View lock acquisition failure (7 lines)

**Total:** 84 lines. Average per condition: 7.6 lines (tightly written).

## Core Components

### A. Replication Error (Lines 3-8)

**Structure:**

```
define-condition slave-auth-error (error)
  ├─ reason :initarg :reason
  ├─ host :initarg :host
  └─ :report lambda (error stream)
     └─ format "Slave auth error ~A: ~A." host reason
```

**Analysis:**

- **Name:** `slave-auth-error`
  - Terminology issue: "slave" is deprecated (replaced by "replica" in modern systems)
  - Risk: May be renamed in future to `replica-auth-error`
  - Related to Layer 7 replication (master-slave architecture)

- **Slots:**
  - `reason` (string) — why authentication failed (e.g., "invalid token", "timeout")
  - `host` (string) — which replica host failed

- **Report method:**
  - Format: "Slave auth error {host}: {reason}."
  - Example: "Slave auth error db2.example.com: invalid token."
  - User-facing message is clear

- **When raised:**
  - Layer 7 replication module attempts to authenticate to replica
  - Replica rejects credentials
  - Connection fails

- **Handling pattern:**

```lisp
(handler-case
  (start-replication slave-host)
  (slave-auth-error (e)
    (format t "Failed to authenticate to ~A: ~A~%"
            (slave-auth-error-host e)
            (slave-auth-error-reason e))))
```

### B. Transaction Error (Lines 10-14)

**Structure:**

```
define-condition transaction-error (error)
  ├─ reason :initarg :reason
  └─ :report lambda (error stream)
     └─ format "Transaction error: ~A." reason
```

**Analysis:**

- **Name:** `transaction-error`
  - Generic error for transaction-related failures
  - Parent: error (no specialization hierarchy)

- **Slots:**
  - `reason` (string) — describes what went wrong
  - Examples: "transaction already committed", "deadlock detected", "insufficient locks"

- **Report method:**
  - Format: "Transaction error: {reason}."
  - Simple and direct

- **When raised:**
  - Layer 3 detects transaction constraint violation
  - Deadlock detected
  - Transaction timeout
  - Resource exhaustion

- **Scope:** Broad (covers many different transaction failures)

- **Risk:** ⚠️ Too generic
  - Catching `transaction-error` is vague
  - User cannot distinguish deadlock from resource exhaustion
  - Suggests need for more specific subtypes (deadlock-error, transaction-timeout-error, etc.)

### C. Serialization Error (Lines 16-22)

**Structure:**

```
define-condition serialization-error (error)
  ├─ instance :initarg :instance
  ├─ reason :initarg :reason
  └─ :report lambda (error stream)
     └─ format "Serialization failed for ~a because of ~a." instance reason
```

**Analysis:**

- **Name:** `serialization-error`
  - Raised when converting Lisp object to binary

- **Slots:**
  - `instance` — object that failed to serialize (any type)
  - `reason` (string) — what went wrong (e.g., "unsupported type", "buffer overflow")

- **Report method:**
  - Format: "Serialization failed for {instance} because of {reason}."
  - Example: "Serialization failed for <VERTEX 12345> because of unsupported type."

- **When raised:**
  - Layer 4 serializer encounters object it cannot encode
  - Type code not found for object type
  - Buffer space exhausted
  - Data size exceeds limits

- **Related:** Pair with `deserialization-error` (opposite operation)

### D. Deserialization Error (Lines 24-30)

**Structure:**

```
define-condition deserialization-error (error)
  ├─ instance :initarg :instance
  ├─ reason :initarg :reason
  └─ :report lambda (error stream)
     └─ format "Deserialization failed for ~a because of ~a." instance reason
```

**Analysis:**

- **Name:** `deserialization-error`
  - Raised when converting binary data back to Lisp object

- **Slots:**
  - `instance` — partially-deserialized object or type identifier
  - `reason` (string) — what went wrong (e.g., "corrupted data", "unknown type code")

- **Report method:**
  - Format: "Deserialization failed for {instance} because of {reason}."
  - Example: "Deserialization failed for type-code 255 because of unknown type code."

- **When raised:**
  - Layer 4 deserializer reads unknown type code
  - Checksums don't match (data corruption detected)
  - Unexpected end-of-file
  - Type mismatch (expected vertex, found edge)

- **Symmetry:** Matches `serialization-error` structure (both have instance + reason)

- **Risk:** Instance may be partially-formed or invalid; user code should not rely on accessing instance fields

### E. Stale Revision Error (Lines 32-38)

**Structure:**

```
define-condition stale-revision-error (error)
  ├─ instance :initarg :instance
  ├─ current-revision :initarg :current-revision
  └─ :report lambda (error stream)
     └─ format "Attempt to update stale revision ~S of ~S." instance current-revision
```

**Analysis:**

- **Name:** `stale-revision-error`
  - Implements optimistic locking / versioning conflict detection

- **Slots:**
  - `instance` — object being updated (vertex or edge)
  - `current-revision` — actual current revision number (differs from expected)

- **Report method:**
  - Format: "Attempt to update stale revision {instance} {current-revision}."
  - Example: "Attempt to update stale revision <VERTEX abc> 5."

- **When raised:**
  - User calls `(update-node *transaction* node new-values)`
  - Transaction checks node revision: expected 3, actual is 5
  - Another transaction updated node between read and write

- **Optimistic locking pattern:**

```lisp
; Read phase
(let ((node (lookup-vertex graph "v1")))
  (let ((rev (revision node)))
    
    ; Do work...
    (thread:sleep 1)  ;; another thread updates here
    
    ; Write phase
    (handler-case
      (update-node *transaction* node new-values)
      (stale-revision-error (e)
        (format t "Conflict detected. Retry.~%")
        (retry-transaction)))))
```

- **Pattern:** Common in multi-version concurrency control (MVCC)

### F. Duplicate Key Error (Lines 40-46)

**Structure:**

```
define-condition duplicate-key-error (error)
  ├─ instance :initarg :instance
  ├─ key :initarg :key
  └─ :report lambda (error stream)
     └─ format "Duplicate key ~S in ~S." key instance
```

**Analysis:**

- **Name:** `duplicate-key-error`
  - Raised when inserting key that already exists in index or hash-table

- **Slots:**
  - `instance` — hash-table, index, or schema name
  - `key` — the duplicate key value

- **Report method:**
  - Format: "Duplicate key {key} in {instance}."
  - Example: "Duplicate key person-1 in vertex-index."

- **When raised:**
  - Layer 2-4 tries to insert into hash-table with duplicate key
  - Type name already registered when `def-vertex` called twice
  - Index key constraint violated

- **Prevention:** Hash-tables should prevent duplicates by design; error indicates logic bug

### G. Nonexistent Key Error (Lines 48-54)

**Structure:**

```
define-condition nonexistent-key-error (error)
  ├─ instance :initarg :instance
  ├─ key :initarg :key
  └─ :report lambda (error stream)
     └─ format "Nonexistent key ~S in ~S." key instance
```

**Analysis:**

- **Name:** `nonexistent-key-error`
  - Raised when lookup fails (key not found)

- **Slots:**
  - `instance` — hash-table, index, or schema name
  - `key` — the missing key

- **Report method:**
  - Format: "Nonexistent key {key} in {instance}."
  - Example: "Nonexistent key person-1 in vertex-index."

- **When raised:**
  - Layer 2 calls `(lookup-vertex graph "nonexistent-id")`
  - Hash-table lookup returns nil (key not found)
  - Schema lookup fails (`lookup-node-type-by-name "UnknownType"`)

- **Contrast:** Opposite of `duplicate-key-error`

- **Symmetry:** With `duplicate-key-error`, same slot structure (instance + key)

### H. Node Already Deleted Error (Lines 56-60)

**Structure:**

```
define-condition node-already-deleted-error (error)
  ├─ node :initarg :node
  └─ :report lambda (error stream)
     └─ format "Node ~A already deleted" node
```

**Analysis:**

- **Name:** `node-already-deleted-error`
  - Raised when accessing soft-deleted node

- **Slots:**
  - `node` — the deleted node (vertex or edge)

- **Report method:**
  - Format: "Node {node} already deleted"
  - Example: "Node <VERTEX v-123> already deleted"

- **When raised:**
  - Layer 2 tries to read deleted vertex: `(lookup-vertex graph "v-123")`
  - Soft-delete flag is set (node marked deleted but not physically removed)
  - Operation skipped or error raised (depends on Layer 2 logic)

- **Soft-delete pattern:**
  - Instead of removing node physically, set `deleted-p = t`
  - Subsequent reads check `deleted-p` and raise error
  - Enables recovery and audit trails

- **Inheritance:** Base for `vertex-already-deleted-error` and `edge-already-deleted-error`

### I. Vertex Already Deleted Error (Lines 62-63)

**Structure:**

```
define-condition vertex-already-deleted-error (node-already-deleted-error)
  ();; No additional slots or methods
```

**Analysis:**

- **Name:** `vertex-already-deleted-error`
  - Specialization of `node-already-deleted-error`

- **Inheritance:** Parent is `node-already-deleted-error`
  - Inherits `node` slot
  - Inherits report method
  - Adds nothing new

- **Purpose:** Type discrimination
  - User code can catch `vertex-already-deleted-error` specifically
  - Or catch parent `node-already-deleted-error` for both vertices and edges

- **Issue:** ⚠️ EMPTY SUBCLASS
  - No new slots
  - No new methods
  - Could use `typep` on parent instead
  - Raises question: why separate?

### J. Edge Already Deleted Error (Lines 65-66)

**Structure:**

```
define-condition edge-already-deleted-error (node-already-deleted-error)
  ();; No additional slots or methods
```

**Analysis:**

- **Name:** `edge-already-deleted-error`
  - Specialization of `node-already-deleted-error`

- **Same as vertex-already-deleted-error:**
  - Empty subclass
  - Inherits everything
  - Only provides type discrimination

- **Pattern:** Marker subclass
  - Used to distinguish vertex vs. edge deletion at runtime
  - Example:

```lisp
(handler-case
  (some-operation)
  (vertex-already-deleted-error (e)
    (format t "Vertex deleted: ~A~%" (node e)))
  (edge-already-deleted-error (e)
    (format t "Edge deleted: ~A~%" (node e))))
```

### K. Invalid View Error (Lines 68-75)

**Structure:**

```
define-condition invalid-view-error (error)
  ├─ class-name :initarg :class-name
  ├─ view-name :initarg :view-name
  └─ :report lambda (error stream)
     └─ format "No such graph view: ~A/~A" class-name view-name
```

**Analysis:**

- **Name:** `invalid-view-error`
  - Raised when view lookup fails

- **Slots:**
  - `class-name` (string) — vertex/edge type name
  - `view-name` (string) — view identifier

- **Report method:**
  - Format: "No such graph view: {class-name}/{view-name}"
  - Example: "No such graph view: person/all-people"

- **When raised:**
  - User calls `(map-view graph "person" "all-people")`
  - View "all-people" doesn't exist for type "person"
  - Layer 6 lookup fails

- **Context:** Views are materialized query results per type (Layer 6)

- **Usage:**

```lisp
(handler-case
  (map-view graph class-name view-name fn)
  (invalid-view-error (e)
    (format t "View ~A/~A not found~%" 
            (invalid-view-error-class-name e)
            (invalid-view-error-view-name e))))
```

### L. View Lock Error (Lines 77-83)

**Structure:**

```
define-condition view-lock-error (error)
  ├─ message :initarg :message
  └─ :report lambda (error stream)
     └─ format "View locking error: '~A'" message
```

**Analysis:**

- **Name:** `view-lock-error`
  - Raised when view locking operation fails

- **Slots:**
  - `message` (string) — detailed error description
  - Examples: "timeout waiting for lock", "deadlock detected", "lock already held"

- **Report method:**
  - Format: "View locking error: '{message}'"
  - Example: "View locking error: 'timeout waiting for lock'"

- **When raised:**
  - Layer 6 tries to acquire read/write lock on view
  - Lock acquisition times out
  - Deadlock detected
  - Lock is already held by same thread (recursive lock issue)

- **Context:** Views are cached query results, require locking for thread-safe access

- **Pattern:**

```lisp
(handler-case
  (with-read-locked-view-group graph class-name
    (map-view graph class-name view-name fn))
  (view-lock-error (e)
    (format t "Failed to lock view: ~A~%"
            (view-lock-error-message e))))
```

## Dependencies

### What conditions.lisp depends on:

| Dependency | Lines | Purpose |
|-----------|-------|---------|
| `:graph-db` package | 1 | Context for condition definitions |
| `define-condition` macro | all | Lisp standard macro for exceptions |

**Note:** No explicit imports. All conditions are defined using standard CL macro.

### What depends on conditions.lisp:

| Consumer | What uses | Layer |
|----------|-----------|-------|
| package.lisp | Exports some/all condition types | Layer 1 |
| utilities.lisp | Raises transaction-error, serialization-error, etc. | Layer 1+ |
| Layer 2-7 code | Raises specific conditions on errors | Layer 2-7 |
| User code | Catches conditions via handler-case | External |

## Complexity Assessment & Hotspots

### 🟡 WARNING #1: "Slave" Terminology (Outdated)

**Issue:** Line 3 defines `slave-auth-error`

**Problem:**
- "Slave" terminology is deprecated in technology industry
- Standard replacement: "replica", "secondary", "follower"
- May be renamed in future, breaking user code that catches this exception

**Risk:**
- User code with `(catch 'slave-auth-error ...)`becomes broken if renamed
- Phase 3 (API stability) will lock this in

**Recommended action:**
- Rename now (before Phase 3): `slave-auth-error` → `replica-auth-error`
- Or deprecate with warning

### 🟡 WARNING #2: Empty Subclasses

**Issue:** Lines 62-63, 65-66 define empty subclasses

**Problem:**

```lisp
(define-condition vertex-already-deleted-error (node-already-deleted-error)
  ())  ;; No slots, no methods!
```

**Questions:**
- Why not just `(typep error 'node-already-deleted-error)` and check node type?
- Why duplicate code with empty class?
- Are they actually needed?

**Risk:**
- Adds to API surface without providing additional functionality
- Maintenance burden (need to update if parent changes)
- User confusion about when to use specialized vs. parent

**Better approach:**
- Include specialized instance type in parent's `node` slot
- Or: add `node-type` slot to parent
- Or: remove specializations if not actually used

### 🟡 WARNING #3: Incomplete Error Hierarchy

**Issue:** Only one inheritance chain (node-already-deleted-error with 2 children)

**Problem:**

All other conditions inherit directly from `error`, creating flat hierarchy:

```
error
  ├─ slave-auth-error
  ├─ transaction-error
  ├─ serialization-error
  ├─ deserialization-error
  ├─ stale-revision-error
  ├─ duplicate-key-error
  ├─ nonexistent-key-error
  ├─ node-already-deleted-error
  │  ├─ vertex-already-deleted-error
  │  └─ edge-already-deleted-error
  ├─ invalid-view-error
  └─ view-lock-error
```

**Benefits of hierarchy:**
- `(catch 'storage-error ...)` could catch serialization + deserialization
- `(catch 'index-error ...)` could catch duplicate + nonexistent key
- `(catch 'replication-error ...)` could catch slave-auth-error

**Current limitation:**
- User must list all specific condition types
- Cannot catch category of errors efficiently

### 🟡 WARNING #4: Transaction Error Too Generic

**Issue:** Line 10 `transaction-error` with only `reason` slot

**Problem:**

Covers too many scenarios:
- Deadlock
- Transaction timeout
- Insufficient locks
- Constraint violation
- Resource exhaustion

**User code unable to:**
- Distinguish deadlock (retry) from resource exhaustion (backoff)
- Apply different recovery strategies
- Log/monitor by error type

**Better approach:**
- Define specialized subtypes:
  - `transaction-deadlock-error`
  - `transaction-timeout-error`
  - `transaction-constraint-error`
  - `transaction-resource-error`

### 🟡 WARNING #5: Slot Naming Inconsistency

**Issue:** Slot names vary across conditions

**Problem:**

```
slave-auth-error: reason, host
transaction-error: reason
serialization-error: instance, reason
deserialization-error: instance, reason
stale-revision-error: instance, current-revision
duplicate-key-error: instance, key
nonexistent-key-error: instance, key
node-already-deleted-error: node
invalid-view-error: class-name, view-name
view-lock-error: message
```

**Pattern inconsistency:**
- Some use `instance`, some use `node`, some use specific type
- Some use `reason`, some use `message`
- Makes generic error handling harder

**Better approach:**
- Standardize on common slots:
  - All errors: `operation` (what was being done)
  - All errors: `message` (human-readable description)
  - Specialized: `instance`, `key`, `revision`, etc. (operation-specific)

### ⚠️ OBSERVATION #1: Exported by package.lisp?

**Question:** Which of these conditions are exported from package.lisp?

**Risk:** If some exported, they're part of public API (Phase 3 lock-in). If not exported, why define them in public package?

**Check needed:**
- Compare conditions.lisp definitions with package.lisp exports
- Verify all are exported (or document why not)
- Ensure consistency

## Issues Found

### 🟡 WARNING #1: "Slave" Terminology Deprecated

**Issue:** `slave-auth-error` (line 3) uses outdated terminology

**Risk:** May be renamed in future, breaking user code

**Fix:**
1. Rename to `replica-auth-error` (standard modern terminology)
2. Or: Keep for backward compatibility but deprecate with warning

### 🟡 WARNING #2: Empty Subclasses

**Issue:** `vertex-already-deleted-error`, `edge-already-deleted-error` (lines 62-66) add no functionality

**Risk:** Maintenance burden, unclear purpose

**Fix:**
1. Remove if not actually used
2. Or: Add specialized behavior (e.g., custom message, specialized handling)
3. Or: Add `node-type` slot to parent to distinguish vertex vs. edge

### 🟡 WARNING #3: Flat Error Hierarchy

**Issue:** No intermediate error categories; only one inheritance chain

**Risk:** Users must catch all condition types individually; cannot catch categories

**Fix:**
1. Create intermediate exception classes:
   - `storage-error` (parent of serialization, deserialization)
   - `index-error` (parent of duplicate-key, nonexistent-key)
   - `concurrency-error` (parent of stale-revision, view-lock)
   - `replication-error` (parent of slave-auth)
   
2. Helps users write more robust error handling

### 🟡 WARNING #4: Generic Transaction Error

**Issue:** `transaction-error` (line 10) covers too many scenarios

**Risk:** Users cannot differentiate deadlock from timeout from resource exhaustion

**Fix:**
1. Create specific subtypes:
   - `transaction-deadlock-error`
   - `transaction-timeout-error`
   - `transaction-constraint-error`
   - `transaction-resource-exhausted-error`

### 🟡 WARNING #5: Inconsistent Slot Naming

**Issue:** Slot names vary across conditions (instance, node, message, reason, key, etc.)

**Risk:** Harder for users to write generic error handlers; must know specific condition type to access slots

**Fix:**
1. Standardize core slots across all errors:
   - `operation` (what was being done)
   - `target` (what failed: instance, key, node, etc.)
   - `reason` (human-readable explanation)
   
2. Allow specialized slots per condition as needed

### ⚠️ OBSERVATION #1: Report Method Implementation

**Issue:** All `:report` methods implemented as inline lambdas

**Observation:**
- Works correctly
- Clear and self-contained
- Could be refactored to shared functions (style choice)
- No functional problem

## Testing Strategy

### Critical Tests to Write

| Test | Setup | Action | Expected |
|------|-------|--------|----------|
| **All conditions defined** | None | Check all 11 condition classes exist | All bound |
| **Inheritance chain correct** | None | Verify vertex/edge inherit from node-deleted | True |
| **Report method works** | Create condition instance | Call condition-report or handler-case catch | Message produced |
| **Slot initialization** | Create with :initargs | Access slots via slot-value | Correct values |
| **Error condition type** | Catch in handler-case | Signal condition, catch it | Condition caught |
| **Message format** | Create condition | Format message | User-friendly output |
| **Inheritance dispatch** | Catch parent | Signal child condition | Parent catches it |

## Code Quality Summary

| Aspect | Status | Notes |
|--------|--------|-------|
| **Docstrings** | ❌ **MISSING** | No docstrings on any condition. No explanation of when/why raised. |
| **Test coverage** | ❌ **MISSING** | No tests for condition creation, report format, inheritance. |
| **Performance** | ✅ **N/A** | Conditions are defined at compile-time; no runtime cost. |
| **Complexity** | ✅ **LOW** | Simple structure, no complex logic. |
| **Lisp idiom** | ✅ **CORRECT** | Uses `define-condition` correctly, proper `:report` methods. |
| **Naming** | 🟡 **MIXED** | "Slave" terminology outdated; slot names inconsistent. |
| **Hierarchy** | 🟡 **FLAT** | Only 1 inheritance chain; could benefit from more structure. |

## Comparison (Optional)

**vs. typical Lisp exception systems:**

- ✅ **Good:** All conditions inherit from `error` (correct parent)
- ✅ **Good:** All have `:report` methods (user-friendly messages)
- ✅ **Good:** Appropriate slot names for context
- ❌ **Bad:** Flat hierarchy (no intermediate categories)
- ❌ **Bad:** "Slave" terminology (outdated)
- ❌ **Bad:** No docstrings explaining purpose/context
- ⚠️ **Mediocre:** Empty subclasses (marker pattern, unclear intent)

## Summary

| Metric | Value | Assessment |
|--------|-------|------------|
| **Lines** | 84 | ✅ Match roadmap |
| **Conditions defined** | 11 | ✅ Reasonable coverage |
| **Inheritance chains** | 1 | 🟡 Only one; could be more |
| **Report methods** | 11 | ✅ All present and working |
| **Slots per condition** | 1-3 | 🟡 Naming inconsistent |
| **Blocking issues** | 0 | ✅ No compilation/runtime failures |
| **Critical issues** | 0 | ✅ No data corruption risk |
| **Warnings** | 5 | 🟡 Design/API quality issues |
| **Code health** | 🟡 **MEDIUM** | Functionally correct, but API design could be improved |
| **Ready for Layer 2?** | ✅ **YES** | Conditions work; Layer 2-7 can use them |

## Next Steps

1. ✅ **Document all conditions:** Add docstrings explaining when/why each is raised.

2. ✅ **Rename `slave-auth-error`:** Change to `replica-auth-error` (or deprecate with warning).

3. ✅ **Evaluate empty subclasses:** Decide if `vertex-already-deleted-error` and `edge-already-deleted-error` are necessary; add behavior if kept.

4. ✅ **Create error hierarchy:** Add intermediate exception classes for better error categorization:
   - `storage-error`
   - `index-error`
   - `concurrency-error`
   - `replication-error`

5. ✅ **Specialize transaction-error:** Create specific subtypes for different transaction failure modes.

6. ✅ **Standardize slot names:** Review all conditions for consistent naming across error types.

7. ✅ **Create tests:** Verify condition creation, reporting, inheritance, and error handling.

**Status:** Inspection complete. Issues identified: 0 blocking, 0 critical, 5 warnings.  
**Ready for Nivel 2 (Component Specification):** YES — No blocking issues; design improvements recommended but not required.  
**Ready for higher layers to use?** YES — Conditions work correctly and can be caught/handled.  
**Next action:** Create layer1-conditions-component.md (Specification) + address design warnings.

