# Layer 1 Component Specification: clos.lisp

**Component:** CLOS Metaclass & Custom Slot Access Protocol  
**File:** src/clos.lisp  
**Lines:** 89  
**Type:** CLOS metaclass definitions + MOP method overrides  
**Purpose:** Enable graph nodes to separate metadata (built-in) from properties (user-defined) while maintaining transparent slot-value access

---

## Conceptual Model

### Core Idea

**clos.lisp implements a dual-slot architecture for graph nodes.**

```
┌─────────────────────────────────────────────────────┐
│            graph-class (custom metaclass)          │
│                                                     │
│  ┌────────────────────────────────────────────┐   │
│  │  node (instance class)                     │   │
│  │                                            │   │
│  │  Meta-slots (built-in graph metadata):    │   │
│  │  ├─ id (16-byte UUID)                     │   │
│  │  ├─ %type-id (classification)             │   │
│  │  ├─ %revision (MVCC counter)              │   │
│  │  ├─ %deleted-p (soft-delete flag)         │   │
│  │  ├─ %heap-written-p, %written-p           │   │
│  │  ├─ %data-pointer (storage offset)        │   │
│  │  └─ ... (10 total meta-slots)             │   │
│  │                                            │   │
│  │  Property storage (user-defined):         │   │
│  │  └─ %data → alist of (:key . value) pairs│   │
│  │     (accessed transparently via slot-value)   │   │
│  │                                            │   │
│  └────────────────────────────────────────────┘   │
│                                                     │
│  Slot Access Protocol (MOP):                       │
│  ├─ slot-value → intercept (getter)               │
│  ├─ (setf slot-value) → intercept (setter)        │
│  └─ slot-makunbound → intercept (deletion)        │
└─────────────────────────────────────────────────────┘
```

### Motivation

**Without clos.lisp:**
- Every property would need explicit accessor definition
- Adding properties requires modifying node class definition
- Code must distinguish property access from metadata access
- Verbose: `(get-user-property node "name")` vs `(slot-value node :name)`

**With clos.lisp:**
- ✅ Properties accessed transparently: `(slot-value node :name)`
- ✅ Properties added dynamically (no class redefinition)
- ✅ Metadata and properties coexist seamlessly
- ✅ Uniform API (slot-value works for both)

### Abstraction Boundary

**What it exposes:**
- Custom metaclass (graph-class) for use in class definitions
- Transparent slot-value protocol (acts like normal CLOS)
- Dynamic property storage in %data slot

**What it hides:**
- Internal alist structure (%data is opaque)
- Distinction between meta-slots and properties
- Persistence triggers (save-node, transaction queuing)

**What should be hidden but isn't:** ⚠️
- MOP implementation details (hard to understand)
- Dependency on save-node (external function)
- Global state (*current-transaction*)
- SBCL-specific code (sb-mop:slot-definition-name)

### Key Properties

Property | Value | Rationale
----------|-------|----------
**Completeness** | 🟡 70% | Core metaclass present; some methods are stubs
**Consistency** | 🟡 50% | Slot access protocol mostly consistent; error handling missing
**Correctness** | 🔴 0% | Undefined references; runtime errors possible
**Performance** | 🟡 40% | O(n) alist search acceptable for small data; poor at scale
**Extensibility** | ✅ 90% | Easy to add properties; schema-less design
**Thread-safety** | 🔴 0% | No locking; concurrent access will corrupt data
**Portability** | 🔴 0% | SBCL-specific (sb-mop:); fails on CCL/LW
**Backward compatibility** | ✅ 100% | Standard CLOS interface; no breaking changes

---

## Interface Definition

### Category A: Metaclass & Slot Definition Classes

#### Interface 1: graph-class (Metaclass)

**Signature:**
```lisp
(defclass graph-class (standard-class)
  ())
```

**Purpose:** Custom metaclass for graph node classes

**Usage:**
```lisp
(defclass my-vertex ()
  ((custom-prop :initarg :custom-prop))
  (:metaclass graph-class))
```

**Behavior:**
- Acts like standard-class but intercepts slot access
- Distinguishes meta-slots from property slots
- Routes slot access via custom protocol

**Guarantee:**
- Instances use custom slot-value protocol
- Properties stored in %data slot
- Metadata in normal slots

**Performance:**
- Class creation: O(n) where n = total slots
- Instance creation: O(1)
- Slot access: O(1) for meta-slots, O(m) for properties (m = alist size)

**Common patterns:**
```lisp
; Pattern A: Define graph class
(defclass vertex ()
  ((degree :accessor degree :initform 0))
  (:metaclass graph-class))

; Pattern B: Add properties dynamically
(defvar v1 (make-instance 'vertex))
(setf (slot-value v1 :name) "Alice")    ;; Property
(setf (slot-value v1 :age) 30)          ;; Property
(setf (degree v1) 5)                    ;; Meta-slot

; Pattern C: Read properties
(slot-value v1 :name)  → "Alice"
(slot-value v1 :age)   → 30
```

**Risks:**
- ⚠️ Properties have no type checking (all stored as-is)
- ⚠️ No uniqueness constraint on properties (can overwrite)
- ⚠️ Error handling missing (nil data crashes)

---

#### Interface 2: graph-slot-definition (Mixin)

**Signature:**
```lisp
(defclass graph-slot-definition (standard-slot-definition)
  ())
```

**Purpose:** Base class for graph-specific slot definitions

**Usage:** Internal (MOP implementation detail)

**Behavior:**
- Empty mixin class
- Used as base for direct/effective slot definitions
- Placeholder for future custom slot behavior

**Guarantee:**
- Slot definitions inherit from this class
- No additional behavior (currently)

**Performance:** O(1) (no additional overhead)

**Risks:**
- ⚠️ Currently a placeholder (unclear future purpose)

---

#### Interface 3: graph-direct-slot-definition

**Signature:**
```lisp
(defclass graph-direct-slot-definition
    (standard-direct-slot-definition graph-slot-definition)
  ())
```

**Purpose:** Custom direct slot definition (MOP protocol)

**Usage:** Internal (returned by direct-slot-definition-class method)

**Behavior:**
- Wrapper around standard-direct-slot-definition
- Used during slot processing in graph-class

**Guarantee:**
- Returned by MOP protocol
- Used only internally

**Risks:**
- ⚠️ No distinguishing features (marker class only)

---

#### Interface 4: graph-effective-slot-definition

**Signature:**
```lisp
(defclass graph-effective-slot-definition
    (standard-effective-slot-definition graph-slot-definition)
  ())
```

**Purpose:** Custom effective slot definition (MOP protocol)

**Usage:** Internal (returned by effective-slot-definition-class method)

**Behavior:**
- Wrapper around standard-effective-slot-definition
- Used during effective slot computation in graph-class

**Guarantee:**
- Returned by MOP protocol
- Used only internally

**Risks:**
- ⚠️ No distinguishing features (marker class only)
- ⚠️ compute-effective-slot-definition :around is stub

---

### Category B: MOP Protocol Methods

#### Operation 1: validate-superclass

**Signature:**
```lisp
(defmethod validate-superclass ((class graph-class) (super standard-class))
  "Graph classes may inherit from ordinary classes."
  t)
```

**Purpose:** Allow graph-class to inherit from standard-class

**Semantics:**
```lisp
(defclass vertex (standard-object)
  ()
  (:metaclass graph-class))
→ Allowed (returns t)
```

**Behavior:**
- Always returns t (no validation)
- Allows any standard-class as superclass

**Guarantee:**
- Any standard-class can be inherited

**Performance:** O(1)

**Common patterns:**
```lisp
; Standard inheritance allowed
(defclass vertex () () (:metaclass graph-class))
(defclass labeled-vertex (vertex) () (:metaclass graph-class))
```

**Risks:**
- ⚠️ No validation (accepts anything; may allow invalid inheritance)

---

#### Operation 2: direct-slot-definition-class

**Signature:**
```lisp
(defmethod direct-slot-definition-class ((class graph-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'graph-direct-slot-definition))
```

**Purpose:** Return custom direct slot definition class (MOP protocol)

**Semantics:**

```lisp
(defclass vertex ()
  ((x :accessor x))
  (:metaclass graph-class))

; During class finalization, for each slot:
(direct-slot-definition-class graph-class)
→ #<CLASS GRAPH-DIRECT-SLOT-DEFINITION>
```

**Behavior:**
- Returns graph-direct-slot-definition class
- Ignores initargs

**Guarantee:**
- Consistent return value (same class every time)

**Performance:** O(1) class lookup

**Risks:**
- ⚠️ Ignores initargs (may lose configuration)

---

#### Operation 3: effective-slot-definition-class

**Signature:**
```lisp
(defmethod effective-slot-definition-class ((class graph-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'graph-effective-slot-definition))
```

**Purpose:** Return custom effective slot definition class (MOP protocol)

**Semantics:**

```lisp
; During effective slot computation:
(effective-slot-definition-class graph-class)
→ #<CLASS GRAPH-EFFECTIVE-SLOT-DEFINITION>
```

**Behavior:**
- Returns graph-effective-slot-definition class
- Ignores initargs

**Guarantee:**
- Consistent return value

**Performance:** O(1)

**Risks:**
- ⚠️ Ignores initargs (may lose configuration)

---

#### Operation 4: compute-effective-slot-definition :around

**Signature:**
```lisp
(defmethod compute-effective-slot-definition :around ((class graph-class) slot-name direct-slots)
  (let ((slot (call-next-method)))
    ;;
    slot))
```

**Purpose:** Customize effective slot computation (stub)

**Semantics:**

```lisp
; During class finalization:
(compute-effective-slot-definition graph-class :name direct-slots)
→ Calls default compute-effective-slot-definition
→ Returns result unchanged
```

**Behavior:**
1. Calls (call-next-method) to get default effective slot
2. Assigns to `slot` variable
3. Returns `slot` unchanged

**Guarantee:**
- Effective slot created normally
- No custom behavior

**Performance:** O(1) (just passthrough)

**⚠️ Status:** 🟡 STUB (empty body; intent unclear)

**Risks:**
- ⚠️ Method exists but does nothing (why?)
- ⚠️ If meant to customize, implementation missing
- ⚠️ Misleading (suggests custom behavior but doesn't provide it)

---

#### Operation 5: slot-value-using-class (GETTER)

**Signature:**
```lisp
(defmethod slot-value-using-class :around ((class graph-class) instance slot)
  (let ((slot-name (sb-mop:slot-definition-name slot)))
    (if (find slot-name *meta-slots*)
        (call-next-method)
        (let ((key (intern (symbol-name slot-name) :keyword)))
          (cdr (assoc key (data instance)))))))
```

**Purpose:** Read slot value; distinguish meta-slots from properties

**Semantics:**

```lisp
; Reading meta-slot:
(slot-value vertex-instance :id)
→ Finds :id in *meta-slots*
→ Calls default getter
→ Returns: #(0xA3 0x7F ...) (16-byte array)

; Reading property:
(slot-value vertex-instance :name)
→ Doesn't find :name in *meta-slots*
→ Converts to keyword: :name
→ Looks up in (data vertex-instance) alist
→ Returns: "Alice" (or nil if not found)
```

**Behavior:**

1. Extract slot name: `(sb-mop:slot-definition-name slot)`
2. Check if meta-slot: `(find slot-name *meta-slots*)`
   - If yes: use default getter (call-next-method)
   - If no: treat as property
3. For properties:
   - Convert slot name to keyword
   - Look up in alist: `(data instance)`
   - Return value part: `(cdr (assoc key alist))`

**Guarantee:**
- Meta-slots return their actual values
- Properties return values stored in %data
- Missing properties return nil

**Performance:**
- Meta-slots: O(1) (direct slot access)
- Properties: O(n) where n = alist size (linear search)

**Common patterns:**
```lisp
; Pattern A: Read meta-slot
(id vertex-instance)  → 16-byte UUID

; Pattern B: Read property
(slot-value vertex-instance :custom-prop)  → value

; Pattern C: Conditional read
(or (slot-value vertex-instance :name) "Unknown")
```

**Risks:**
- 🔴 BLOCKING: No error handling for nil data
  ```lisp
  (slot-value instance :prop)
  where (data instance) = nil
  → (assoc :prop nil) → nil
  → (cdr nil) → TYPE-ERROR
  ```

- 🔴 BLOCKING: No error handling for missing key
  ```lisp
  (slot-value instance :nonexistent)
  → (assoc :nonexistent alist) → nil
  → (cdr nil) → TYPE-ERROR
  ```

- 🟠 CRITICAL: Performance O(n) with large alist
  - Each property read requires linear search
  - 1000 properties = ~500 comparisons per read

- ⚠️ SBCL-specific: `(sb-mop:slot-definition-name slot)`
  - Fails on CCL/LispWorks

---

#### Operation 6: (setf slot-value-using-class) (SETTER)

**Signature:**
```lisp
(defmethod (setf slot-value-using-class) :around (new-value (class graph-class) instance slot)
  (let ((slot-name (sb-mop:slot-definition-name slot)))
    (if (find slot-name *meta-slots*)
        (call-next-method)
        (let ((key (intern (symbol-name slot-name) :keyword)))
          (setf (cdr (assoc key (data instance))) new-value)
          (if *current-transaction*
              (pushnew instance (txn-update-queue *current-transaction*) :test 'equalp :key 'id)
              (save-node instance))))))
```

**Purpose:** Write slot value; trigger persistence

**Semantics:**

```lisp
; Setting meta-slot:
(setf (slot-value vertex-instance :id) new-id)
→ Finds :id in *meta-slots*
→ Calls default setter
→ Returns: new-id

; Setting property:
(setf (slot-value vertex-instance :name) "Bob")
→ Doesn't find :name in *meta-slots*
→ Converts to keyword: :name
→ Modifies alist in (data instance)
→ If in transaction: queues instance for batch save
→ Else: saves immediately via save-node
→ Returns: "Bob"
```

**Behavior:**

1. Extract slot name
2. Check if meta-slot
   - If yes: use default setter (call-next-method)
   - If no: treat as property
3. For properties:
   - Convert to keyword
   - Find in alist: `(assoc key (data instance))`
   - Modify in-place: `(setf (cdr ...) new-value)`
   - Trigger persistence:
     - If *current-transaction*: queue for batch save
     - Else: save immediately

**Guarantee:**
- Meta-slots written to their slots
- Properties written to %data alist
- Persistence triggered after each write

**Performance:**
- Meta-slots: O(1)
- Properties: O(n) for alist search + O(I/O) for persistence

**Common patterns:**
```lisp
; Pattern A: Set property
(setf (slot-value vertex-instance :name) "Alice")

; Pattern B: Batch updates in transaction
(with-transaction (graph)
  (setf (slot-value v1 :prop1) 1)
  (setf (slot-value v2 :prop2) 2)
  (commit))  ;; Single save instead of two

; Pattern C: Update with default
(setf (slot-value vertex-instance :counter)
      (1+ (or (slot-value vertex-instance :counter) 0)))
```

**Risks:**
- 🔴 BLOCKING: No error handling for nil data
  ```lisp
  (setf (slot-value instance :prop) value)
  where (data instance) = nil
  → (assoc :prop nil) → nil
  → (setf (cdr nil) value) → TYPE-ERROR
  ```

- 🔴 BLOCKING: Undefined function: save-node
  - Function not defined in this file
  - Compilation/runtime error

- 🔴 BLOCKING: Undefined variable: *current-transaction*
  - Global variable not defined
  - Compilation error

- 🔴 BLOCKING: Undefined function: txn-update-queue
  - Function/accessor not defined
  - Compilation error

- 🟠 CRITICAL: Thread-unsafe
  ```lisp
  Thread A: (setf (slot-value instance :prop) 1)
  Thread B: (setf (slot-value instance :prop) 2)
  
  Race condition: which value wins?
  Global state (*current-transaction*) accessed without locking
  ```

- 🟠 CRITICAL: No rollback on save-node failure
  - If save-node fails, data left in inconsistent state
  - Instance modified but not persisted

- ⚠️ SBCL-specific: `(sb-mop:slot-definition-name slot)`

---

#### Operation 7: slot-makunbound-using-class

**Signature:**
```lisp
(defmethod slot-makunbound-using-class :around ((class graph-class) instance slot)
  (let ((slot-name (sb-mop:slot-definition-name slot)))
    (if (find slot-name *meta-slots*)
        (call-next-method)
        (let ((key (intern (symbol-name slot-name) :keyword)))
          (setf (data instance) (delete key (data instance) :key 'car))
          instance))))
```

**Purpose:** Unbind slot; remove property from alist

**Semantics:**

```lisp
; Unbinding property:
(slot-makunbound vertex-instance :name)
→ Finds :name not in *meta-slots*
→ Converts to keyword: :name
→ Removes from alist
→ Returns: instance

; Effect:
(slot-value vertex-instance :name)
→ Returns: nil (no longer found)
```

**Behavior:**

1. Extract slot name
2. Check if meta-slot
   - If yes: unbind normally (call-next-method)
   - If no: remove from alist
3. For properties:
   - Convert to keyword
   - Call (delete key alist :key 'car)
   - Reassign (data instance) with modified list
   - Return instance

**Guarantee:**
- Meta-slots unbound normally
- Properties removed from %data
- Slot no longer accessible after unbinding

**Performance:**
- Meta-slots: O(1)
- Properties: O(n) (linear delete + reassign)

**Common patterns:**
```lisp
; Pattern A: Remove property
(slot-makunbound vertex-instance :temporary-prop)

; Pattern B: Check and remove
(when (slot-boundp vertex-instance :cache)
  (slot-makunbound vertex-instance :cache))

; Pattern C: Clear all properties
(setf (data vertex-instance) nil)
```

**Risks:**
- 🔴 BLOCKING: No error handling for nil data
  ```lisp
  (slot-makunbound instance :prop)
  where (data instance) = nil
  → (delete :prop nil :key 'car) → nil
  → (setf (data instance) nil) → ok (no-op)
  ```

- 🟡 WARNING: (delete key alist) is destructive
  - Modifies list in-place
  - If list shared, side-effects leak

- 🟡 WARNING: Unbinding triggers persistence
  - (setf (data instance) ...) calls setter
  - Which calls save-node or queues for transaction
  - Every unbind causes I/O overhead

- ⚠️ SBCL-specific: `(sb-mop:slot-definition-name slot)`

---

### Category C: node Class

#### Data Class: node

**Signature:**
```lisp
(defclass node ()
  ((id :accessor id :initform +null-key+ :initarg :id
       :type (simple-array (unsigned-byte 8) (16)))
   (%type-id :accessor %type-id :initform 1 :initarg :%type-id
            :type (unsigned-byte 16))
   ... (13 more slots)
   )
  (:metaclass graph-class))
```

**Purpose:** Base class for graph nodes

**Slots:**
- 13 metadata slots: id, %type-id, %revision, %deleted-p, etc.
- 1 property storage slot: %data (alist)

**Constructors:**

```lisp
(make-instance 'node :id some-uuid)
→ #<NODE id=...>
```

**Accessors:**

```lisp
(id node)  → 16-byte UUID
(%type-id node)  → classification
(%revision node)  → MVCC counter
(%data node)  → alist of properties
(slot-value node :custom-prop)  → user property
```

**Guarantee:**
- Metadata stored in standard slots
- Properties stored in %data
- All accessible via slot-value

**Performance:**
- Instance creation: O(1)
- Slot access: O(1) for metadata, O(n) for properties

**Common patterns:**
```lisp
; Pattern A: Create node
(defvar v (make-instance 'node :id (gen-id)))

; Pattern B: Add properties
(setf (slot-value v :name) "Alice")
(setf (slot-value v :age) 30)

; Pattern C: Read properties
(let ((name (slot-value v :name)))
  (format t "Node: ~A~%" name))

; Pattern D: Update metadata
(setf (%revision v) (1+ (%revision v)))
```

**Risks:**
- ⚠️ Properties have no type checking
- ⚠️ No schema (any property name allowed)
- ⚠️ No validation (any value allowed)

---

## Variants / Specializations

**clos.lisp has no variants; all behavior conditional on *meta-slots* list**

---

## Usage Patterns

### Pattern 1: Define Graph Class

**Context:** Create custom node class for specific entity type

**Example:**
```lisp
(defclass vertex ()
  ((degree :accessor degree :initform 0 :type integer))
  (:metaclass graph-class))

(defvar v (make-instance 'vertex :id (gen-id)))
(setf (degree v) 5)           ;; Meta-slot
(setf (slot-value v :name) "Alice")  ;; Property
```

**Pattern:** 
- Use (:metaclass graph-class) to enable custom slot access
- Metadata as normal slots
- Properties via slot-value

---

### Pattern 2: Transparent Property Access

**Context:** Code accesses properties without distinguishing from metadata

**Example:**
```lisp
(defun get-node-info (node)
  (list (id node)                           ;; Metadata
        (slot-value node :name)             ;; Property
        (slot-value node :age)))            ;; Property
```

**Pattern:** Uniform access via slot-value (or direct accessors)

---

### Pattern 3: Batch Updates in Transaction

**Context:** Multiple property changes queued for batch persistence

**Example:**
```lisp
(with-transaction (graph)
  (let ((v1 (get-vertex graph id1)))
    (setf (slot-value v1 :status) "processing")
    (setf (slot-value v1 :updated-at) (get-unix-time))
    (commit)))  ;; Single save
```

**Pattern:** Properties modified within transaction; save triggered on commit

---

### Pattern 4: Dynamic Properties

**Context:** Add properties without modifying class definition

**Example:**
```lisp
(defun add-cache-entry (node key value)
  (setf (slot-value node (intern (format nil "~A-cache" key) :keyword))
        value))

(add-cache-entry v "results" '(1 2 3))
→ (slot-value v :results-cache) → (1 2 3)
```

**Pattern:** Properties added dynamically; schema-free

---

## Performance Characteristics

### Big O Summary

| Operation | Time | Space | Notes |
|-----------|------|-------|-------|
| Create node instance | O(1) | O(1) | Allocate slots |
| Read meta-slot | O(1) | O(1) | Direct slot access |
| Read property | O(n) | O(1) | Linear alist search (n = #properties) |
| Write meta-slot | O(1) | O(1) | Direct slot access |
| Write property | O(n + I/O) | O(1) | Alist search + persistence |
| Unbind property | O(n) | O(1) | Delete + reassign alist |
| Class creation | O(m) | O(m) | m = #slots in class |

**Critical Bottleneck: O(n) Property Access**

```
Properties with O(n) performance:
- Reading: (slot-value node :prop) → O(n)
- Writing: (setf (slot-value node :prop) val) → O(n + I/O)
- Unbinding: (slot-makunbound node :prop) → O(n)

Example: 1000 properties in alist
- Read 1000th property: ~1000 comparisons
- Write 1000th property: ~1000 comparisons + I/O
- Repeated access becomes slow

Better alternative: Use hash-table for properties
- Read: O(1)
- Write: O(1) + I/O
- Unbind: O(1)
```

---

## Constraints & Safety

### Safe Operations

| Operation | Safe? | Reason | Caveats |
|-----------|-------|--------|---------|
| Read meta-slot | ✅ | Direct slot access | Requires initialized instance |
| Read property (SBCL) | ⚠️ | MOP protocol ok, but nil check missing | Crashes if data=nil |
| Write meta-slot | ✅ | Direct slot access | Requires initialized instance |
| Create instance | ✅ | Normal CLOS instantiation | Depends on save-node being defined |

### Unsafe Operations

| Operation | Safe? | Reason | Risk |
|-----------|-------|--------|------|
| Read property | ❌ | No nil check on data | TYPE-ERROR if data=nil |
| Write property | ❌ | Global state access without locking | Race conditions |
| Unbind property | ❌ | No nil check; triggers persistence | TYPE-ERROR; unexpected I/O |
| Concurrent access | ❌ | No synchronization | Data corruption |

### Recommended Patterns

| Situation | Do This | Avoid This |
|-----------|---------|-----------|
| New property | (setf (slot-value n :prop) val) | Manual alist manipulation |
| Read with default | (or (slot-value n :prop) default) | (slot-value n :prop) without checking |
| Batch updates | Use with-transaction | Individual setf calls |
| Large property set | Redesign (use hash-table) | Adding 1000+ properties |

---

## Edge Cases & Gotchas

### Gotcha 1: nil Data Crashes Getter/Setter

**Problem:**
```lisp
(setf (slot-value instance :prop) value)
where (data instance) = nil
→ (assoc :prop nil) → nil
→ (setf (cdr nil) value)
→ signal: TYPE-ERROR
```

**Mitigation:** Initialize %data to empty alist

```lisp
(defclass node ()
  ((%data :accessor data :initarg :%data :initform nil)))
  
; Should be:
(defclass node ()
  ((%data :accessor data :initarg :%data :initform '())))
```

---

### Gotcha 2: Missing Key in Alist Returns nil (then crashes on setf)

**Problem:**
```lisp
(slot-value instance :nonexistent)
→ (assoc :nonexistent alist) → nil
→ (cdr nil) → TYPE-ERROR
```

**Better design:** Initialize key with nil value

```lisp
(setf (slot-value instance :prop) nil)  ;; Creates entry
(slot-value instance :prop) → nil       ;; Works (entry exists)
```

---

### Gotcha 3: O(n) Property Access Becomes Unacceptable at Scale

**Problem:**
```lisp
(loop for i from 1 to 1000
      do (setf (slot-value node (intern (format nil "p~D" i))) i)))

; Reading property #1000:
(slot-value node :p1000)
→ O(1000) alist search + O(I/O) persistence
→ Slow
```

**Mitigation:** Use hash-table if >100 properties expected

---

### Gotcha 4: Thread-Unsafe Global State

**Problem:**
```lisp
Thread A: (setf (slot-value n1 :prop) 1)
Thread B: (setf (slot-value n2 :prop) 2)

; Both access *current-transaction* without locking
; Both may corrupt transaction queue
```

**Mitigation:** Wrap in with-lock or redesign

---

### Gotcha 5: compute-effective-slot-definition is Stub (Intent Unclear)

**Problem:**
```lisp
(defmethod compute-effective-slot-definition :around ((class graph-class) ...)
  (let ((slot (call-next-method)))
    ;;  ← Empty body
    slot))
```

**Question:** Is this intentionally a no-op, or unfinished implementation?

---

## Integration Context

### Upstream Dependencies

| Dependency | Purpose | Source |
|------------|---------|--------|
| package.lisp | (in-package :graph-db) | Layer 1 |
| globals.lisp | +null-key+ constant | Layer 1 |
| utilities.lisp | MOP utilities (maybe) | Layer 1 |

### Downstream Usage

| Consumer | What imports | Layer |
|----------|-------------|-------|
| graph-class.lisp | graph-class metaclass | Layer 1 |
| node-class.lisp | node base class | Layer 1 |
| Layer 2-7 | slot-value protocol | All |

### Undefined Imports (must be provided by other files)

| Name | Expected from | Type | Used in |
|------|---------------|------|---------|
| save-node | ? | Function | Line 53 (setter) |
| *current-transaction* | globals.lisp? | Variable | Line 51 (setter) |
| txn-update-queue | ? | Function | Line 52 (setter) |
| +null-key+ | globals.lisp | Constant | Line 64 (node initform) |
| sb-mop:slot-definition-name | SBCL | Function | Lines 39, 46, 56 |

---

## When to Use What

### Decision Table: Which Access Method?

| Need | Method | Why |
|------|--------|-----|
| Read meta-slot (id, type, etc.) | Direct accessor or (slot-value n :id) | O(1); standard CLOS |
| Write meta-slot | (setf (slot-value n :id) ...) | O(1) |
| Read property | (slot-value n :name) | Transparent; O(n) but ok for small data |
| Write property | (setf (slot-value n :name) ...) | Transparent; triggers persistence |
| Add many properties | Redesign (hash-table) | O(n) becomes unacceptable |
| Batch updates | with-transaction macro | Efficient persistence |

---

## Summary

| Aspect | Status | Details |
|--------|--------|---------|
| **Completeness** | 🟡 70% | Core present; some stubs |
| **Consistency** | 🟡 50% | Slot protocol mostly consistent; errors missing |
| **Correctness** | 🔴 0% | Undefined refs; runtime errors likely |
| **Error handling** | 🔴 0% | No nil checks; no validation |
| **Performance** | 🟡 40% | O(n) for properties; acceptable at small scale |
| **Thread-safety** | 🔴 0% | No synchronization; data corruption likely |
| **Portability** | 🔴 0% | SBCL-specific (sb-mop:) |
| **Documentation** | 🔴 0% | No docstrings |

---

### Key Insights

1. **Dual-slot architecture is sound** — Separating metadata from properties is good design
2. **MOP implementation is complete** — All required methods defined
3. **Stubs present** — compute-effective-slot-definition appears unfinished
4. **Error handling missing** — No nil checks; will crash on edge cases
5. **Thread-safety ignored** — Global state without locking
6. **Performance acceptable at small scale** — O(n) alist ok for <100 properties
7. **Portability blocked** — SBCL-specific code
8. **Undefined dependencies** — save-node, *current-transaction* must come from elsewhere

---

### Critical Decisions Before Phase 3

**MUST fix:**

1. ☐ **Resolve undefined references** — save-node, *current-transaction*, txn-update-queue
2. ☐ **Add nil checks** — slot access methods crash on nil data
3. ☐ **Clarify compute-effective-slot-definition** — Stub or unfinished?
4. ☐ **Fix SBCL-specific code** — Use portable (sb-mop:) or document SBCL-only

**SHOULD fix:**

5. ☐ **Add thread-safety** — Locking for global state access
6. ☐ **Optimize properties** — Consider hash-table for large datasets
7. ☐ **Document slot categories** — Clearly mark meta-slots vs properties

**NICE to have:**

8. ☐ **Add docstrings** — Explain MOP protocol
9. ☐ **Add type validation** — Enforce types for properties
10. ☐ **Complete slot definitions** — Implement custom behavior in effective-slot

---

**Status:** Specification complete.  
**Blocking issues:** 7 (from Nivel 1)  
**Design quality:** Good (pattern sound; implementation incomplete)  
**Ready for Nivel 3 (Docstrings)?** 🟡 PARTIAL — Can proceed but note blocking issues  
**Ready for higher layers?** 🔴 NO (dependencies undefined)  
**Next action:** Resolve undefined references + add error handling.

