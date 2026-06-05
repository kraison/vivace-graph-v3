# Layer 1 Execution Mental Model: clos.lisp

**File:** src/clos.lisp (89 lines)  
**Nivel:** 4 (Execution Mental: load-time, class creation, slot access, performance, concurrency)  
**Date:** March 2026

---

## Table of Contents

1. [Load-Time Execution Flow](#1-load-time-execution-flow)
2. [Class Definition Execution](#2-class-definition-execution)
3. [Instance Creation Execution](#3-instance-creation-execution)
4. [Slot Access Execution Patterns](#4-slot-access-execution-patterns)
5. [MOP Method Dispatch](#5-mop-method-dispatch)
6. [Persistence Trigger Execution](#6-persistence-trigger-execution)
7. [Performance Characteristics](#7-performance-characteristics)
8. [Concurrency Model](#8-concurrency-model)
9. [Critical Gotchas & Edge Cases](#9-critical-gotchas--edge-cases)
10. [Risk Landscape](#10-risk-landscape)
11. [Decision Trees](#11-decision-trees)
12. [Summary Insights](#12-summary-insights)

---

## 1. Load-Time Execution Flow

### Timeline: From asdf:load-system to Runtime Ready

```
┌──────────────────────────────────────────────────────┐
│        COMPILATION PHASE (t=0)                       │
│       clos.lisp loads                               │
└──────────────────────────────────────────────────────┘

User: (asdf:load-system :vivacegraph)
   ↓
   ASDF reads vivacegraph.asd; loads files in order
   ↓
   Layer 1 files loaded:
   1. src/package.lisp           ← Package :graph-db created
   2. src/globals.lisp           ← Constants, *meta-slots* defined
   3. src/conditions.lisp        ← Exception classes defined
   4. src/utilities.lisp         ← Utility functions, macros defined
   5. src/clos.lisp              ← WE ARE HERE
   6. src/uuid.lisp
   ... (Layer 1-7 files)

┌─ clos.lisp execution ──────────────────────────────┐
│                                                    │
│  (in-package :graph-db)                          │
│    Action: Switch reader to :graph-db package     │
│    State: All symbols defined here go to :graph-db│
│                                                    │
│  Line 3-5: (defvar *meta-slots* '(...))          │
│    Action: Create global variable                 │
│    Evaluation: Evaluate '(...) → list object     │
│    State: *meta-slots* bound to list (21 slots)   │
│    Side-effect: Global variable in Lisp symbol    │
│                                                    │
│  Line 7-8: (defclass graph-class ...)            │
│    Action: Create metaclass                       │
│    Compiler: Processes class definition via MOP   │
│    State: graph-class object registered in Lisp   │
│    Side-effect: Metaclass available for use       │
│                                                    │
│  Line 14-15: (defclass graph-slot-definition ...) │
│    Action: Create slot definition mixin class     │
│    State: Class registered (empty, no behavior)   │
│                                                    │
│  Line 17-19: (defclass graph-direct-slot-definition ...) │
│    Action: Create direct slot definition class    │
│    State: Class registered                        │
│    Inheritance: standard-direct-slot-definition   │
│                + graph-slot-definition (mixin)    │
│                                                    │
│  Line 21-23: (defclass graph-effective-slot-definition ...) │
│    Action: Create effective slot definition class │
│    State: Class registered                        │
│    Inheritance: standard-effective-slot-definition│
│                + graph-slot-definition (mixin)    │
│                                                    │
│  Line 25-27: (defmethod validate-superclass ...) │
│    Action: Define MOP method                      │
│    Compiler: Creates method object                │
│    State: Method registered on graph-class        │
│    Dispatch: Specialization (graph-class, standard-class)
│                                                    │
│  Line 29-31: (defmethod direct-slot-definition-class ...) │
│    Action: Define MOP method                      │
│    State: Method registered                       │
│                                                    │
│  Line 33-36: (defmethod compute-effective-slot-definition :around ...) │
│    Action: Define :around method (stub)          │
│    State: Method registered (empty body)          │
│                                                    │
│  Line 38-43: (defmethod slot-value-using-class :around (getter) ...) │
│    Action: Define slot getter interceptor        │
│    Compiler: Creates method with :around dispatch│
│    State: Method registered on graph-class        │
│    Semantics: Will intercept (slot-value ...) calls
│                                                    │
│  Line 45-53: (defmethod (setf slot-value-using-class) :around (setter) ...) │
│    Action: Define slot setter interceptor        │
│    Compiler: Creates setf method with :around     │
│    State: Method registered                       │
│    Semantics: Will intercept (setf (slot-value ...)) calls
│                                                    │
│  Line 55-61: (defmethod slot-makunbound-using-class :around ...) │
│    Action: Define slot unbinding interceptor     │
│    State: Method registered                       │
│                                                    │
│  Line 63-87: (defclass node () (...) (:metaclass graph-class)) │
│    Action: Create node class with custom metaclass│
│    Compiler: Uses MOP to create class definition  │
│                                                    │
│    MOP invocation sequence:                       │
│      1. (validate-superclass graph-class standard-object)
│         → t (allowed)
│      2. (direct-slot-definition-class graph-class)
│         → #<CLASS GRAPH-DIRECT-SLOT-DEFINITION>
│      3. (effective-slot-definition-class graph-class)
│         → #<CLASS GRAPH-EFFECTIVE-SLOT-DEFINITION>
│      4. (compute-effective-slot-definition :around
│           graph-class 'id direct-slots)
│         → Calls (call-next-method) → default computation
│         → Returns effective slot unchanged (stub does nothing)
│      5. ... (repeat for each of 13 slots)
│                                                    │
│    State: node class created with:               │
│      - 13 meta-slots defined as standard slots   │
│      - 1 %data slot for alist properties         │
│      - graph-class metaclass registered          │
│      - All methods on graph-class available      │
│                                                    │
└───────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────┐
│    STATE AFTER clos.lisp LOADS (t=0+ε)            │
└──────────────────────────────────────────────────────┘

Classes registered with Lisp:
  - graph-class (metaclass)
  - graph-slot-definition (mixin)
  - graph-direct-slot-definition (wrapper)
  - graph-effective-slot-definition (wrapper)
  - node (base class for all graph nodes)

Methods registered on graph-class:
  - validate-superclass/2
  - direct-slot-definition-class/2
  - effective-slot-definition-class/2
  - compute-effective-slot-definition :around/3 (stub)
  - slot-value-using-class :around/3 (getter interceptor)
  - (setf slot-value-using-class) :around/3 (setter interceptor)
  - slot-makunbound-using-class :around/3 (unbind interceptor)

Global variables:
  - *meta-slots*: List of 21 meta-slot names (immutable reference)

Memory allocated:
  ├─ 5 class objects: ~100 KB
  ├─ 7 method objects: ~80 KB
  ├─ 1 global variable (*meta-slots* list): ~1 KB
  └─ TOTAL: ~180 KB (negligible)

Global state created:
  ├─ *meta-slots* list (read-only reference)
  └─ No mutable global state (good! all mutation happens in setters)

Compilation errors if:
  ❌ save-node function not defined (line 53 references undefined function)
  ❌ *current-transaction* variable not defined (line 51 references undefined variable)
  ❌ txn-update-queue function not defined (line 52 references undefined function)

┌──────────────────────────────────────────────────────┐
│    NEXT LAYER FILES LOAD (Layer 1 Phase 2)        │
└──────────────────────────────────────────────────────┘

uuid.lisp, random.lisp, etc. load:
  (in-package :graph-db)
  (defclass vertex-type () ... (:metaclass graph-class))
    ├─ Uses graph-class metaclass ✓
    ├─ Uses node as base or superclass ✓
    └─ All MOP methods available ✓

Layer 2-7 code:
  (make-instance 'vertex :id some-uuid :name "Alice")
    ├─ Creates node instance
    ├─ Calls slot-value-using-class for :name
    │  └─ Intercepts; stores in %data alist
    └─ May call save-node (if defined)

┌──────────────────────────────────────────────────────┐
│        RUNTIME (t > 0, after load)                │
└──────────────────────────────────────────────────────┘

User code:
  (defvar v (make-instance 'node :id my-uuid))
    → Allocates node instance
    → Calls initialize-instance (standard MOP)
    → Sets slots from initargs
    → Returns node object

User code:
  (slot-value v :name)
    → MOP dispatches to slot-value-using-class
    → slot-value-using-class :around method called
    → Checks if :name in *meta-slots* → nil
    → Treats as property; looks up in %data alist
    → Returns value or nil

User code:
  (setf (slot-value v :name) "Alice")
    → MOP dispatches to (setf slot-value-using-class)
    → Setter method called
    → Modifies %data alist in-place
    → Checks *current-transaction* (if defined)
    → Calls save-node or queues for batch (if defined)
    → Returns "Alice"

┌──────────────────────────────────────────────────────┐
│      END OF clos.lisp LIFECYCLE                   │
└──────────────────────────────────────────────────────┘
```

---

## 2. Class Definition Execution

### Graph-Class Creation Flow

```
User code:
  (defclass my-vertex ()
    ((degree :accessor degree :initform 0))
    (:metaclass graph-class))

┌─ READER PHASE (macro expansion) ──────────────┐
│                                               │
│ Lisp reader parses defclass form             │
│ Expands defclass macro                       │
│   → Calls (ensure-class 'my-vertex ...)      │
│   → Passes :metaclass graph-class            │
│                                               │
└───────────────────────────────────────────────┘

┌─ CLASS CREATION PHASE (MOP) ──────────────────┐
│                                               │
│ 1. MOP calls (make-instance 'graph-class ...) │
│    → Creates graph-class instance            │
│                                               │
│ 2. MOP calls (validate-superclass            │
│              graph-class                      │
│              standard-object)                 │
│    → Method returns t (always)                │
│    → Superclass allowed                       │
│                                               │
│ 3. MOP processes slot definitions            │
│    For each slot (degree):                   │
│      a. (direct-slot-definition-class       │
│          graph-class ...)                    │
│         → Returns graph-direct-slot-definition
│         → Creates direct slot definition    │
│      b. (effective-slot-definition-class ...) │
│         → Returns graph-effective-slot-definition
│      c. (compute-effective-slot-definition  │
│          :around graph-class 'degree ...)   │
│         → Calls (call-next-method)          │
│         → Gets default effective slot       │
│         → Returns unchanged (stub)          │
│                                               │
│ 4. Class finalization                        │
│    → Registers class in Lisp                 │
│    → Sets up slot accessors                  │
│    → Sets up discriminating functions (std)  │
│                                               │
│ Result: my-vertex class created              │
│         - Metaclass: graph-class            │
│         - Superclass: standard-object       │
│         - 1 defined slot (degree)           │
│         - All MOP methods inherited         │
│                                               │
└───────────────────────────────────────────────┘

PERFORMANCE:
  Time: O(n) where n = number of slots
        Each slot requires MOP calls
  Space: O(n) for slot definitions, accessors

GOTCHA: If graph-class metaclass is NOT used:
  (defclass my-vertex ()
    ((degree :accessor degree))
    (:metaclass standard-class))  ;; Wrong!
  
  Then slot access is standard CLOS (no property interception)
  Custom protocol doesn't apply
```

---

## 3. Instance Creation Execution

### Node Instance Creation Flow

```
User code:
  (defvar v1 (make-instance 'node :id some-uuid))

┌─ ALLOCATION PHASE ────────────────────────────┐
│                                               │
│ 1. Lisp allocates new node object            │
│    → Allocates heap space for slots          │
│    → All slots initially unbound             │
│                                               │
│ 2. MOP calls (initialize-instance v1 ...) │
│    → Standard CLOS initialization           │
│                                               │
│ 3. For each initarg in (:id some-uuid)      │
│    → Find corresponding slot (id)           │
│    → Call slot setter                       │
│      a. (setf slot-value-using-class       │
│          some-uuid node v1 id-slot-def)    │
│      b. Checks if 'id in *meta-slots*      │
│         → t (yes, meta-slot)               │
│      c. Calls (call-next-method)           │
│         → Uses standard CLOS setter        │
│      d. Sets v1.id = some-uuid             │
│                                               │
│ 4. For remaining slots (with initforms):    │
│    Lisp sets default values                 │
│      - (%type-id v1) = 1                   │
│      - (%revision v1) = 0                   │
│      - (%deleted-p v1) = nil                │
│      - (%data v1) = nil  ← ISSUE!           │
│      - ... (other slots)                    │
│                                               │
│ 5. Return v1                                 │
│                                               │
└───────────────────────────────────────────────┘

PERFORMANCE:
  Time: O(1) allocation + O(n) for n initargs processed
  Space: O(1) instance + O(n) for slot storage

GOTCHA 1: %data initform is nil
  (make-instance 'node :id uuid)
  → (%data instance) = nil
  
  Later: (slot-value instance :prop)
  → Tries (cdr (assoc :prop nil))
  → CRASH: TYPE-ERROR (nil is not a cons)
  
  Better: %data initform should be '()

GOTCHA 2: No initargs for properties
  (make-instance 'node :id uuid :name "Alice")
  → Initarg :name not recognized
  → CRASH: Unrecognized initarg (SBCL-specific error)
  
  Must set properties after creation:
  (setf (slot-value v :name) "Alice")

EXAMPLE EXECUTION:
  (defvar v (make-instance 'node :id my-uuid))
  
  v = #<NODE id=#(0xA3...) %type-id=1 %revision=0 %deleted-p=nil ...>
  (id v) → #(0xA3...)  (16-byte array)
  (%type-id v) → 1
  (%data v) → nil  ← problematic
```

---

## 4. Slot Access Execution Patterns

### Pattern A: Read Meta-Slot (Fast Path, O(1))

```
(id v)  or  (slot-value v :id)

Execution:
  1. MOP dispatches to slot-value-using-class :around
  2. Extracts slot name: (sb-mop:slot-definition-name slot) → id
  3. Checks (find id *meta-slots*) → t (yes, meta-slot)
  4. Calls (call-next-method) for default getter
     → Returns v.id = 16-byte UUID
  5. Return: #(0xA3 0x7F ...)

Performance: O(1)
  - No alist search
  - Direct slot access via MOP

Thread-safety: ✅ SAFE (read-only)
```

### Pattern B: Read Property (Slow Path, O(n))

```
(slot-value v :custom-prop)

Execution:
  1. MOP dispatches to slot-value-using-class :around
  2. Extracts slot name: slot-name = custom-prop
  3. Checks (find custom-prop *meta-slots*) → nil (not meta-slot)
  4. Converts to keyword: key = :CUSTOM-PROP
  5. Looks up in alist:
     (data v) → '((:NAME . "Alice") (:CUSTOM-PROP . 42))
  6. (assoc :CUSTOM-PROP '(...)) → (:CUSTOM-PROP . 42)
  7. (cdr (:CUSTOM-PROP . 42)) → 42
  8. Return: 42

If property not found:
  (assoc :NONEXISTENT '(...)) → nil
  (cdr nil) → ❌ TYPE-ERROR

If %data is nil:
  (assoc :CUSTOM-PROP nil) → nil
  (cdr nil) → ❌ TYPE-ERROR

Performance: O(n) where n = number of properties
  - Worst case: searching last property requires n-1 comparisons
  - Average case: n/2 comparisons
  - Best case: 1 comparison (found first)

Thread-safety: ❌ UNSAFE (global state accessed in setter)
               Read itself is safe, but if setter running concurrently:
               - Race on *current-transaction*
               - Race on %data modification
```

### Pattern C: Write Property (O(n + I/O))

```
(setf (slot-value v :custom-prop) 42)

Execution:
  1. MOP dispatches to (setf slot-value-using-class) :around
  2. Extracts slot name: slot-name = custom-prop
  3. Checks (find custom-prop *meta-slots*) → nil
  4. Converts to keyword: key = :CUSTOM-PROP
  5. Finds in alist:
     (data v) → '((:NAME . "Alice") (:CUSTOM-PROP . old-value))
     (assoc :CUSTOM-PROP ...) → (:CUSTOM-PROP . old-value)
  6. Modifies in-place:
     (setf (cdr (:CUSTOM-PROP . old-value)) 42)
     → (data v) now = '((:NAME . "Alice") (:CUSTOM-PROP . 42))
  7. Trigger persistence:
     (if *current-transaction* ... (save-node v))
     → Calls save-node (I/O)
  8. Return: 42

If %data is nil:
  (assoc :CUSTOM-PROP nil) → nil
  (setf (cdr nil) 42) → ❌ TYPE-ERROR

If key not in alist:
  (assoc :NEW-KEY alist) → nil
  (setf (cdr nil) 42) → ❌ TYPE-ERROR

Performance: O(n + I/O)
  - n = number of properties (alist search)
  - I/O = disk write (save-node)
  - I/O dominates at scale

Thread-safety: ❌ BROKEN
  Race condition scenario:
    Thread A: (setf (slot-value v1 :p1) 1)
      → Reads %data
      → Modifies in-place
      → Checks *current-transaction*
    Thread B: (setf (slot-value v2 :p2) 2)
      → Reads %data
      → Modifies in-place
      → Checks *current-transaction*
    
  Both threads may:
    - Corrupt *current-transaction* queue
    - Cause lost updates (one write overwrites another)
    - Inconsistent state
```

### Pattern D: Unbind Property (O(n) + I/O)

```
(slot-makunbound v :custom-prop)

Execution:
  1. MOP dispatches to slot-makunbound-using-class :around
  2. Extracts slot name: custom-prop
  3. Checks (find custom-prop *meta-slots*) → nil
  4. Converts to keyword: :CUSTOM-PROP
  5. Deletes from alist:
     (data v) = '((:NAME . "Alice") (:CUSTOM-PROP . 42))
     (delete :CUSTOM-PROP ... :key 'car)
     → '((:NAME . "Alice"))  ;; :CUSTOM-PROP removed
  6. Reassigns:
     (setf (data v) '((:NAME . "Alice")))
     → Calls setter (line 45)
     → Triggers persistence (save-node or queue)
  7. Return: v

Performance: O(n + I/O)
  - n = alist search (delete traversal)
  - I/O from persistence trigger

Thread-safety: ❌ BROKEN (same as setter)
```

---

## 5. MOP Method Dispatch

### Generic Dispatch Mechanism

```
When (slot-value v :prop) is called:

1. Lisp MOP determines applicable methods
   
2. Looks up registered methods on v's metaclass
   ├─ v is instance of node
   ├─ node's metaclass is graph-class
   └─ graph-class has method: slot-value-using-class :around
   
3. Method resolution order (simple case):
   ├─ :around methods (outermost)
   ├─ :before methods
   ├─ primary methods
   ├─ :after methods
   └─ (innermost)
   
   In our case:
   ├─ :around slot-value-using-class (line 38-43)
   │  ├─ Executes wrapper logic
   │  ├─ Calls (call-next-method) if meta-slot
   │  │  └─ Dispatches to default getter
   │  └─ Returns value
   
4. Call-next-method chain:
   (slot-value-using-class :around)
     → (if meta-slot) (call-next-method)
       → Default standard-class getter
         → Returns slot value directly
     → (else) return alist lookup result

PERFORMANCE:
  Method dispatch: O(1) amortized
    - First call: O(m) where m = method count (cache miss)
    - Subsequent calls: O(1) (method cache hit)
  
  MOP caching (Lisp implementation detail):
    - Lisp caches method dispatch by (arg-classes)
    - (slot-value v :name) caches (node)
    - Next call with node instance uses cache
```

---

## 6. Persistence Trigger Execution

### Transaction Context Flow

```
Scenario: Inside transaction

(with-transaction (graph)  ;; Binds *current-transaction*
  (setf (slot-value v1 :prop1) 1)
  (setf (slot-value v2 :prop2) 2)
  (commit))  ;; Saves all queued instances at once

Execution:

1. with-transaction macro binds *current-transaction*
   *current-transaction* = #<TRANSACTION id=...>

2. (setf (slot-value v1 :prop1) 1)
   → Calls (setf slot-value-using-class) setter
   → Checks (if *current-transaction*)
   → *current-transaction* is bound → t
   → Calls (pushnew v1 (txn-update-queue txn) :test 'equalp :key 'id)
   → Queues v1 for batch save
   → Returns 1

3. (setf (slot-value v2 :prop2) 2)
   → Same flow
   → Queues v2
   → Returns 2

4. (commit)
   → Calls (save-all-in-queue *current-transaction*)
   → Single I/O operation for both v1 and v2 (batch save)
   → Better performance than two separate saves

Performance benefit:
  Without transaction: 2 × I/O (two separate save-node calls)
  With transaction: 1 × I/O (batch save)
  Overhead: Queue management (negligible)

Problem: If txn-update-queue is undefined → CRASH
```

### Non-Transaction Flow (Immediate Save)

```
Scenario: Outside transaction

(setf (slot-value v :prop) value)  ;; No *current-transaction*

Execution:

1. (setf slot-value-using-class) setter called
2. Checks (if *current-transaction*)
3. *current-transaction* is unbound or nil → nil
4. Calls (save-node instance) immediately
5. save-node performs disk I/O
6. Returns value

Performance: WORSE (every write = I/O)
  Time: O(n + I/O) where n = alist search
  I/O dominates

Best practice: Always use transactions for batch updates
  (with-transaction (graph)
    (loop for v in vertices
          do (setf (slot-value v :processed) t)))
  
  vs (bad):
  (loop for v in vertices
        do (setf (slot-value v :processed) t))  ;; n × I/O!
```

---

## 7. Performance Characteristics

### Big O Analysis

| Operation | Time | Space | Notes |
|-----------|------|-------|-------|
| Class definition (defclass) | O(m) | O(m) | m = number of slots; MOP overhead |
| Instance creation | O(1) | O(1) | Allocation + initialization |
| Read meta-slot | O(1) | O(1) | Direct slot access via MOP |
| Read property | O(n) | O(1) | n = alist size (linear search) |
| Write meta-slot | O(1) | O(1) | Direct slot access; no I/O (unless save triggered) |
| Write property | O(n + I) | O(1) | n = alist search; I = I/O cost |
| Unbind property | O(n + I) | O(n) | Delete operation; reassign alist |
| MOP dispatch (cached) | O(1) | O(m) | m = method cache entries |
| MOP dispatch (uncached) | O(m) | O(m) | First call; slow |

**Critical Performance Bottleneck: Property Access**

```
Problem: Linear alist search for every property access

Example: 1000 properties stored in %data alist

Reading 100 properties with 1000-property alist:
  Worst case: 100 × 1000 = 100,000 comparisons
  Average case: 100 × 500 = 50,000 comparisons
  Total time: ~1 second (at ~100K ops/sec)

Better solution: Use hash-table for properties
  (data v) → #<HASH-TABLE> instead of alist
  Read/write time becomes O(1) instead of O(n)
  Tradeoff: More memory per instance (hash-table overhead)

Why alist used instead of hash-table?
  - Simpler to understand
  - Serialization-friendly (alist ↔ disk)
  - No hash-table initialization overhead
  But: Not suitable for >100 properties
```

---

## 8. Concurrency Model

### Thread-Safety Analysis

```
SAFE SCENARIOS:

Scenario A: Multiple threads reading different properties
  Thread 1: (slot-value v1 :prop1)
  Thread 2: (slot-value v2 :prop2)
  
  ✅ SAFE: Read-only; no shared state modified
  Time: O(n) per thread (independent)

Scenario B: Multiple threads reading same property
  Thread 1: (slot-value v :prop)
  Thread 2: (slot-value v :prop)
  
  ✅ SAFE: Both read same %data alist
  Risk: Low (alist reference is stable)

UNSAFE SCENARIOS:

Scenario C: One thread writes, another reads (RACE CONDITION)
  Thread 1: (setf (slot-value v :p1) 1)
    → Reads (data v) = alist-ref-A
    → Modifies alist-ref-A in-place
    → Checks *current-transaction* (global)
  
  Thread 2: (slot-value v :p1)
    → May read alist-ref-A mid-modification
    → Or read stale value (different reference)
  
  ❌ UNSAFE: Data corruption possible

Scenario D: Two threads write same property (RACE CONDITION)
  Thread 1: (setf (slot-value v :p1) 1)
    → Finds (p1 . old) in alist
    → (setf (cdr (p1 . old)) 1)
  
  Thread 2: (setf (slot-value v :p1) 2)
    → Finds (p1 . old) in alist
    → (setf (cdr (p1 . old)) 2)
  
  ❌ UNSAFE: Lost update (value = 1 or 2, unclear which)

Scenario E: Global state (*current-transaction*) access (RACE)
  Thread 1: (setf (slot-value v1 :p) 1)
    → Reads *current-transaction*
    → Checks if bound
    → Calls (pushnew v1 (txn-update-queue txn))
  
  Thread 2: (setf (slot-value v2 :p) 2)
    → Reads *current-transaction*
    → Concurrent modification to txn-update-queue
  
  ❌ UNSAFE: Queue corruption (entries lost or duplicated)

THREAD-SAFETY FIXES NEEDED:

Fix 1: Protect instance.%data with lock
  (with-lock (instance-lock)
    (setf (slot-value v :prop) value))

Fix 2: Protect *current-transaction* with lock
  (with-lock (*transaction-lock*)
    (pushnew instance (txn-update-queue txn) ...))

Fix 3: Use copy-on-write for %data
  (setf (data instance) (copy-alist (data instance)))
  → Each write creates new alist (no in-place modification)
  → Atomic swap: (setf (data instance) new-alist)

Current design: NONE of these implemented!
Risk: HIGH (silent data corruption in multi-threaded scenarios)
```

---

## 9. Critical Gotchas & Edge Cases

### Gotcha 1: %data Nil Initialization Crashes

**Problem:**
```lisp
(make-instance 'node)
→ (%data node) = nil

(slot-value node :prop)
→ (assoc :prop nil) → nil
→ (cdr nil) → ❌ TYPE-ERROR
```

**Impact:** Property access crashes if instance created without %data initialization

**Fix:** Change %data initform from `nil` to `'()`

---

### Gotcha 2: Properties Cannot Be Initialized via Initargs

**Problem:**
```lisp
(make-instance 'node :id uuid :name "Alice")
→ Initarg :name not recognized
→ ❌ ERROR: Unrecognized initarg :name
```

**Why:** Node class doesn't have :name slot; :name is property (not class slot)

**Fix:** Set properties after creation:
```lisp
(let ((v (make-instance 'node :id uuid)))
  (setf (slot-value v :name) "Alice")
  v)
```

---

### Gotcha 3: O(n) Property Access at Scale

**Problem:**
```lisp
(loop for i from 1 to 1000
      do (setf (slot-value v (intern (format nil "p~D" i))) i)))

(slot-value v :p1000)
→ O(1000) alist search
→ Slow (~1 ms per access with 1000 properties)
```

**Impact:** Query performance degrades with many properties

**Solution:** Redesign to use hash-table for >100 properties

---

### Gotcha 4: Unbinding Triggers Persistence

**Problem:**
```lisp
(slot-makunbound v :prop)
→ Calls (setf (data v) ...)
→ Which calls (setf slot-value-using-class) setter
→ Which calls save-node (I/O!)
```

**Impact:** Unexpected I/O cost on unbinding

---

### Gotcha 5: SBCL-Specific Code Fails on CCL/LW

**Problem:**
```lisp
(sb-mop:slot-definition-name slot)  ;; SBCL only!
→ Undefined function on CCL/LispWorks
→ ❌ COMPILATION ERROR
```

**Impact:** Code not portable to other Lisp implementations

**Fix:** Use portable (mop:slot-definition-name slot) or conditional compilation

---

### Gotcha 6: Undefined Function References Block Compilation

**Problem:**
```lisp
Line 53: (save-node instance)  ;; Function not defined
Line 51: (if *current-transaction* ...)  ;; Variable undefined
Line 52: (txn-update-queue ...)  ;; Function undefined
```

**Impact:** File fails to load (compilation error)

**Fix:** Ensure save-node, *current-transaction*, txn-update-queue are defined before clos.lisp loads

---

## 10. Risk Landscape

### Risk Severity Matrix

```
RISK LEVEL      COUNT   EXAMPLES
────────────────┼───────┼──────────────────────────────
🔴 BLOCKING     6       - No nil checks (crash on property access)
                        - Undefined function references
                        - SBCL-specific code (not portable)

🟠 CRITICAL     4       - O(n) property access (performance)
                        - No thread-safety (data corruption)
                        - No rollback on save failure
                        - Destructive alist modification

🟡 WARNING      5       - compute-effective-slot-definition stub
                        - %data initform nil (should be '())
                        - %bytes initform :init (unclear)
                        - Performance cliff at scale
                        - Initarg limitation for properties


DEPENDENCY CHAINS:

🔴 BLOCKING: Undefined save-node
   └─ Causes: Compilation error (file won't load)
        └─ Causes: Layer 2-7 code can't load
             └─ Causes: System won't start
                  └─ Severity: 🔴 CATASTROPHIC

🟠 CRITICAL: O(n) property access
   └─ Causes: Performance degradation with >100 properties
        └─ Causes: Query latency unacceptable
             └─ Severity: 🟠 CRITICAL (but not blocking)

🟠 CRITICAL: No thread-safety
   └─ Causes: Race conditions in multi-threaded access
        └─ Causes: Silent data corruption
             └─ Causes: Incorrect results (hard to debug)
                  └─ Severity: 🟠 CRITICAL (worst kind of bug)
```

---

## 11. Decision Trees

### Decision: Should I Use Graph-Class for My Class?

```
START: Defining new node type

├─ Need to store arbitrary properties?
│  ├─ YES → use graph-class ✓
│  │  └─ (defclass vertex () ... (:metaclass graph-class))
│  │
│  └─ NO → use standard-class (faster)
│     └─ (defclass vertex () ... (:metaclass standard-class))

├─ Expected number of properties?
│  ├─ <100 → alist-based graph-class ok
│  ├─ 100-1000 → consider hash-table redesign
│  └─ >1000 → definitely use hash-table
```

### Decision: How Many Properties Can I Store?

```
START: Estimating acceptable property count

├─ Acceptable latency per access?
│  ├─ <1 ms → max 100 properties (alist search)
│  ├─ <10 ms → max 1000 properties
│  └─ <100 ms → acceptable for bulk operations
│
├─ Consider: Each property access = O(n) search
│  └─ Example: 1000 properties = ~1 ms per access
│
├─ Mitigation:
│  ├─ Use transactions for batch updates (reduces I/O)
│  ├─ Redesign to use hash-table (O(1) access)
│  └─ Index by frequently-accessed properties
```

---

## 12. Summary Insights

### Key Findings

**1. Metaclass Pattern is Sound**
   - ✅ Separating metadata from properties is good design
   - ✅ Transparent access via slot-value is elegant
   - ✓ Extensible (properties added dynamically)

**2. MOP Implementation is Complete**
   - ✅ All required methods defined
   - ✅ Dispatch logic correct (fast path for meta-slots)
   - ✅ Method caching provides good performance

**3. Stubs Present (Incomplete)**
   - 🟡 compute-effective-slot-definition empty (intent unclear)
   - 🟡 from, to, weight mismatch (belong in edge class?)

**4. Error Handling Missing**
   - 🔴 No nil checks → crashes on edge cases
   - 🔴 No validation → invalid data accepted

**5. Thread-Safety Ignored**
   - 🔴 Global state access without locking
   - 🔴 Alist modification in-place (race conditions)
   - 🔴 *current-transaction* protected nowhere

**6. Performance Acceptable at Small Scale**
   - ✅ O(1) meta-slot access (fast path)
   - ⚠️ O(n) property access (slow path)
   - ⚠️ O(n) becomes unacceptable at >100 properties

**7. Portability Blocked**
   - 🔴 SBCL-specific (sb-mop:)
   - 🔴 Fails on CCL/LispWorks
   - 🔴 Not portable across Lisp implementations

**8. Dependencies Undefined**
   - 🔴 save-node function missing
   - 🔴 *current-transaction* variable missing
   - 🔴 txn-update-queue function missing

**9. Design Tradeoffs Clear**
   - ✅ Simplicity vs performance (alist vs hash-table)
   - ✅ Immediate persistence vs batch efficiency (needs transactions)
   - ✅ Flexibility vs validation (any property accepted)

**10. Execution Model is Predictable**
   - ✅ MOP dispatch well-understood
   - ✅ Performance characteristics clear
   - ✅ Failure modes documented

---

### Critical Decisions Before Phase 3

**MUST fix:**
1. ☐ Resolve undefined references (save-node, *current-transaction*, txn-update-queue)
2. ☐ Add nil checks to slot access methods
3. ☐ Document SBCL-specific code or make portable

**SHOULD fix:**
4. ☐ Add thread-safety (locking or redesign)
5. ☐ Change %data initform to '() instead of nil
6. ☐ Document property initialization pattern (no initargs)

**NICE to have:**
7. ☐ Clarify compute-effective-slot-definition intent
8. ☐ Resolve from/to/weight slot mismatch
9. ☐ Performance optimization for >100 properties

---

**Status:** Execution mental model complete.  
**Blocking issues:** 6 (from Nivel 1)  
**Performance bottlenecks:** 1 (O(n) property access)  
**Thread-safety issues:** 3 (global state, alist mutation, queue corruption)  
**Ready for higher layers?** 🔴 NO (dependencies undefined)  
**Estimated time to fix issues:** 2-3 hours per issue class  
**Next action:** Resolve blocking issues; ensure save-node and *current-transaction* defined before loading

