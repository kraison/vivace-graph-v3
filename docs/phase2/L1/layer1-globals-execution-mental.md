# Layer 1 Execution Mental Model: globals.lisp

**File:** src/globals.lisp (134 lines)  
**Nivel:** 4 (Execution Mental: patterns, gotchas, performance, data flow)  
**Date:** March 2026

---

## Table of Contents

1. [Load-Time Execution Flow](#1-load-time-execution-flow)
2. [Global State Interaction Model](#2-global-state-interaction-model)
3. [Configuration Value Flow](#3-configuration-value-flow)
4. [Thread-Safety Execution Patterns](#4-thread-safety-execution-patterns)
5. [Performance Characteristics](#5-performance-characteristics)
6. [Concurrency Model](#6-concurrency-model)
7. [Memory Layout & Access Patterns](#7-memory-layout--access-patterns)
8. [Critical Gotchas & Edge Cases](#8-critical-gotchas--edge-cases)
9. [Risk Landscape](#9-risk-landscape)
10. [Decision Trees](#10-decision-trees)
11. [Summary Insights](#11-summary-insights)

---

## 1. Load-Time Execution Flow

### Timeline: From asdf:load-system to Running Code

```
┌──────────────────────────────────────────────────────────────┐
│                 COMPILATION PHASE (t=0)                      │
└──────────────────────────────────────────────────────────────┘

User: (asdf:load-system :vivacegraph)
   ↓
   ASDF reads vivacegraph.asd
   ↓
   Compiles files in order:
   1. src/package.lisp
   2. src/globals.lisp ← WE ARE HERE
   3. src/clos.lisp
   ... (Layer 1-7 files)

┌─ globals.lisp execution ─────────────────────────────────────┐
│                                                               │
│  Line 1: (in-package :graph-db)                             │
│    Action: Switch reader context to :graph-db package        │
│    State: All symbols now interned in :graph-db              │
│                                                               │
│  Line 3: (defvar *cache-enabled* t)                         │
│    Action: Create slot in :graph-db for *cache-enabled*     │
│    Effect: Variable now exists, bound to t                  │
│    State: *cache-enabled* = t (globally)                    │
│                                                               │
│  Line 5: (alexandria:define-constant +db-version+ 1)        │
│    Action: Create immutable constant                        │
│    Effect: +db-version+ now inlined by compiler             │
│    Attempt to modify: COMPILATION ERROR                     │
│                                                               │
│  Lines 7-11: File name constants                            │
│    Action: Define 4 string constants                        │
│    State: +main-table-file+, +meta-file+, +data-file+      │
│                                                               │
│  Line 12: (defvar *schema-node-metadata* (make-hash-table...))
│    Action: Create hash-table object                         │
│    Effect: Empty hash-table allocated in heap               │
│    ⚠️ WARNING: NOT thread-safe (no :synchronized flag)      │
│    State: *schema-node-metadata* = {} (empty)               │
│                                                               │
│  Line 13: +max-node-types+ = 65536                          │
│    Action: Define type limit constant                       │
│                                                               │
│  Lines 15-30: Storage format constants                      │
│    Action: Define magic bytes, sizes, extents               │
│    Examples:                                                │
│      +storage-version+ = #x01                               │
│      +data-magic-byte+ = #x17                               │
│      +key-bytes+ = 16                                        │
│      +bucket-size+ = 24                                      │
│      +data-extent-size+ = 104857600 (100 MB)               │
│    State: All constants defined, ready for Layer 2-7       │
│                                                               │
│  Lines 32-36: UUID namespace computation (CRITICAL)         │
│    Action: Load-time UUID string → byte-array conversion   │
│      *vertex-namespace* = uuid-to-byte-array("2140DCE1...") │
│      *edge-namespace* = uuid-to-byte-array("0392C7B5...")  │
│    Effect: Two 16-byte arrays created, stored in variables │
│    Timing: Takes ~1 ms per UUID (uuid library overhead)     │
│    Risk: ⚠️ Hardcoded UUIDs → if changed, ID generation breaks
│    State: Namespaces now immutable (defvar never reassigned) │
│                                                               │
│  Lines 38-63: Index constants                               │
│    Action: Define VE-index and VEV-index sentinel values    │
│    Sentinels: :gmin, :gmax, :gagg (keywords)               │
│    Byte-array sentinels: +null-ve-key+, +max-ve-key+, etc. │
│    State: All index constants defined                       │
│                                                               │
│  Lines 65-101: Type serialization byte codes (35 constants) │
│    Action: Define 35 type codes (0-30, 100-101)            │
│    Each constant maps Lisp type → byte code                 │
│    Example:                                                 │
│      +positive-integer+ = 2                                 │
│      +string+ = 5                                           │
│      +vertex+ = 18                                          │
│    State: Type mapping now available for Layer 4 serializer │
│    Risk: ⚠️ Not versioned; if code adds new type, breaks old data
│                                                               │
│  Lines 102-113: Prolog state initialization                 │
│    Action: Create/initialize Prolog state variables         │
│      *occurs-check* = t (parameter)                         │
│      *trail* = array(200, :fill-pointer 0) (adjustable)    │
│      *var-counter* = 0 (counter, non-atomic!)              │
│      *functor* = nil (context)                              │
│      *select-list* = nil (accumulator)                      │
│      *cont* = nil (continuation)                            │
│    Side-effects:                                             │
│      - *trail* allocated: 200 * element-size = ~1.6 KB      │
│      - No thread-local variants created                     │
│    ⚠️ WARNING: All global, will interfere if concurrent Prolog
│                                                               │
│  Lines 115-128: Prolog functor registries (Lisp-specific)   │
│    Conditional compilation based on Lisp:                  │
│                                                               │
│    #+sbcl                                                   │
│      *prolog-global-functors* = make-hash-table(:synchronized t)
│      *user-functors* = make-hash-table(:synchronized t)    │
│      Action: SBCL allocates thread-safe hash-tables        │
│      State: Two empty thread-safe hash-tables created       │
│                                                               │
│    #+ccl                                                    │
│      *prolog-global-functors* = make-hash-table(:shared t) │
│      *user-functors* = make-hash-table(:shared t)          │
│      Action: CCL allocates shared-memory hash-tables        │
│                                                               │
│    #+lispworks                                              │
│      *prolog-global-functors* = make-hash-table(:single-thread nil)
│      *user-functors* = make-hash-table(:single-thread nil) │
│      Action: LispWorks allocates thread-safe hash-tables    │
│                                                               │
│  Lines 130-134: Prolog debugging constants                  │
│    Action: Define final constants                          │
│      *prolog-trace* = nil (debug flag)                     │
│      +unbound+ = :unbound (sentinel)                        │
│      +no-bindings+ = ((t . t)) (empty environment)         │
│      +fail+ = nil (failure marker)                         │
│    State: Prolog infrastructure complete                    │
│                                                               │
└─────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│          STATE AFTER globals.lisp LOADS (t=0+ε)             │
└──────────────────────────────────────────────────────────────┘

Global state created:
  ├─ Mutable:
  │  ├─ *cache-enabled* = t
  │  ├─ *graph* = nil
  │  ├─ *schema-node-metadata* = {} (empty hash-table)
  │  ├─ *graph-hash* = nil
  │  ├─ *trail* = array(200, fill-pointer=0) (Prolog choice points)
  │  ├─ *var-counter* = 0 (Prolog variable counter)
  │  ├─ *functor* = nil (Prolog context)
  │  ├─ *select-list* = nil (Prolog results)
  │  ├─ *cont* = nil (Prolog continuation)
  │  ├─ *prolog-global-functors* = {} (thread-safe, Lisp-specific)
  │  ├─ *user-functors* = {} (thread-safe, Lisp-specific)
  │  ├─ *vertex-namespace* = byte-array(16) (UUID namespace)
  │  ├─ *edge-namespace* = byte-array(16) (UUID namespace)
  │  └─ *occurs-check* = t (Prolog parameter)
  │
  ├─ Immutable:
  │  ├─ 50+ constants (type codes, magic bytes, sizes, limits)
  │  └─ All inlined by compiler

Memory allocated:
  ├─ *schema-node-metadata* hash-table: ~1 KB
  ├─ *trail* array: ~1.6 KB (200 slots)
  ├─ Two Prolog hash-tables: ~2 KB total
  ├─ Two UUID byte-arrays: 32 bytes
  └─ TOTAL: ~5 KB (negligible)

┌──────────────────────────────────────────────────────────────┐
│         NEXT LAYER FILES LOAD (Layer 1 Phase 2)              │
└──────────────────────────────────────────────────────────────┘

clos.lisp:
  (in-package :graph-db)  ← Package exists! ✓
  (defclass vertex () ...)  ← Defines CLOS class
    → Will later register in *schema-node-metadata*

utilities.lisp:
  (in-package :graph-db)
  (defun make-graph ...)  ← Uses +db-version+, file constants
  (defun make-vertex ...)  ← Uses UUID namespaces, type codes

Layer 2-7:
  All use constants from globals.lisp
  All read/write mutable global variables

┌──────────────────────────────────────────────────────────────┐
│                RUNTIME (t > 0, after load)                   │
└──────────────────────────────────────────────────────────────┘

Application startup:
  User: (let ((g (make-graph "/tmp/mydb")))
          (let ((*graph* g))
            (with-transaction
              (make-vertex "Person" ...))))
  
  Step 1: make-graph reads +db-version+ (constant, inlined)
          reads file constants (+main-table-file+, +data-file+, etc.)
          creates graph instance
  
  Step 2: Binding *graph* = g (thread-local dynamic scope)
  
  Step 3: with-transaction binds *transaction*
  
  Step 4: make-vertex generates UUID using *vertex-namespace*
          looks up "Person" type in *schema-node-metadata*
          creates vertex instance
  
  Step 5: serialize vertex to disk using type codes
          writes byte: +vertex+ = 18
          writes byte: field type code (e.g., +string+ = 5)
          writes field data

Prolog query execution:
  User: (? (ancestor 'alice ?x))
  
  Step 1: Query engine looks up 'ancestor in *prolog-global-functors*
  
  Step 2: Initializes *trail* = array(200) with fill-pointer = 0
  
  Step 3: Increments *var-counter* (0 → 1) for ?x
  
  Step 4: Executes unification using *occurs-check* flag
          if fails, backtracks by reading *trail*
  
  Step 5: Collects solutions in *select-list*
  
  Step 6: Returns solutions

┌──────────────────────────────────────────────────────────────┐
│               PERSISTENCE (end of runtime)                    │
└──────────────────────────────────────────────────────────────┘

On close-graph or shutdown:
  - Dirty data in *schema-node-metadata* flushed to disk
  - *trail*, *select-list* discarded (session-local)
  - *var-counter* reset (next session)
  - Global variables persist in memory until process exit
```

---

## 2. Global State Interaction Model

### Who reads/writes what

```
┌────────────────────────────────────────────────────────────────┐
│         GLOBAL STATE DEPENDENCY & DATA FLOW                    │
└────────────────────────────────────────────────────────────────┘

*graph* (Dynamic variable)
   ↓ (written by)
   ├─ make-graph (creates new graph instance)
   ├─ open-graph (loads existing graph)
   ├─ let-binding (per thread, by user code)
   │
   ↓ (read by)
   ├─ with-transaction (needs current *graph*)
   ├─ make-vertex, make-edge (operate on *graph*)
   ├─ traverse (graph traversal)
   └─ Layer 3+ (all operations check *graph*)

*schema-node-metadata* (Hash-table)
   ↓ (written by)
   ├─ def-vertex (adds type metadata)
   ├─ def-edge (adds type metadata)
   ├─ open-graph (loads schema from disk)
   │
   ↓ (read by)
   ├─ make-vertex (looks up type)
   ├─ instantiate-node-type (gets class definition)
   ├─ serialize operations (checks type constraints)
   └─ Layer 2-7 (type system operations)

*trail* (Array with fill-pointer)
   ↓ (written by)
   ├─ Prolog unification (records bindings)
   │
   ↓ (read by)
   ├─ Prolog backtracking (unwinds bindings)

*var-counter* (Integer)
   ↓ (written by)
   ├─ Prolog variable generation (increment)
   │
   ↓ (read by)
   ├─ Prolog unique naming (?1, ?2, ?3, ...)

*prolog-global-functors* (Hash-table, thread-safe)
   ↓ (written by)
   ├─ def-global-prolog-functor (registers predicate)
   │
   ↓ (read by)
   ├─ Prolog query dispatch (looks up predicate)

*occurs-check* (Boolean parameter)
   ↓ (read by)
   ├─ Prolog unification (controls cycle detection)

*cache-enabled* (Boolean)
   ↓ (read by)
   ├─ Layer 2 lookup operations (cache bypass on nil)
```

### State Transition Diagram

```
┌──────────────┐
│  Application │
│   Startup    │
└──────┬───────┘
       │
       ├─ Create graph (load globals.lisp)
       │  ├─ *cache-enabled* ← t
       │  ├─ *graph* ← nil
       │  ├─ *schema-node-metadata* ← {}
       │  ├─ *trail* ← array(200)
       │  ├─ All constants defined
       │  └─ Prolog registries initialized ← {}
       │
       ↓
┌──────────────┐
│  Graph Open  │
│ (User code)  │
└──────┬───────┘
       │
       ├─ open-graph("/tmp/mydb")
       │  ├─ Reads +main-table-file+ = "main.dat"
       │  ├─ Reads +meta-file+ = "meta.dat"
       │  ├─ Loads schema from meta.dat
       │  └─ Populates *schema-node-metadata* ← {types...}
       │
       ├─ let ((*graph* my-graph-instance))
       │  └─ Binds *graph* thread-locally
       │
       ↓
┌──────────────┐
│  Transaction │
│   Execution  │
└──────┬───────┘
       │
       ├─ with-transaction
       │  ├─ Binds *transaction*
       │  ├─ make-vertex "Person"
       │  │  ├─ Looks up "Person" in *schema-node-metadata*
       │  │  ├─ Uses *vertex-namespace* for UUID generation
       │  │  └─ Serializes using type codes (+vertex+, +string+, etc.)
       │  │
       │  └─ commit
       │     ├─ Flushes *schema-node-metadata* to disk
       │     └─ Writes type codes to data file
       │
       ↓
┌──────────────┐
│   Prolog     │
│   Queries    │
└──────┬───────┘
       │
       ├─ (? (ancestor 'alice ?x))
       │  ├─ Looks up 'ancestor in *prolog-global-functors*
       │  ├─ Initializes *trail* for backtracking
       │  ├─ Increments *var-counter* (0 → 1) for ?x
       │  ├─ Checks *occurs-check* = t
       │  ├─ Executes unification
       │  └─ Collects results in *select-list*
       │
       ↓
┌──────────────┐
│   Shutdown   │
│   Cleanup    │
└──────────────┘
       │
       ├─ close-graph
       │  ├─ Flushes *schema-node-metadata* to disk
       │  └─ Sets *graph* ← nil
       │
       └─ Global variables persist in memory
          (will be GC'd with process exit)
```

---

## 3. Configuration Value Flow

### How constants propagate through system

```
┌─────────────────────────────────────────────────────────────┐
│  globals.lisp (Layer 1)                                     │
│  ───────────────────────────────────────────────────────────│
│  Defines:                                                   │
│    +key-bytes+ = 16                                         │
│    +value-bytes+ = 8                                        │
│    +bucket-size+ = 24                                       │
│    +data-magic-byte+ = #x17                                │
│    +vertex+ = 18                                            │
│    +timestamp+ = 101                                        │
│    etc. (50+ constants)                                     │
└─────┬──────────────────────────────────────────────────────┘
      │
      ├─ COMPILE-TIME INLINING
      │  Compiler reads constants and inlines their values
      │  Result: Constants not referenced at runtime (no lookup cost)
      │
      ↓
┌─────────────────────────────────────────────────────────────┐
│  Layer 2: utilities.lisp                                    │
│  ───────────────────────────────────────────────────────────│
│  Uses:                                                      │
│    (defun make-vertex (graph type-name data)                │
│      (let ((uuid (uuid:make-uuid ...)))                    │
│        (serialize-vertex uuid data)  ← uses +vertex+ = 18
│          ↓                                                   │
│        (write-byte +vertex+ stream)                        │
│          ↓ (inlined: write-byte 18 stream)                 │
│          ↓ Writes 18 to disk                               │
│          ))                                                 │
│                                                             │
│  Compiler has inlined +vertex+ value at this point.        │
└─────┬──────────────────────────────────────────────────────┘
      │
      ├─ RUNTIME SERIALIZATION
      │  When serialize-vertex executes:
      │  - Type code 18 already in compiled code (inlined)
      │  - No lookup needed
      │  - Direct write to stream
      │
      ↓
┌─────────────────────────────────────────────────────────────┐
│  Layer 4: Storage (Data File on Disk)                       │
│  ───────────────────────────────────────────────────────────│
│  Serialized data:                                           │
│    [18] [5] [4] [65 6C 69 63 65] ...                      │
│     ^    ^   ^   ^                                          │
│     |    |   |   └─ ASCII "alice" (5 bytes)               │
│     |    |   └─ Type code 5 = +string+                    │
│     |    └─ Type code 4 = +symbol+                        │
│     └─ Type code 18 = +vertex+                            │
│                                                             │
│  Reader must know mapping:                                 │
│    18 → "This is a vertex"                                │
│    5 → "This is a string"                                 │
│    etc.                                                    │
└─────┬──────────────────────────────────────────────────────┘
      │
      ├─ DESERIALIZATION (on graph open)
      │  Reader.lisp checks first byte
      │  (case byte                                           │
      │    (18 (read-vertex ...))      ← +vertex+ = 18      │
      │    (5 (read-string ...))       ← +string+ = 5       │
      │    ... )                                             │
      │
      ├─ ⚠️ RISK: If +vertex+ was 18 when written, but 19 when read
      │           Reader interprets vertex data as edge data!
      │
      ↓
┌─────────────────────────────────────────────────────────────┐
│  Result: Data Corruption or Unreadable Graph                │
│  ───────────────────────────────────────────────────────────│
│  Lesson: Type codes must NEVER change between writes/reads  │
└─────────────────────────────────────────────────────────────┘
```

---

## 4. Thread-Safety Execution Patterns

### Safe patterns with globals.lisp

```
PATTERN 1: Binding *graph* per thread
─────────────────────────────────────

(let ((my-graph (open-graph "/tmp/db1")))
  (let ((*graph* my-graph))
    ;; Thread 1: reads/modifies my-graph
    (thread:make-thread
      (lambda ()
        (let ((other-graph (open-graph "/tmp/db2")))
          (let ((*graph* other-graph))
            ;; Thread 2: reads/modifies other-graph
            ;; *graph* bindings isolated!
            ))))))

Execution model:
  Heap:
    ├─ my-graph (instance 1)
    └─ other-graph (instance 2)
  
  Thread 1 stack:
    ├─ *graph* ← my-graph
    └─ reads/writes instance 1
  
  Thread 2 stack:
    ├─ *graph* ← other-graph
    └─ reads/writes instance 2
  
  Result: ✅ Safe - threads don't interfere


PATTERN 2: Type definition at startup (before threading)
─────────────────────────────────────────────────────────

(def-vertex person ((name :type string) (age :type integer)))
(def-edge knows (:from person :to person))
;; Both write to *schema-node-metadata*
;; Now *schema-node-metadata* = {person → class, knows → class}

;; Later, spawn threads
(thread:make-thread (lambda () (make-vertex "person" ...)))
(thread:make-thread (lambda () (make-vertex "person" ...)))

;; Both threads READ from *schema-node-metadata*
;; No concurrent writes, so safe

Execution model:
  *schema-node-metadata* = {person → class, knows → class}
  
  Thread 1: reads person class definition
  Thread 2: reads person class definition
  
  Result: ✅ Safe - concurrent reads are fine


PATTERN 3: Prolog query with thread-local trail
────────────────────────────────────────────────

(let ((*trail* (make-array 200 :fill-pointer 0 :adjustable t)))
  (let ((*select-list* nil))
    (? (ancestor 'alice ?x))
    *select-list*))

Each query gets own *trail* (via let-binding)

Execution model:
  Thread 1:
    *trail* ← array-1 (200 slots)
    Query A executes, modifies array-1
  
  Thread 2:
    *trail* ← array-2 (200 slots)  ← DIFFERENT array!
    Query B executes, modifies array-2
  
  Result: ✅ Safe - trails isolated per thread


PATTERN 4: Caching control per operation
──────────────────────────────────────────

(let ((*cache-enabled* nil))
  ;; Bypass cache for this operation
  (lookup-vertex graph "v1"))

Cache flag is thread-local (let-binding)

Result: ✅ Safe - flag isolated per thread
```

### Unsafe patterns with globals.lisp

```
PATTERN X: Global *graph* modification
──────────────────────────────────────

(setf *graph* my-graph)  ;; GLOBAL modification!
(thread:make-thread
  (lambda ()
    (make-vertex "Person" ...)))

Execution model:
  Main thread:
    Global *graph* ← my-graph (shared!)
  
  Child thread:
    Reads global *graph* = my-graph
    Both threads see SAME *graph*!
  
  If both modify simultaneously:
    Race condition
    Undefined behavior
  
Result: ❌ UNSAFE - threads interfere


PATTERN Y: Concurrent type definition
──────────────────────────────────────

Thread 1: (def-vertex person ...)
  Writes to *schema-node-metadata*

Thread 2: (def-edge knows ...)
  Writes to *schema-node-metadata*

Execution model:
  *schema-node-metadata* (no locking!)
  
  Both threads call (setf (gethash key table) value)
  
  Race condition on hash-table structure
  Possible corruption:
    - Lost write
    - Hash-table corruption
    - SEGV (in worst case)

Result: ❌ UNSAFE - concurrent writes


PATTERN Z: Global Prolog state
───────────────────────────────

Thread 1: (? (ancestor 'alice ?x))
  Modifies *trail*, *var-counter*, *select-list*

Thread 2: (? (parent 'bob ?y))
  Reads/modifies *trail*, *var-counter*, *select-list*

Execution model:
  *trail* (shared, no locking!)
  *var-counter* (non-atomic)
  *select-list* (shared)
  
  Thread 1 adds binding to *trail*
  Thread 2 reads stale *trail*
  Query results wrong!
  
  Thread 1 increments *var-counter* (0→1)
  Thread 2 increments *var-counter* (1→2)  (or reads 0 again!)
  Variable name collision!

Result: ❌ UNSAFE - queries interfere
```

---

## 5. Performance Characteristics

### Big O Analysis

```
┌──────────────────────────────────────────────────────────────┐
│  COMPILE-TIME OPERATIONS                                     │
└──────────────────────────────────────────────────────────────┘

Operation              │ Time        │ Space       │ Notes
──────────────────────┼─────────────┼─────────────┼──────────────
define-constant       │ O(1)        │ O(1)        │ Single entry in symbol table
defvar                │ O(1)        │ O(1)        │ Single slot
defparameter          │ O(1)        │ O(1)        │ Same as defvar
UUID to bytes         │ O(32)       │ O(32)       │ 16-byte UUID conversion
make-hash-table       │ O(1)        │ O(initial)  │ Allocates hash structure
make-array            │ O(1)        │ O(n)        │ Creates array of size n


┌──────────────────────────────────────────────────────────────┐
│  RUNTIME OPERATIONS (After load-time)                        │
└──────────────────────────────────────────────────────────────┘

Reference constant    │ O(1)        │ O(1)        │ Inlined by compiler
Access *graph*        │ O(1)        │ O(1)        │ Symbol lookup
Access *trail*        │ O(1)        │ O(1)        │ Variable reference
Hash-table lookup     │ O(1)avg     │ O(1)        │ SBCL/CCL/LispWorks optimized
Hash-table insert     │ O(1)amort   │ O(1)        │ Grows table as needed
Array element access  │ O(1)        │ O(1)        │ Direct indexing


┌──────────────────────────────────────────────────────────────┐
│  CRITICAL PATH: Vertex Creation                              │
└──────────────────────────────────────────────────────────────┘

(make-vertex "Person" '(("name" . "Alice")))

Step 1: Look up "Person" in *schema-node-metadata*
   Time: O(1) hash lookup
   
Step 2: Generate UUID using *vertex-namespace*
   Time: O(1) reference + O(hash) to compute UUID
   (hash is constant time for fixed-size input)
   
Step 3: Create vertex instance
   Time: O(1) allocation + O(k) where k = field count
   
Step 4: Serialize for storage (uses type codes)
   Time: O(k) where k = data size
   
TOTAL: O(k) where k = field count/data size


┌──────────────────────────────────────────────────────────────┐
│  CRITICAL PATH: Prolog Query                                 │
└──────────────────────────────────────────────────────────────┘

(? (ancestor 'alice ?x))

Step 1: Look up 'ancestor in *prolog-global-functors*
   Time: O(1) hash lookup
   
Step 2: Initialize *trail*
   Time: O(1) reset fill-pointer
   
Step 3: Increment *var-counter*
   Time: O(1) non-atomic increment (RISKY!)
   
Step 4: Execute unification
   Time: O(?) depends on query complexity
   
Step 5: Backtracking (if failure)
   Time: O(t) where t = trail entries
   
TOTAL: O(query_complexity)


┌──────────────────────────────────────────────────────────────┐
│  MEMORY FOOTPRINT                                             │
└──────────────────────────────────────────────────────────────┘

Item                      │ Bytes    │ Notes
──────────────────────────┼──────────┼──────────────────────
*cache-enabled* variable  │ 16       │ Symbol + value pointer
*graph* variable          │ 16       │ Symbol + value pointer
*schema-node-metadata*    │ 1 KB     │ Empty hash-table
*trail* array (200)       │ 1.6 KB   │ 200 array slots
*var-counter*             │ 16       │ Symbol + integer
*functor*                 │ 16       │ Symbol + value
*select-list*             │ 16       │ Symbol + value
Hash-table (SBCL)         │ 1 KB     │ Per registry
UUID byte-arrays          │ 32       │ Two 16-byte arrays
Type codes (35)           │ ~64      │ Immutable constants
──────────────────────────────────────────────────────────────
TOTAL                     │ ~5 KB    │ Negligible memory use


┌──────────────────────────────────────────────────────────────┐
│  PERFORMANCE BOTTLENECKS                                     │
└──────────────────────────────────────────────────────────────┘

🟡 MEDIUM: Hash-table operations under high concurrency
   - If 100 threads register types simultaneously
   - Hash-table internal locks may contend
   - Mitigation: Register types once at startup

🟡 MEDIUM: Array growth in *trail*
   - If Prolog query has deep backtracking
   - *trail* grows beyond initial 200 slots
   - May trigger GC pauses
   - Mitigation: Increase initial size for deep recursion

✅ LOW: Constant inlining
   - Constants inlined at compile-time
   - Zero runtime lookup cost

✅ LOW: UUID namespace computation
   - One-time at load-time
   - No repeated computation

✅ LOW: Global variable access
   - Simple symbol lookup
   - Negligible overhead
```

---

## 6. Concurrency Model

### Thread-Safety Summary Table

```
Variable              │ Thread-safe? │ Protection      │ Risk
──────────────────────┼──────────────┼─────────────────┼────────────────
*cache-enabled*       │ ⚠️  Partial   │ None            │ Benign (read-only mostly)
*graph*               │ ✅ If let     │ Dynamic binding │ 🔴 Unsafe if setf globally
*schema-node-metadata*│ ❌ NO         │ None            │ 🔴 Corruption on concurrent write
*graph-hash*          │ ❌ NO         │ None            │ 🔴 Corruption on concurrent write
*trail*               │ ❌ NO         │ None            │ 🔴 Queries interfere
*var-counter*         │ ❌ NO         │ None            │ 🔴 Variable collision
*functor*             │ ❌ NO         │ None            │ 🔴 Context overwrite
*select-list*         │ ❌ NO         │ None            │ 🔴 Interspersed results
*cont*                │ ❌ NO         │ None            │ 🔴 Continuation collision
*occurs-check*        │ ✅ Partial    │ None            │ 🟡 Global flag, benign read
*prolog-global-functors*│ ✅ YES       │ Hash-table lock │ ✅ Atomic operations
*user-functors*       │ ✅ YES        │ Hash-table lock │ ✅ Atomic operations
*vertex-namespace*    │ ✅ YES        │ Immutable       │ ✅ Never modified
*edge-namespace*      │ ✅ YES        │ Immutable       │ ✅ Never modified
*prolog-trace*        │ ✅ Partial    │ None            │ 🟡 Read-only mostly
```

### Lock Contention Scenarios

```
Scenario A: Multiple threads defining types
──────────────────────────────────────────

Thread 1: def-vertex person
Thread 2: def-edge knows
Thread 3: def-vertex address

All three try to write *schema-node-metadata* simultaneously.

*schema-node-metadata* has NO :synchronized flag!
Result: Hash-table corruption possible


Scenario B: Multiple threads opening graphs
──────────────────────────────────────────

Thread 1: open-graph "db1"  → populates *schema-node-metadata*
Thread 2: open-graph "db2"  → populates *schema-node-metadata*

Both load schemas into same hash-table.

If overlapping type names:
  Thread 1: (setf (gethash "person" table) class1)
  Thread 2: (setf (gethash "person" table) class2)
  
Race condition: Which class wins? Unpredictable.


Scenario C: Prolog query interference
──────────────────────────────────────

Thread 1: (? (ancestor 'alice ?x))
  Modifies *trail* with bindings
  Increments *var-counter* (0→1)
  Collects results in *select-list*

Thread 2: (? (parent 'bob ?y))
  Reads/modifies *trail* (sees Thread 1's bindings!)
  Increments *var-counter* (may read 0 or 1, unclear)
  Appends results to *select-list* (mixed with Thread 1's results)

Result:
  - Query 1 gets Query 2's results
  - Variable names collide (?1 used twice)
  - Backtracking unwinds wrong bindings


Scenario D: Type code versioning disaster
──────────────────────────────────────────

Version 1 of code:
  +vertex+ = 18
  Writes 18 to disk

Version 2 of code (adds new type):
  +new-type+ = 18  ← reused!
  Reads 18 from disk as new-type
  Interprets vertex data as new-type data
  
Result: Data corruption (silent!)
```

---

## 7. Memory Layout & Access Patterns

### How globals.lisp data structures look in memory

```
┌────────────────────────────────────────────────────────────────┐
│                    SYMBOL TABLE (:graph-db)                    │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  Symbol name  │ Type    │ Value address  │ Flags              │
│  ──────────────┼─────────┼────────────────┼────────────────────│
│  *cache-enabled* │ var   │ 0x7FFF0100     │ special            │
│  *graph*        │ var   │ 0x7FFF0110     │ special            │
│  *trail*        │ var   │ 0x7FFF0120     │ special, adjustable│
│  +db-version+   │ const │ (inlined: 1)   │ immutable          │
│  +key-bytes+    │ const │ (inlined: 16)  │ immutable          │
│  +vertex+       │ const │ (inlined: 18)  │ immutable          │
│  ...            │ ...   │ ...            │ ...                │
│                                                                │
└────────────────────────────────────────────────────────────────┘

Value addresses:
  0x7FFF0100: t (boolean, *cache-enabled* bound here)
  0x7FFF0110: nil (*graph* bound here initially)
  0x7FFF0120: array object (pointer to array struct)
  
When let-binding overrides:
  0x7FFF0110: my-graph-instance (overrides nil, thread-local)


┌────────────────────────────────────────────────────────────────┐
│                 HASH-TABLE LAYOUT (*schema-node-metadata*)     │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  Bucket index  │ Key         │ Value (type metadata)          │
│  ──────────────┼─────────────┼────────────────────────────────│
│  0             │ "person"    │ CLOS class object (16 KB)      │
│  5             │ "address"   │ CLOS class object (8 KB)       │
│  17            │ "knows"     │ CLOS class object (4 KB)       │
│  ...           │ ...         │ ...                            │
│                                                                │
│  Initial size: ~256 buckets (SBCL/CCL default)               │
│  Growth: Doubles when load factor exceeds ~0.75               │
│  Current usage: ~3 types = 3 * 10 KB = 30 KB                │
│  Empty table: ~1 KB (hash structure + empty buckets)         │
│                                                                │
└────────────────────────────────────────────────────────────────┘


┌────────────────────────────────────────────────────────────────┐
│                    ARRAY LAYOUT (*trail*)                      │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  Initial allocation:                                          │
│    Size: 200 elements                                         │
│    Element type: binding record (unspecified, ~32 bytes)     │
│    Total: 200 * 32 = 6.4 KB                                 │
│    Fill-pointer: 0 (initially empty)                         │
│                                                                │
│  Usage pattern:                                               │
│    Query 1: (? (goal))                                       │
│      Adds ~10 bindings to *trail* (fill-pointer → 10)       │
│      Backtrack, unwind → fill-pointer → 0                    │
│                                                                │
│    Query 2: (? (complex-goal))                              │
│      Adds ~150 bindings → fill-pointer → 150                 │
│      Still fits in 200-element array                         │
│                                                                │
│    Query 3: (? (very-deep-query))                           │
│      Needs 250 bindings → exceeds 200!                       │
│      Array auto-grows: 200 → 400 elements                    │
│      New memory allocated, old data copied → GC pause        │
│                                                                │
│  Memory growth over time:                                     │
│    Startup: 200 * 32 = 6.4 KB                               │
│    After 10 queries: ~400 * 32 = 12.8 KB (or higher)       │
│    Peak (very deep query): Could reach 1 MB+ if needed       │
│                                                                │
└────────────────────────────────────────────────────────────────┘


┌────────────────────────────────────────────────────────────────┐
│            CACHE-LINE ANALYSIS (CPU efficiency)               │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  Modern CPU cache line: 64 bytes                              │
│                                                                │
│  Globals.lisp variables in memory:                            │
│    *cache-enabled*: 16 bytes → fits in 1 cache line         │
│    *graph*: 16 bytes → fits in 1 cache line                 │
│    *trail*: 16 bytes (pointer to array)                     │
│    UUID namespaces: 32 bytes → fits in 1 cache line         │
│                                                                │
│  Access pattern:                                              │
│    Operations check *cache-enabled* frequently               │
│    Multiple variables could share cache line                 │
│    No false-sharing issue (variables are read-mostly)        │
│                                                                │
│  Result: CPU cache efficiency is EXCELLENT for globals.lisp  │
│                                                                │
└────────────────────────────────────────────────────────────────┘
```

---

## 8. Critical Gotchas & Edge Cases

### Gotcha 1: Type Code Collision Risk

**Problem:**

```
Line 16: +fixed-integer-64+ = #x01  (same as +storage-version+ = #x01)
```

Two different constants have identical values!

**Execution scenario:**

```
Layer 4 reader:
  (read-byte stream) → #x01
  (case *
    (#x01 (read-storage-version))      ← matches FIRST
    (#x01 (read-fixed-integer) ) ← never reached!
```

If reader intended to interpret as fixed-integer, it reads as storage-version instead.

**Impact:** Silent misinterpretation of data

---

### Gotcha 2: UUID Namespace Hardcoding

**Problem:**

UUIDs hardcoded at load-time. If changed in source code:

```lisp
Old code:
  *vertex-namespace* = UUID("2140DCE1-3208-4354-8696-5DF3076D1CEB")
  
New code:
  *vertex-namespace* = UUID("AAAAAAAA-AAAA-AAAA-AAAA-AAAAAAAAAAAA")
```

Old saved vertex with ID = hash(old-namespace, data)
New code generates vertex ID = hash(new-namespace, data)

**Result:** Old and new vertex IDs never match, graph broken

---

### Gotcha 3: *schema-node-metadata* Corruption on Concurrent Write

**Problem:**

```lisp
Thread A: (setf (gethash "person" *schema-node-metadata*) class-A)
Thread B: (setf (gethash "person" *schema-node-metadata*) class-B)
```

No locking. Hash-table internal structure may corrupt.

**Scenario:**

```
Hash-table resize triggered during write:
  1. Thread A allocates new bucket array
  2. Thread B allocates new bucket array (different!)
  3. Thread A starts copying entries to new array
  4. Thread B overwrites new array with its copy
  5. Some of Thread A's entries lost

Result: Hash-table corrupted, subsequent lookups fail or return wrong values
```

---

### Gotcha 4: *var-counter* Non-Atomic Increment

**Problem:**

```
*var-counter* = 5

Thread A: (setf *var-counter* (1+ *var-counter*))
  ├─ Read *var-counter* → 5
  ├─ Compute 1+ → 6
  └─ Write back → 6

Thread B: (setf *var-counter* (1+ *var-counter*))
  ├─ Read *var-counter* → 5  (still 5, if B reads before A writes!)
  ├─ Compute 1+ → 6
  └─ Write back → 6  (both wrote 6, one lost!)

Expected: *var-counter* = 7
Actual: *var-counter* = 6
```

**Execution trace:**

```
Time t0: *var-counter* = 5
Time t1: Thread A reads → 5
Time t2: Thread B reads → 5 (race!)
Time t3: Thread A writes → 6
Time t4: Thread B writes → 6 (overwrites A's write)
Time t5: *var-counter* = 6 (should be 7)

Variable name collision:
  Both threads think they created ?1 (from same counter value 5)
  Later unification confuses bindings
```

---

### Gotcha 5: +data-extent-size+ Overflow

**Problem:**

```
+data-extent-size+ = 104,857,600 bytes = 100 MB (compile-time constant)

If system needs 300 MB:
  - Extent 1: 0-100 MB
  - Extent 2: 100-200 MB
  - Extent 3: 200-300 MB
  
But what if extent size needs to be 50 MB for low-memory system?

Cannot change: constant is compile-time inlined everywhere
Need to: Recompile entire system with different +data-extent-size+
```

---

### Gotcha 6: Prolog *trail* Array Exhaustion

**Problem:**

```
*trail* = array(200) (initial)

Query with 500 backtracking points:
  Fills 200 slots
  Grows to 400 slots (auto-expansion)
  Fills 400 slots
  Grows to 800 slots
  GC pause triggered by allocation
  
On very deep recursion (5000+ backtracking points):
  Array grows to 8000+ slots
  May allocate 256 KB+ per query
  System RAM pressure increases
```

---

## 9. Risk Landscape

### Risk Severity Heatmap

```
SEVERITY LEVEL        COUNT   EXAMPLES
──────────────────────┼───────┼────────────────────────────────────
🔴 BLOCKING           4       - *schema-node-metadata* not sync'd
                              - *trail*, *var-counter* not isolated
                              - UUID namespaces hardcoded
                              - Type codes not versioned

🟠 CRITICAL           3       - +data-extent-size+ hardcoded
                              - *graph-hash* not thread-safe
                              - Prolog global state interference

🟡 WARNING            6+      - +fixed-integer-64+ redundancy
                              - +max-node-types+ no overflow check
                              - *cache-enabled* global mutation
                              - *occurs-check* global flag
                              - Array growth GC pauses
                              - Concurrent type registration


RISK DEPENDENCY CHAIN:

  🔴 BLOCKING: *schema-node-metadata* not thread-safe
      └─ Causes: Hash-table corruption on concurrent write
           └─ Causes: Type definitions lost or malformed
                └─ Causes: Vertex creation with wrong type
                     └─ Severity: 🔴 DATA CORRUPTION


  🔴 BLOCKING: *trail*, *var-counter* not isolated
      └─ Causes: Prolog query interference
           └─ Causes: Wrong query results
                └─ Causes: Silent data inconsistency
                     └─ Severity: 🔴 CRITICAL


  🔴 BLOCKING: Type codes not versioned
      └─ Causes: Serialization format change undetectable
           └─ Causes: Old data misinterpreted
                └─ Causes: Permanent data loss
                     └─ Severity: 🔴 CATASTROPHIC


  🟠 CRITICAL: UUID namespaces hardcoded
      └─ Causes: ID generation incompatibility
           └─ Causes: Graph structure broken on namespace change
                └─ Severity: 🔴 BLOCKING (after change)
```

---

## 10. Decision Trees

### Decision: Can I safely use concurrent type definition?

```
START: Want to register Prolog predicate from multiple threads

  ├─ Using built-in predicates only?
  │  └─ YES → Safe
  │     *prolog-global-functors* is thread-safe (has :synchronized flag)
  │
  └─ Registering custom predicates from threads?
     └─ Using def-global-prolog-functor on each thread?
        └─ NO → Safe (query-time safe)
        │  Looking up predicates is atomic
        │
        └─ YES → ⚠️ RISKY
           Write operations on *prolog-global-functors* from threads
           Even though hash-table is sync'd, other Prolog state (*trail*) not
           Result: Unpredictable behavior

RECOMMENDATION: Register all custom predicates at startup, before spawning threads
```

### Decision: Should I bind *graph* with let or setf globally?

```
START: Need to access graph in code

  ├─ Single-threaded application?
  │  └─ Either works (let preferred for clarity)
  │
  └─ Multi-threaded application?
     └─ All threads operate on same graph?
     │  └─ Use let-binding (thread-local)
     │
     └─ Different threads, different graphs?
        └─ Use let-binding (thread-local)
           Each thread gets own *graph* binding

NEVER use (setf *graph* global-value)
ALWAYS use (let ((*graph* my-graph)) ...)
```

### Decision: How deep can my Prolog query go?

```
START: Planning Prolog query depth

  ├─ Shallow queries (< 100 backtracking points)?
  │  └─ Safe with default *trail* (200 slots)
  │
  ├─ Medium queries (100-500 backtracking points)?
  │  └─ *trail* will auto-grow to 400-800 slots
  │     GC pause possible but manageable
  │
  └─ Deep queries (> 500 backtracking points)?
     └─ Consider increasing initial *trail* size:
        (let ((*trail* (make-array 2000 :fill-pointer 0 :adjustable t)))
          (? (very-deep-query)))

RECOMMENDATION: Profile actual query depth before deployment
```

---

## 11. Summary Insights

### Key Takeaways

**1. Load-time computation minimizes runtime cost**
   - UUID namespaces computed once at load
   - Constants inlined by compiler
   - No repeated computation overhead

**2. Global state creates threading bottleneck**
   - 9 mutable globals, most unprotected
   - Even with let-binding, some (e.g., *schema-node-metadata*) shared
   - Multi-threaded apps need careful coordination

**3. Type codes are critical but fragile**
   - 35 type codes must never change
   - No versioning mechanism
   - Single change breaks all old data

**4. UUID namespaces are permanent**
   - Hardcoded at load-time
   - Changing namespace breaks ID generation
   - No migration path

**5. Hash-table thread-safety is Lisp-specific**
   - SBCL: :synchronized t
   - CCL: :shared t
   - LispWorks: :single-thread nil
   - Code is portable, but requires correct flags

**6. Prolog state is fundamentally problematic**
   - *trail*, *var-counter*, *select-list* are global
   - Concurrent queries interfere
   - Needs thread-local isolation

**7. Constants are fast but immutable**
   - Compile-time inlining = zero runtime cost
   - Cannot be adjusted at runtime
   - Configuration limited to defparameters

**8. Configuration is mostly hardcoded**
   - +data-extent-size+ fixed at 100 MB
   - Macro-heavy approach, no runtime adjustment
   - Changing requires recompilation

**9. Memory footprint is minimal**
   - Total: ~5 KB for all globals.lisp state
   - Negligible compared to actual graph data
   - Not a bottleneck

**10. Phase 3 lock-in will be severe**
   - All 50+ constants must freeze
   - Type codes, UUIDs, magic bytes permanent
   - Future changes require major versioning

---

### Critical Decisions Before Phase 3

**MUST address:**

1. ☐ Thread-safety of *schema-node-metadata* (add :synchronized)
2. ☐ Thread-isolation of *trail*, *var-counter* (per-query binding)
3. ☐ Versioning of type codes (add format version)
4. ☐ UUID namespace storage (version with graph)

**SHOULD address:**

5. ☐ Configuration of +data-extent-size+ (make parameter)
6. ☐ Validation of +max-node-types+ (check on type def)
7. ☐ Documentation of Prolog state interactions

**NICE to have:**

8. ☐ Remove +fixed-integer-64+ redundancy
9. ☐ Clean up unused/unclear symbols
10. ☐ Add comprehensive docstrings (Nivel 3 done ✓)

---

## Conclusion

**globals.lisp is the configuration skeleton of VivaceGraph.** Its constants affect every layer; its global variables are used throughout. While load-time execution is efficient and memory footprint is minimal, thread-safety issues and lack of versioning create significant risks for multi-threaded applications and long-term maintainability.

**Key challenge:** Balancing configuration immutability (compile-time constants for performance) with flexibility (runtime configuration). Current approach heavily favors immutability, at cost of flexibility.

**Phase 3 implications:** Once these constants freeze, all future changes require versioning and migration. Design decisions made here are permanent.

