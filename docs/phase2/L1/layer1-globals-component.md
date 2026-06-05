# Layer 1 Component Specification: globals.lisp

**Component:** VivaceGraph Global State & Configuration Constants  
**File:** src/globals.lisp  
**Lines:** 134  
**Type:** Global state initialization & configuration  
**Purpose:** Define all global variables, dynamic parameters, and serialization constants used by Layers 1-7  
**Scope:**
- ✓ Defines mutable global state (`*graph*`, `*schema-node-metadata*`, `*trail*`, etc.)
- ✓ Defines immutable compile-time constants (magic bytes, type codes, sizes)
- ✓ Defines thread-safe hash-tables for Prolog (conditional per Lisp)
- ✗ Does NOT implement any functionality
- ✗ Does NOT perform initialization beyond variable creation

---

## Conceptual Model

### Core Idea

**`globals.lisp` is the configuration and state backbone of VivaceGraph.**

It serves two purposes:

1. **Configuration layer:** Defines all constants (magic bytes, type codes, sizes, limits) that other layers depend on
2. **Global state registry:** Declares all mutable global variables that multiple layers use to coordinate execution

### Motivation

Without `globals.lisp`:
- ❌ Each layer would define its own constants (inconsistent, error-prone)
- ❌ No centralized place to find magic byte meanings or type code mappings
- ❌ Global state scattered across files (hard to reason about side effects)
- ❌ No single point to add thread-safety wrappers

With `globals.lisp`:
- ✅ Single source of truth for all constants
- ✅ Central registry of mutable state
- ✅ Easy to understand what globals exist and how they interact
- ✅ Configuration can be adjusted in one place (when made parameterizable)

### Abstraction Boundary

**What it exposes:**
- 50+ constants (magic bytes, type codes, sizes, limits)
- 9 mutable global variables (graph context, schema registry, Prolog state)
- Thread-safe hash-tables for Prolog (3 variants per Lisp)
- UUID namespaces for deterministic ID generation

**What it hides:**
- Implementation details of Prolog trail structure
- Internal structure of schema metadata
- Serialization format (just defines byte codes, not structure)

**What it should hide but currently doesn't:** ⚠️
- `*graph*`, `*trail*`, `*var-counter*`, `*select-list*` are exposed for direct access
- Prolog state variables should be encapsulated in query macros

**What it should NOT hide:**
- Constants (compile-time knowledge needed by all layers)
- UUID namespaces (needed for consistent ID generation)

### Key Properties

Property | Value | Rationale
----------|-------|----------
**Mutability** | Mixed | Constants immutable; variables mutable (threading risk)
**Scope** | Global | All definitions in `:graph-db` package
**Initialization** | Load-time | UUIDs computed at load; hash-tables created at load
**Thread safety** | Partial | Prolog hash-tables thread-safe; other state not
**Configuration** | Hardcoded | Values not adjustable without code modification
**Versioning** | None | Type codes, magic bytes not versioned
**Permanence** | Permanent | Changes break all stored graphs

---

## Interface Definition

### Operation 1: Access Compile-Time Constants

**Signature:**

Reference any of 50+ constants at compile or runtime (e.g., `+db-version+`, `+key-bytes+`, `+positive-integer+`)

**Semantics:**
- **Constants** are immutable values defined via `alexandria:define-constant`
- Cannot be modified (attempt raises error)
- Evaluated at compile-time, inlined by compiler
- Available in all layers that `(in-package :graph-db)`

**Returns:**
- Constant value (integer, byte-array, keyword, etc.)

**Guarantee:**
- Same value throughout program execution
- Shared across all threads (no thread-local variants)

**Examples:**

```
+db-version+              → 1           (database format version)
+key-bytes+               → 16          (size of UUID key in bytes)
+positive-integer+        → 2           (type code for positive ints)
+null-key+                → byte-array  (sentinel for range queries)
+data-magic-byte+         → #x17        (identifies data blocks)
+max-node-types+          → 65536       (limit on distinct types)
```

**Performance:**
- **Time:** O(1) compile-time; O(1) runtime (inlined)
- **Space:** O(1) per constant

**Edge Cases:**

1. **Constant not bound:**
   - Code references undefined constant (e.g., `+new-type+`)
   - Error: UNDEFINED VARIABLE
   - Fix: Add constant definition to globals.lisp, recompile

2. **Constant value assumption violated:**
   - Layer 4 assumes `+key-bytes+ = 16` (UUID size)
   - If changed to 32, Layer 4 breaks
   - Risk: Silent data corruption (reads wrong bytes)
   - No validation that constant changes are compatible

3. **Magic byte collision:**
   - If two constants have same value (e.g., `+storage-version+ = #x01`, `+fixed-integer-64+ = #x01`)
   - Deserialization ambiguous
   - Risk: Misinterprets data type

---

### Operation 2: Access Mutable Global Variables

**Signature:**

Bind or access mutable globals (`*graph*`, `*schema-node-metadata*`, `*trail*`, etc.)

**Semantics:**
- **Defvar** creates mutable variable with optional initial value
- Can be rebound with `let` (dynamic scope)
- Can be modified with `setf` (not recommended; breaks invariants)
- Thread-local when bound with `let`; global when modified with `setf`

**Returns:**
- Current value of variable

**Guarantee:**
- Value persists across function calls (unless rebound)
- Same value for all code in same dynamic scope

**Examples:**

```
*graph*                   → nil or graph-instance
*cache-enabled*           → t (caching on)
*schema-node-metadata*    → hash-table of type metadata
*trail*                   → adjustable array (Prolog choice points)
*var-counter*             → 0 or positive integer
*functor*                 → nil or functor object
*select-list*             → nil or list of query results
```

**Safe patterns:**

```lisp
; Pattern A: Bind for scope
(let ((*graph* my-graph))
  (with-transaction
    (make-vertex "Person" ...)))

; Pattern B: Macro handles binding
(with-transaction
  (make-vertex "Person" ...))

; Pattern C: Query with result accumulation
(let ((*select-list* nil))
  (? (ancestor 'x 'y))
  *select-list*)
```

**Unsafe patterns:**

```lisp
; Pattern X: Direct global modification (AVOID)
(setf *graph* my-graph)
(thread:make-thread (lambda () ...))
; Both threads see same *graph*; data race!

; Pattern Y: Modifying Prolog state
(setf *trail* (make-array 200))
; Breaks backtracking; stale bindings
```

---

### Operation 3: Initialize UUID Namespaces

**Signature:**

UUID namespaces computed at load-time and stored in `*vertex-namespace*`, `*edge-namespace*`

**Semantics:**
- **Load-time computation:** When globals.lisp loads, UUID-to-byte-array conversion happens once
- **Immutable after creation:** Variables defined as `defvar` but value never reassigned
- **Used for v5 UUID generation:** Each vertex/edge ID is hash(namespace + data)

**Returns:**
- Byte-array (16 bytes) representing UUID namespace

**Guarantee:**
- Same namespace value throughout execution (unless code reassigns, which is dangerous)
- Different namespace for vertices vs. edges (prevents ID collision)

**Examples:**

```
*vertex-namespace*
  = byte-array from UUID "2140DCE1-3208-4354-8696-5DF3076D1CEB"
  
*edge-namespace*
  = byte-array from UUID "0392C7B5-A38B-466F-92E5-5A7493C2775A"

When creating vertex ID:
  hash(*vertex-namespace* + vertex-data) → unique UUID
  
When creating edge ID:
  hash(*edge-namespace* + edge-data) → unique UUID
  
Result: vertex IDs ≠ edge IDs (even with identical data)
```

**Performance:**
- **Load-time:** O(32) bytes converted (UUID size)
- **Runtime:** O(1) reference to pre-computed namespace

**Edge Cases:**

1. **Namespace mismatch:**
   - Old file created with namespace A
   - New code loads with namespace B
   - Old IDs don't regenerate correctly
   - Result: Graph structure broken

2. **Hardcoded UUIDs risk:**
   - These specific UUIDs are baked into every saved graph
   - If code moves to new machine/environment, UUIDs must be identical
   - No migration path if UUIDs need to change

---

### Operation 4: Access Thread-Safe Prolog Hash-Tables

**Signature:**

Access or modify `*prolog-global-functors*` or `*user-functors*` with thread-safety guarantees

**Semantics:**
- **Hash-tables created with Lisp-specific thread-safety flags**
- SBCL: `:synchronized t` (internal locks)
- CCL: `:shared t` (thread-shared)
- LispWorks: `:single-thread nil` (thread-safe)
- Atomic operations on hash-table (getgethash, setf, remhash)
- But: Other Prolog state (*trail*, *var-counter*) NOT synchronized

**Returns:**
- Value from hash-table lookup

**Guarantee:**
- Single hash-table operation is atomic
- Multiple operations may race with other threads

**Examples:**

```
; Register Prolog predicate (atomic operation)
(setf (gethash 'ancestor *prolog-global-functors*)
      (make-functor 'ancestor ...))

; Lookup predicate (atomic operation)
(gethash 'parent *prolog-global-functors*)
  → functor object (or nil if not found)

; Different Lisps, same semantics
SBCL:   hash-table operations use SB-THREAD locks
CCL:    hash-table operations use CCL's shared hash-table
LispWorks: hash-table operations are documented as thread-safe
```

**Performance:**
- **Time:** O(1) hash lookup + atomic operation
- **Space:** O(n) where n = number of functors

**Edge Cases:**

1. **Functor registry vs. trail inconsistency:**
   - Thread A looks up functor, gets F
   - Thread B redefines functor to F'
   - Thread A starts executing old F, but *trail* modified by other thread's F'
   - Result: Prolog state corrupted

2. **Hash-table empty after lookup but before use:**
   - Thread A: checks `(gethash 'pred *functors*)` → found
   - Thread B: deletes predicate
   - Thread A: tries to call predicate → not found
   - Result: Query fails unexpectedly

---

### Operation 5: Configure Tunable Parameters

**Signature:**

Access or modify parameters (`*cache-enabled*`, `*occurs-check*`, `*prolog-trace*`, `*initial-extents*`, `*max-locks*`)

**Semantics:**
- **Defparameter** creates adjustable parameter with default value
- Can be rebound with `let` (dynamic scope)
- Can be modified with `setf` (affects global behavior)
- No thread-safety guarantees

**Returns:**
- Current parameter value

**Examples:**

```
*cache-enabled*           → t       (enable caching globally)
*occurs-check*            → t       (enable occurs check in unification)
*prolog-trace*            → nil     (disable tracing)
*initial-extents*         → 10      (10 initial memory extents)
*max-locks*               → 10000   (max concurrent locks)
```

**Safe patterns:**

```lisp
; Temporarily disable caching
(let ((*cache-enabled* nil))
  (graph-db:with-transaction
    (make-vertex ...)))

; Enable Prolog tracing for debugging
(let ((*prolog-trace* t))
  (? (some-query ...)))
```

**Unsafe patterns:**

```lisp
; Global modification affects all threads
(setf *cache-enabled* nil)
(thread:make-thread (lambda () ...))
; Child thread sees caching disabled (unintended side effect)
```

---

## Variants / Specializations

VivaceGraph's globals definition has **4 variants** depending on Lisp implementation:

### Variant A: SBCL (Lines 115-118)

**Characteristics:**

```lisp
#+sbcl
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
#+sbcl
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql))
```

- Uses SBCL's `:synchronized t` flag for thread-safety
- Hash-table locks automatically manage concurrent access
- High performance (lock-free or lock-based, depends on SBCL version)
- Only variant available on SBCL

**Thread-safety guarantee:**
- Individual hash-table operations are atomic
- But multiple operations can race (no transactional semantics)

---

### Variant B: CCL (Lines 125-128)

**Characteristics:**

```lisp
#+ccl
(defvar *prolog-global-functors* (make-hash-table :shared t))
#+ccl
(defvar *user-functors* (make-hash-table :shared t :test 'eql))
```

- Uses CCL's `:shared t` flag for thread-safe hash-tables
- Designed for shared memory concurrency
- CCL explicitly documents shared hash-tables as thread-safe
- Lower overhead than SBCL (no external locks)

**Thread-safety guarantee:**
- Individual hash-table operations are atomic
- Shared semantics across threads

---

### Variant C: LispWorks (Lines 120-123)

**Characteristics:**

```lisp
#+lispworks
(defvar *prolog-global-functors* (make-hash-table :single-thread nil))
#+lispworks
(defvar *user-functors* (make-hash-table :single-thread nil :test 'eql))
```

- Uses LispWorks's `:single-thread nil` flag for thread-safe hash-tables
- Default is single-threaded; `:single-thread nil` enables multi-threaded access
- LispWorks documentation guarantees thread-safety
- Efficient on LispWorks (optimized for concurrent access)

**Thread-safety guarantee:**
- Individual hash-table operations are atomic
- Designed for multi-threaded environments

---

### Summary: Hash-Table Thread-Safety Across Lisps

| Lisp | Parameter | Method | Guarantee |
|------|-----------|--------|-----------|
| **SBCL** | `:synchronized t` | Internal locks | Atomic operations |
| **CCL** | `:shared t` | Shared memory | Atomic operations |
| **LispWorks** | `:single-thread nil` | Thread-safe flag | Atomic operations |

**Key insight:** All three achieve atomic hash-table operations, but implementation differs. Code using hash-tables is portable; performance characteristics differ per Lisp.

---

## Usage Patterns

### Pattern 1: Graph Context Binding (Thread-Local Scope)

**Context:** User code needs to work with a specific graph.

**Example:**

```lisp
(in-package :my-app)

(let ((my-graph (graph-db:open-graph "/tmp/mydb")))
  (let ((graph-db:*graph* my-graph))
    (graph-db:with-transaction
      (graph-db:make-vertex "Person" '(("name" . "Alice"))))))
```

**How it works:**

1. Line 2: Open graph from disk, get instance
2. Line 3: Bind `*graph*` to my-graph (dynamic scope)
3. Line 4: Macro binds `*transaction*` (within `*graph*` scope)
4. Line 5: Operations use current `*graph*` and `*transaction*`

**Why thread-safe:**
- Each thread has its own dynamic binding of `*graph*`
- Two threads can have different `*graph*` values simultaneously
- When thread exits `let`, binding restored

---

### Pattern 2: Schema Type Registration

**Context:** Define vertex and edge types at startup.

**Example:**

```lisp
(graph-db:def-vertex person
  ((name :type string)
   (age :type integer)))

(graph-db:def-edge knows
  (:from person :to person)
  ((since :type timestamp)))
```

**How it works:**

1. `def-vertex` macro creates CLOS class
2. Registers metadata in `*schema-node-metadata*` hash-table
3. Can now create instances: `(make-vertex "person" ...)`
4. Edge definitions similar

**Side effects:**
- Modifies `*schema-node-metadata*` (global state)
- Type registry grows as new types defined
- Definitions are permanent (not thread-local)

**Recommendations:**
- Define all types at application startup
- Before spawning threads
- Avoid concurrent type registration (not thread-safe)

---

### Pattern 3: Prolog Query with Result Collection

**Context:** Execute Prolog query and collect all solutions.

**Example:**

```lisp
(graph-db:def-global-prolog-functor 'ancestor '(x y)
  '((parent x y))
  '((ancestor x z) (ancestor z y)))

(let ((graph-db:*select-list* nil))
  (graph-db:? (ancestor 'alice ?x))
  graph-db:*select-list*)
```

**How it works:**

1. Define Prolog rule (lines 1-3)
2. Initialize `*select-list*` to empty (line 5)
3. Execute query `(? (ancestor 'alice ?x))`
4. During query, each solution added to `*select-list*`
5. Return accumulated results

**Side effects:**
- Modifies `*select-list*` during query
- Modifies `*trail*` for backtracking
- Increments `*var-counter*` for variable naming

**Recommendations:**
- Use `let` to isolate `*select-list*` per thread
- Don't rely on global `*select-list*` (thread-safety risk)
- Better: use `graph-db:select` or similar wrapper macro

---

### Pattern 4: Caching Control

**Context:** Temporarily disable caching for consistency.

**Example:**

```lisp
(let ((graph-db:*cache-enabled* nil))
  (graph-db:with-transaction
    (graph-db:make-vertex "Person" ...)
    (graph-db:lookup-vertex ...)))
```

**How it works:**

1. Bind `*cache-enabled*` to nil (disables caching)
2. All operations in scope bypass cache
3. Ensures fresh reads from storage
4. When `let` exits, caching restored

**When to use:**
- Debugging data consistency issues
- Testing against stale cache data
- Performance testing (cache vs. no-cache)

---

### Pattern 5: Type Code Reference in Serialization

**Context:** Layer 4 serializes a Lisp value.

**Example:**

```lisp
(defun serialize-value (value)
  (cond
    ((integerp value)
     (if (< value 0)
       (write-byte graph-db:+negative-integer+ stream)
       (write-byte graph-db:+positive-integer+ stream)))
    ((stringp value)
     (write-byte graph-db:+string+ stream))
    ((graph-db:vertex-p value)
     (write-byte graph-db:+vertex+ stream))
    ... ))
```

**How it works:**

1. Inspect value type
2. Look up corresponding type code from globals.lisp
3. Write type byte to stream
4. Write value data
5. Deserializer reads type byte, dispatches appropriately

**Guarantee:**
- Same type code always maps to same data format
- Serialized data interpretable across restarts (if type codes unchanged)

**Risk:**
- If type codes change, old data becomes garbage
- No version number to detect mismatch

---

## Implementation Guide

### How VivaceGraph Initializes Its Globals

**Step 1: Load globals.lisp at startup**

```
(asdf:load-system :vivacegraph)
  → Loads vivacegraph.asd
  → Compiles & loads src/package.lisp (package definition)
  → Compiles & loads src/globals.lisp (this file)
  → UUID namespaces computed (load-time)
  → Hash-tables created (Lisp-specific)
```

**Step 2: Constants become available**

All 50+ constants now defined:
- `+db-version+`, `+key-bytes+`, `+positive-integer+`, etc.
- Accessible in all layers via `:graph-db` package

**Step 3: Global variables initialized**

```
*graph* ← nil                    (no graph loaded yet)
*cache-enabled* ← t              (caching enabled by default)
*schema-node-metadata* ← {}      (empty hash-table)
*trail* ← array(200)             (empty, ready for Prolog)
*prolog-global-functors* ← {}    (empty, ready for predicates)
```

**Step 4: Subsequent layers depend on this**

- Layer 2-7 code imports constants and uses global variables
- `clos.lisp` populates `*schema-node-metadata*` with MOP support
- `utilities.lisp` uses constants for serialization
- Layer 3+ uses `*graph*`, `*transaction*` for transaction handling

---

### Global State Interaction Model

**State flow during typical operation:**

```
Application startup:
  (asdf:load-system :vivacegraph)
    └─ globals.lisp loads
       └─ All constants defined
       └─ UUIDs computed
       └─ Hash-tables created (empty)

User opens graph:
  (let ((g (open-graph "/tmp/mydb")))
    (let ((*graph* g))
      ...))
    
    ├─ open-graph reads file
    ├─ Populates *schema-node-metadata* with schema from file
    ├─ Binds *graph* for dynamic scope
    └─ Other operations reference *graph*, *schema-node-metadata*

User creates vertex:
  (make-vertex "Person" ...)
    ├─ Checks *graph* (current graph)
    ├─ Looks up type in *schema-node-metadata*
    ├─ Generates UUID using *vertex-namespace*
    ├─ Creates instance
    └─ Returns vertex

Prolog query execution:
  (? (ancestor 'x 'y))
    ├─ Looks up 'ancestor in *prolog-global-functors*
    ├─ Initializes *trail* (choice points)
    ├─ Increments *var-counter* for ?x, ?y
    ├─ Executes unification
    └─ If fail, backtracks using *trail*
```

---

### Thread Safety Interaction Map

```
Safe (thread-local):        Unsafe (global):
─────────────────────────   ────────────────
*graph*                     *schema-node-metadata*
  (bound via let)             (shared hash-table, not sync'd)
*transaction*               *trail*
  (bound via with-transaction) (shared array, not sync'd)
*select-list*               *var-counter*
  (bound via let)             (shared counter, not atomic)
                             *functor*
                               (shared context, not locked)
                             *cache-enabled*
                               (global flag, benign if toggle)
                             
Thread-safe (by design):
──────────────────────
*prolog-global-functors*
  (sync'd hash-table, Lisp-specific)
*user-functors*
  (sync'd hash-table, Lisp-specific)
```

---

## Performance Characteristics

### Compile-Time Cost

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Define constant | O(1) | `alexandria:define-constant` (one-time at load) |
| Define variable | O(1) | `defvar` creates slot in package |
| Create UUID namespace | O(32) | 16-byte UUID converted to byte-array |
| Create hash-table | O(1) | Allocate hashtable structure |
| Total load | O(1) | All operations constant-time |

**Observation:** Load time dominated by I/O (disk reads), not computation.

---

### Runtime Cost

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Reference constant | O(1) | Compiler inlines value |
| Access defvar | O(1) | Symbol lookup in package |
| Access defparameter | O(1) | Symbol lookup in package |
| Hash-table lookup | O(1) | Average case; O(n) worst case (very rare) |
| Hash-table insert | O(1) | Amortized; grows table as needed |

**Observation:** All operations have negligible cost. Hash-table performance depends on hash function quality (library responsibility).

---

### Memory Cost

| Item | Bytes | Notes |
|------|-------|-------|
| Constant value | 1-32 | Varies by constant (integers, byte-arrays, etc.) |
| Variable slot | 16 | Symbol pointer + value slot |
| UUID namespace | 32 | 16-byte UUID * 2 (vertices + edges) |
| Hash-table (empty) | 256-1024 | SBCL/CCL/LispWorks depend on initial size |
| Type codes | 64 | 35 constants * ~2 bytes each |
| **Total** | ~2 KB | Negligible |

**Observation:** Globals.lisp uses minimal memory. Hash-tables grow as needed.

---

## Constraints & Safety

### Safe Operations

| Operation | Safe? | Reason |
|-----------|-------|--------|
| Reference constant | ✅ | Immutable, compile-time |
| Bind defvar with `let` | ✅ | Thread-local scope |
| Bind defparameter with `let` | ✅ | Thread-local scope |
| Hash-table lookup | ✅ | Atomic operation (per Lisp) |
| UUID namespace reference | ✅ | Immutable after load |

---

### Unsafe Operations

| Operation | Safe? | Reason | Risk |
|-----------|-------|--------|------|
| Modify constant | ❌ | Violates immutability | COMPILATION ERROR |
| `(setf *graph* ...)` globally | ❌ | Global mutation | Data race in threads |
| `(setf *trail* ...)` | ❌ | Breaks backtracking | Prolog queries fail |
| `(setf *var-counter* ...)` | ❌ | Non-atomic, races | Variable name collisions |
| Modify `*schema-node-metadata*` without lock | ❌ | No synchronization | Hash-table corruption |
| Redefine UUID namespace | ❌ | Breaks ID generation | Old IDs unmatchable |
| Change type code assignments | ❌ | Breaks serialization | Old data unreadable |

---

### Recommended Patterns

| Pattern | Rationale |
|---------|-----------|
| Bind globals with `let` | Thread-local scope, safe |
| Use macros to manage state | Encapsulation, less error-prone |
| Define all types at startup | Avoid concurrent registration |
| Never modify constants | Immutability contract |
| Assume UUIDs never change | ID consistency guarantee |
| Don't rely on global `*graph*` | Use with-transaction instead |

---

### Concurrency Model

**Thread-safe by design:**
- `*prolog-global-functors*`, `*user-functors*` (hash-table atomicity)
- `*graph*` when bound with `let` (thread-local)
- `*transaction*` when bound via macro (thread-local)

**NOT thread-safe:**
- `*schema-node-metadata*` (no `:synchronized` flag)
- `*trail*`, `*var-counter*`, `*functor*`, `*select-list*` (global state)
- `*cache-enabled*` (global flag, benign but not transactional)

**Recommended approach:**
- Bind all mutable globals with `let` or macro
- Avoid direct `setf` on global state
- Define schema types before threading
- Let macros manage Prolog state isolation

---

## Edge Cases & Gotchas

### Gotcha 1: Hash-Table Not Thread-Safe Enough

**Problem:**

```lisp
SBCL: *schema-node-metadata* = (make-hash-table :test 'equal)
No `:synchronized t` flag!
```

Two threads can corrupt the hash-table:

```
Thread A: (setf (gethash type1 *schema-node-metadata*) metadata1)
Thread B: (setf (gethash type2 *schema-node-metadata*) metadata2)
Result: Both operations may race, corrupting internal structure
```

**Solution:**

```lisp
#+sbcl
  (make-hash-table :test 'equal :synchronized t)
#+ccl
  (make-hash-table :test 'equal :shared t)
#+lispworks
  (make-hash-table :test 'equal :single-thread nil)
```

---

### Gotcha 2: UUID Namespace Immutability Not Enforced

**Problem:**

```lisp
(setf *vertex-namespace* (uuid:uuid-to-byte-array
                          (uuid:make-uuid-from-string "NEW-UUID")))
```

Code can reassign namespace at runtime. Later vertex creation generates different UUIDs, breaking compatibility.

**Solution:**

1. Make namespace a constant (not defvar)
2. OR: Add validation that namespace unchanged after load
3. OR: Store namespace in graph metadata (version with graph)

---

### Gotcha 3: Type Codes Without Version

**Problem:**

Old saved graph has byte stream:

```
[type-code: 22] [data...]
```

In old code: type-code 22 = `+type-index+`

New code adds:
```
+new-type+ = 22  (reused!)
```

Reading old data interprets as new-type, not type-index.

**Solution:**

Add format version prefix to all serialized data:

```
[format-version: 1] [type-code: 22] [data...]
```

On load, check version before interpreting type codes.

---

### Gotcha 4: Prolog State Leakage Between Threads

**Problem:**

```lisp
Thread 1: (? (ancestor 'alice ?x))
  Modifies *trail* with binding [alice → ?x]

Thread 2: (? (parent 'bob ?y))
  Reads *trail* from Thread 1
  Wrong bindings used!
```

**Solution:**

```lisp
(let ((*trail* (make-array 200 :fill-pointer 0 :adjustable t)))
  (? (parent 'bob ?y)))
```

Create thread-local trail per query.

---

### Gotcha 5: +data-extent-size+ Too Large for Embedded Systems

**Problem:**

```
Line 30: +data-extent-size+ = 104,857,600 bytes = 100 MB

On embedded system with 256 MB RAM:
  2 extents = 200 MB (80% of RAM)
  Operation fails or swaps to disk
```

**Solution:**

Make configurable:

```lisp
(defparameter *data-extent-size* (* 1024 1024 100))

; User can override:
(let ((*data-extent-size* (* 1024 1024 10)))  ; 10 MB
  (with-graph (graph)
    ...))
```

---

### Gotcha 6: +max-node-types+ Silently Exceeded

**Problem:**

```
+max-node-types+ = 65536
User defines 100,000 types

Type ID = 100,000 mod 65536 = 34464
Collision with existing type!
```

**Solution:**

Add validation in `def-vertex`, `def-edge`:

```lisp
(when (>= (hash-table-count *schema-node-metadata*)
          +max-node-types+)
  (error "Type limit exceeded"))
```

---

### Gotcha 7: Prolog Occurs-Check Disabled by Accident

**Problem:**

```lisp
(setf *occurs-check* nil)  ; Disabled globally
; Now all unification allows cycles
(? (X = f(X)))  ; Succeeds (bad!)
```

**Solution:**

Bind locally:

```lisp
(let ((*occurs-check* nil))
  (? (X = f(X))))
```

---

## Integration Context

### Upstream Dependencies

| Dependency | Lines | Purpose |
|-----------|-------|---------|
| `:alexandria` | 5, 8-9, 13, etc. | `define-constant` macro |
| `:uuid` | 33-36 | UUID generation and conversion |
| `:graph-db` package | 1 | Defines symbols in package |

**Critical:** If `alexandria` or `uuid` libraries missing, load fails.

---

### Downstream Usage

| Consumer | What uses | Layer |
|----------|-----------|-------|
| `clos.lisp` | `*schema-node-metadata*`, `*graph-hash*` | Layer 1 |
| `utilities.lisp` | Constants (type codes, magic bytes, sizes) | Layer 1 |
| `conditions.lisp` | Error condition constants | Layer 1 |
| Layer 2+ files | All constants (indices, serialization) | Layer 2-7 |
| Layer 3+ files | `*graph*`, `*transaction*` | Layer 3+ |
| Layer 5+ files | `*prolog-global-functors*`, `*trail*` | Layer 5+ |
| User code | Exported constants and variables | External |

---

### Layer Position

**Layer 1 (Infrastructure & Base Utilities)** is where `globals.lisp` lives.

**Why Layer 1?** Because all Layer 2-7 files depend on these constants and globals. Must be defined first, before any other code can compile.

---

## When to Use What

### Decision Table 1: Accessing Constants vs. Variables

| Need | Use | Example |
|------|-----|---------|
| Magic byte value (compile-time) | Constant | `+data-magic-byte+` |
| Magic byte value (runtime) | Constant | Inlined by compiler |
| Current graph context | Variable `*graph*` | `(let ((*graph* g)) ...)` |
| Type definitions | Variable `*schema-node-metadata*` | Populated by `def-vertex` |
| Prolog state | Variable `*trail*`, `*var-counter*` | Used by `?` macro |
| Tunable flag | Parameter `*cache-enabled*` | `(let ((*cache-enabled* nil)) ...)` |

---

### Decision Table 2: Thread-Safety Per Use Case

| Use case | Approach | Rationale |
|----------|----------|-----------|
| Single-threaded app | Use constants directly | No concurrency risk |
| Multi-threaded, shared graph | Bind `*graph*` with `let` | Thread-local scope |
| Multi-threaded, Prolog queries | Bind `*trail*` per thread | Isolated backtracking |
| Define types at startup | Do once, before threads | Avoid concurrent registration |
| Prolog functor lookup | Use sync'd hash-table | Atomic operations |
| Caching control | Toggle `*cache-enabled*` per thread | Benign, isolated |

---

### Decision Table 3: Configuration Patterns

| Scenario | Change required | Method |
|----------|-----------------|--------|
| Adjust extent size per use | ✗ (hardcoded) | Make parameter, allow `let` binding |
| Change magic bytes | ✗ (impossible; breaks data) | Don't; data compatibility lost |
| Add new type code | ✗ (risky; needs versioning) | Add version check first |
| Register new Prolog predicate | ✓ (dynamic) | Call `def-global-prolog-functor` at startup |
| Disable caching | ✓ (dynamic) | Bind `*cache-enabled* = nil` |

---

## Summary

| Aspect | Status | Details |
|--------|--------|---------|
| **Core purpose** | ✅ Clear | Define all constants and global state for Layers 1-7 |
| **Constant completeness** | ✅ Complete | 50+ constants covering storage, serialization, indexes |
| **Global state coverage** | 🟡 Partial | 9 globals defined; some undocumented, some unsafe |
| **Thread safety** | 🔴 Unsafe | `*schema-node-metadata*` not sync'd; Prolog state not isolated |
| **Configuration** | ❌ Limited | Hardcoded values; no runtime configuration options |
| **Versioning** | ❌ Missing | Type codes, magic bytes, UUIDs not versioned |
| **Documentation** | ❌ Missing | No docstrings; constant meanings unclear |
| **Testing** | ❌ Missing | No tests for constant correctness, size assumptions |
| **Backward compatibility** | ⚠️ At risk | Changes to constants break all saved graphs; Phase 3 requires freeze |

---

### Key Insights

1. **globals.lisp is the configuration backbone.** Every constant here is critical to data compatibility.

2. **Mutable global state creates threading risks.** `*trail*`, `*var-counter*`, `*schema-node-metadata*` need synchronization or thread-local isolation.

3. **Type codes must be versioned.** Without version information, serialization format upgrades are impossible.

4. **UUID namespaces are permanent.** If changed, all stored graph IDs become invalid.

5. **Parameters should be configurable.** Hardcoded sizes like `+data-extent-size+` limit applicability.

6. **Thread-safe hash-tables are conditional per Lisp.** Code is portable but requires correct Lisp-specific flags.

7. **Constants are compile-time knowledge.** All layers depend on these; changes require full recompilation and data migration.

---

### Critical Decision: Phase 3 Readiness

**Before Phase 3 (API Stability), must:**

1. ☐ **Add docstrings** to all constants explaining purpose, unit, and dependency
2. ☐ **Fix thread-safety** of `*schema-node-metadata*` (add `:synchronized` per Lisp)
3. ☐ **Version serialization** type codes (add format version prefix)
4. ☐ **Make sizes configurable** (defparameter instead of constant)
5. ☐ **Document which globals are stable** and which are internal
6. ☐ **Provide Prolog state isolation** (thread-local binding of `*trail*`, etc.)

---

**Status:** Specification complete.  
**Blocking issues identified:** 2 (thread-safety, type versioning)  
**Ready for Nivel 3 (Docstrings & Inline Comments):** YES (after blocking issues documented)  
**Ready for Nivel 4 (Execution Mental / Diagrams):** YES (after Nivel 3)  
**Next action:** Create layer1-globals-docstrings.lisp with comprehensive annotations

