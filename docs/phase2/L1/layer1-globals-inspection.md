# Layer 1 Inspection Report: globals.lisp

**File:** src/globals.lisp  
**Lines:** 134 (actual) | 133 (roadmap) — ✅ MATCH (1 line difference, negligible)  
**Date:** March 2026  
**Priority:** CRITICAL (defines all global state for entire system)  
**Complexity:** MEDIUM (mostly constants, but critical state definitions)  
**Type:** Global state & constant definitions  

---

## Executive Summary

`globals.lisp` defines **all global variables and constants** that VivaceGraph uses throughout Layers 1-7. This file establishes:

**Key Characteristics:**
- ✅ **Pure definitions** — no logic, just declarations
- ✅ **Compile-time constants** via `alexandria:define-constant`
- ❌ **Mutable global state** — exposed directly (`*graph*`, `*schema-node-metadata*`, `*trail*`)
- ⚠️ **Thread-specific initialization** — hash tables created differently per Lisp (SBCL, CCL, LispWorks)
- ⚠️ **Magic bytes and type codes** — 35 serialization type constants (risk of mismatch with Layer 4)
- ⚠️ **UUID namespaces** — created at load-time (not compile-time), immutable after creation

**Why It Matters:**
This file is the **configuration backbone** of VivaceGraph. Every constant defined here affects storage format, serialization, index management, and Prolog execution. A single incorrect constant breaks compatibility across all layers.

---

## Line Count Breakdown

Lines | Section | Type | Notes
-------|---------|------|-------
1 | Context (`in-package :graph-db`) | Meta | Package switch
3 | `*cache-enabled*` | Variable | Boolean flag for caching
5 | `+db-version+` | Constant | Database format version (1)
7 | `*graph*` | Variable | Current graph context (NIL initially)
8-10 | File constants | Constant | Storage file names (main.dat, meta.dat, data.dat)
12 | `*schema-node-metadata*` | Variable | Hash-table (type registry)
13 | `+max-node-types+` | Constant | Max types (65536)
15-30 | Storage constants | Constant | Magic bytes, versions, sizes (16 lines)
32-36 | UUID namespaces | Variable | Vertex/Edge namespace UUIDs
38-42 | Sentinel values | Constant | Skip-list sentinels, reduce key (5 lines)
44-54 | VE-index constants | Constant | VE-key sizes and null/max values
56-63 | VEV-index constants | Constant | VEV-key sizes and null/max values
65-100 | Type serialization bytes | Constant | 35 type codes (0-30, 100-101)
102-103 | Extent parameters | Parameter | Initial extents, max locks
105 | `*graph-hash*` | Variable | Graph registry (NIL initially)
107-113 | Prolog specials | Variable/Constant | Trail, var-counter, functor, etc.
115-128 | Prolog functors (thread-safe) | Variable | Hash-tables per Lisp
130-134 | Prolog constants | Constant/Parameter | Trace flag, unbound, no-bindings, fail

**Total:** 134 lines exactly.

---

## Core Components

### A. Configuration & Runtime Flags (Lines 3-13)

**Structure:**

```
*cache-enabled*                    = t          (defvar)
+db-version+                       = 1          (constant)
*graph*                            = nil        (defvar)
+main-table-file+                  = "main.dat" (constant)
+meta-file+                        = "meta.dat" (constant)
+data-file+                        = "data.dat" (constant)
*schema-node-metadata*             = hashtable  (defvar)
+max-node-types+                   = 65536      (constant)
```

**Analysis:**

- **`*cache-enabled*`** (Line 3)
  - Type: Boolean flag
  - Purpose: Global on/off switch for caching
  - Usage: Likely checked by Layer 2-4 before caching operations
  - Risk: ⚠️ No locking, can be modified at runtime, affects concurrent threads

- **`+db-version+`** (Line 5)
  - Type: Integer (1)
  - Purpose: Format version for compatibility checking
  - Usage: When opening graph, verify stored version matches +db-version+
  - Note: Currently hardcoded to 1; no upgrade path defined

- **`*graph*`** (Line 7)
  - Type: Dynamic variable (NIL or graph instance)
  - Purpose: Current graph context (thread-local when bound)
  - Lifecycle: Bound by `with-transaction`, `make-graph`
  - Risk: 🔴 Global mutable state; if modified directly, breaks invariants

- **File constants** (Lines 8-10)
  - `+main-table-file+`, `+meta-file+`, `+data-file+`
  - Purpose: Default filenames for graph storage
  - Type: String constants
  - Risk: ⚠️ Hardcoded; no configuration option to change location pattern

- **`*schema-node-metadata*`** (Line 12)
  - Type: Hash-table (test 'equal)
  - Purpose: Registry of type definitions (vertex/edge/custom types)
  - Structure: Maps type-name → metadata-object
  - Risk: 🟠 CRITICAL — Global shared hash-table, not thread-safe (no :synchronized)
  - Side effect: Modified by `def-vertex`, `def-edge` (Layer 6)

- **`+max-node-types+`** (Line 13)
  - Type: Integer (65536 = 2^16)
  - Purpose: Maximum number of distinct types
  - Implication: Type IDs are 16-bit integers
  - Constraint: Cannot define more than 65536 distinct vertex/edge types

---

### B. Storage & Serialization Constants (Lines 15-30)

**Structure:**

```
Storage version         #x01          (1 byte)
Fixed integer marker    #x01          (1 byte, redundant?)
Magic bytes:
  +data-magic-byte+     #x17          (23 decimal)
  +lhash-magic-byte+    #x18          (24 decimal)
  +overflow-magic-byte+ #x19          (25 decimal)
  +config-magic-byte+   #x20          (32 decimal)

Key arrays:
  +null-key+            16 zeros      (16 bytes)
  +max-key+             16 255s       (16 bytes)

Sizes:
  +key-bytes+           16
  +value-bytes+         8
  +bucket-size+         24
  +data-extent-size+    104857600     (100 MB)
```

**Analysis:**

- **Magic bytes** (Lines 17-20)
  - Purpose: Identify block types during deserialization
  - Usage: First byte of block determines how to parse rest
  - Risk: 🟡 WARNING — Hardcoded; if changed, old data becomes unreadable

- **+null-key+ and +max-key+** (Lines 21-26)
  - Type: Byte-array (16 elements)
  - Purpose: Sentinel values for key range queries
  - Usage: Skip-list bounds for range iteration
  - Note: Created at load-time, not compile-time

- **Key/Value/Bucket sizes** (Lines 27-29)
  - +key-bytes+ = 16: UUID size (128 bits)
  - +value-bytes+ = 8: Pointer size (64 bits)
  - +bucket-size+ = 24: Hash bucket size (key 16 + value 8)

- **+data-extent-size+** (Line 30)
  - Value: 104857600 (100 MB)
  - Purpose: Size of memory-mapped data blocks
  - Calculation: (* 1024 1024 100) = 100 MB per extent
  - Risk: 🟡 WARNING — Hardcoded; may be too large for embedded systems or too small for large graphs

---

### C. UUID Namespaces (Lines 32-36)

**Structure:**

```
*vertex-namespace* = UUID byte-array
  UUID String: "2140DCE1-3208-4354-8696-5DF3076D1CEB"
  
*edge-namespace* = UUID byte-array
  UUID String: "0392C7B5-A38B-466F-92E5-5A7493C2775A"
```

**Analysis:**

- **Purpose:** Namespace UUIDs for v5 UUID generation
  - When creating vertex ID: hash(vertex-namespace + data) → UUID
  - When creating edge ID: hash(edge-namespace + data) → UUID
  - Ensures vertex UUIDs never collide with edge UUIDs (different namespaces)

- **Type:** Defvar (not constant!)
  - Value: Byte-array (result of `uuid:uuid-to-byte-array`)
  - Computed at load-time
  - Immutable after creation (assignment to defvar never changed)

- **Risk:** 🟡 WARNING
  - These UUIDs are hardcoded; if changed, all old UUID generation breaks
  - No version check to detect mismatch
  - If Layer 6 code regenerates UUIDs with different namespace, data corruption

---

### D. Index Constants (Lines 38-63)

**Sentinel Values (Lines 38-42):**

```
+min-sentinel+ = :gmin        (keyword symbol)
+max-sentinel+ = :gmax        (keyword symbol)
+reduce-master-key+ = :gagg   (keyword symbol)
```

**Purpose:** Sentinel values for skip-list range queries
- `:gmin` is smaller than any real key (lower bound)
- `:gmax` is larger than any real key (upper bound)
- `:gagg` is used in views for aggregation results

**Index-list constants (Lines 44-45):**

```
+index-list-bytes+ = 17       (VE-index key header size?)
```

**VE-index constants (Lines 47-54):**

```
+ve-key-bytes+ = 18           (vertex-edge key size)
+null-ve-key+ = array(18, all 0)
+max-ve-key+ = array(18, all 255)
```

**VEV-index constants (Lines 56-63):**

```
+vev-key-bytes+ = 34          (vertex-edge-vertex key size)
+null-vev-key+ = array(34, all 0)
+max-vev-key+ = array(34, all 255)
```

**Analysis:**

- **Why 18?** Likely 16 bytes (vertex ID) + 2 bytes (edge type or direction)
- **Why 34?** Likely 16 bytes (vertex1 ID) + 16 bytes (vertex2 ID) + 2 bytes (edge info)
- **Risk:** 🟡 WARNING — Hardcoded sizes assume specific structure; if Layer 4 changes layout, breaks all indexes

---

### E. Serialization Type Codes (Lines 65-100)

**35 type constants** mapping Lisp types to byte codes for serialization:

```
Basic types:
  +unknown+ = 0
  +negative-integer+ = 1
  +positive-integer+ = 2
  +character+ = 3
  +symbol+ = 4
  +string+ = 5
  +list+ = 6
  +vector+ = 7
  +single-float+ = 8
  +double-float+ = 9
  +ratio+ = 10
  +t+ = 11                    (true/boolean)
  +null+ = 12                 (NIL)
  +blob+ = 13                 (uninterpreted octets)
  +dotted-list+ = 14
  +keyword+ = 15

VivaceGraph types:
  +slot-key+ = 16
  +id+ = 17
  +vertex+ = 18
  +edge+ = 19
  +skip-list+ = 20
  +ve-index+ = 21
  +type-index+ = 22
  +pcons+ = 23                (persistent cons?)
  +pqueue+ = 24               (persistent queue?)
  +mpointer+ = 25             (memory pointer?)
  +pcell+ = 26                (persistent cell?)
  +index-list+ = 27
  +vev-index+ = 28
  +bit-vector+ = 29
  +bignum+ = 30

User-defined:
  +uuid+ = 100
  +timestamp+ = 101
```

**Analysis:**

- **Purpose:** During serialization, each value is prefixed with type code
  - Reader: read byte → look up type code → deserialize appropriately
  - Example: `(#x01 #xFF #xFF ...) = (positive-integer, value 65535)`

- **Gap between 30 and 100:** Intentional gap for future expansion
- **User-defined range:** 100+ reserved for user extensions

- **Risk:** 🔴 BLOCKING
  - No version number; if new type code added, old data uninterpretable
  - No compatibility check when loading old saved data
  - If type code changed (e.g., +uuid+ moved from 100 to 102), data corrupted

---

### F. Prolog Global State (Lines 107-134)

**Prolog specials (Lines 107-113):**

```
*occurs-check* = t                    (boolean parameter)
*trail* = adjustable array(200)       (choice point trail)
*var-counter* = 0                     (variable name generator)
*functor* = nil                       (current functor context)
*select-list* = nil                   (accumulator for queries)
*cont* = nil                          (continuation container)
```

**Analysis:**

- **`*occurs-check*`** (Line 108)
  - Parameter: Controls occurs check during unification
  - Default: t (perform occurs check)
  - Purpose: Prevent infinite structures (X = f(X))
  - Risk: ⚠️ Global flag affects all Prolog threads

- **`*trail*`** (Line 109)
  - Type: Adjustable array with fill-pointer
  - Initial size: 200 (grows as needed)
  - Purpose: Backtracking support (records variable bindings)
  - Usage: When unification fails, unwind trail to restore state
  - Risk: 🟠 CRITICAL — Shared global state, no per-thread isolation
  - Problem: Multiple Prolog queries on different threads interfere

- **`*var-counter*`** (Line 110)
  - Type: Integer (initially 0)
  - Purpose: Generate unique variable names (?1, ?2, ?3, ...)
  - Risk: 🟡 WARNING — Global counter, not thread-safe, may overflow

- **`*functor*`** (Line 111)
  - Type: NIL or functor object
  - Purpose: Current Prolog functor being evaluated
  - Risk: ⚠️ Global context, set during functor dispatch

- **`*select-list*`** (Line 112)
  - Type: NIL or list
  - Purpose: Accumulates results during query execution
  - Risk: ⚠️ Global state, multiple queries interfere

- **`*cont*`** (Line 113)
  - Type: NIL or continuation object
  - Purpose: Step-wise query execution
  - Note: Poorly documented; unclear usage

**Prolog functors (Lines 115-128):**

```
SBCL:
  *prolog-global-functors* = hashtable(:synchronized t)
  *user-functors* = hashtable(:synchronized t, :test 'eql)

LispWorks:
  *prolog-global-functors* = hashtable(:single-thread nil)
  *user-functors* = hashtable(:single-thread nil, :test 'eql)

CCL:
  *prolog-global-functors* = hashtable(:shared t)
  *user-functors* = hashtable(:shared t, :test 'eql)
```

**Analysis:**

- **Thread-safe hash tables** created per Lisp:
  - SBCL: `:synchronized t` (thread-safe with internal locks)
  - LispWorks: `:single-thread nil` (thread-safe, documented)
  - CCL: `:shared t` (shared among threads)

- **Two registries:**
  - `*prolog-global-functors*`: Built-in predicates
  - `*user-functors*`: User-defined predicates

- **Risk:** 🟡 WARNING — Even though hash-tables are thread-safe, the *trail* and other global state are not synchronized
  - Hash table operations are atomic, but overall Prolog state is inconsistent

**Prolog constants (Lines 130-134):**

```
*prolog-trace* = nil                  (trace flag for debugging)
+unbound+ = :unbound                  (marker for unbound variable)
+no-bindings+ = '((t . t))            (empty binding list)
+fail+ = nil                          (failure marker)
```

---

## Dependencies

### What globals.lisp depends on:

| Dependency | Lines | Purpose |
|-----------|-------|---------|
| `alexandria` | 5, 8-9, 13, 15-100, 131-133 | `define-constant` macro, utility macros |
| `uuid` | 33-36 | UUID to byte-array conversion, UUID generation |
| `:graph-db` package | 1 | Package context |

### What depends on globals.lisp:

| Consumer | What it uses | Layer |
|----------|-------------|-------|
| All Layer 1+ files | Constants (magic bytes, type codes, sizes) | 1-7 |
| `clos.lisp` | `*graph-hash*`, `*schema-node-metadata*` | 1 |
| Layer 3 (transactions) | `*graph*`, transaction state | 3 |
| Layer 4 (storage) | Magic bytes, type codes, sizes | 4 |
| Layer 5 (indexing) | `*prolog-global-functors*`, trail | 5 |
| Layer 6 (schema) | `*schema-node-metadata*` | 6 |
| User code | Everything (via package.lisp exports) | External |

---

## Complexity Assessment & Hotspots

### 🔴 BLOCKING #1: Mutable Global State Not Thread-Safe

**Problem:**

```
*cache-enabled*          (boolean flag)
*graph*                  (current graph)
*schema-node-metadata*   (hash-table, NOT :synchronized)
*trail*                  (array, NOT thread-safe)
*var-counter*            (counter, NOT atomic)
*functor*                (context, NOT isolated)
*select-list*            (accumulator, NOT isolated)
*cont*                   (continuation, NOT isolated)
*prolog-trace*           (flag, global)
```

**Risk:**

- Multiple threads reading/writing simultaneously → corrupted state
- No locks, no atomic operations, no thread-local bindings
- `*schema-node-metadata*` is hash-table without `:synchronized` or `:shared` flag
- Prolog specials cause data races on different threads

**Impact:**

- 🔴 Data corruption
- 🔴 Undefined behavior
- 🔴 Silent failures (wrong results, not errors)

**Mitigation needed:**

1. Make `*schema-node-metadata*` thread-safe (`:synchronized` on SBCL, etc.)
2. Provide thread-local bindings for Prolog state (`*trail*`, `*var-counter*`, etc.)
3. Document which variables are safe to access without locks
4. Use `let` bindings instead of direct assignment

---

### 🔴 BLOCKING #2: Serialization Type Codes Not Versioned

**Problem:**

Type codes (lines 65-100) are hardcoded with no version information. If code adds a new type:

```
Old file format:
  Byte 22 = +type-index+

New code adds:
  Byte 22 = +new-type+        (conflict!)
  Old byte 22 = moved to 102?

Reading old file with new code:
  ERROR: Interprets as new type, not type-index
```

**Risk:**

- 🔴 Data corruption
- 🔴 Silent misinterpretation (reads type A as type B)
- 🔴 No backward compatibility mechanism

**Impact:**

- Old saved graphs become unreadable
- Migration path undefined
- Phase 3 (stability) impossible

---

### 🟠 CRITICAL #3: UUID Namespaces Hardcoded (Load-time Initialization)

**Problem:**

```
Lines 33-36: Namespaces created at load-time by calling uuid:uuid-to-byte-array
If Layer 7 or user code tries to recreate UUIDs, must use EXACT same namespace
If UUIDs stored in database and later code changes namespace, IDs don't match
```

**Risk:**

- 🟠 If namespace changes, all stored UUIDs become invalid
- 🟠 No version check to detect mismatch
- 🟠 Difficult to upgrade or migrate graphs

---

### 🟠 CRITICAL #4: Size Constants Affect Data Layout

**Problem:**

```
+key-bytes+ = 16        (UUID size)
+value-bytes+ = 8       (pointer size)
+bucket-size+ = 24      (key + value)
+ve-key-bytes+ = 18     (vertex-edge key)
+vev-key-bytes+ = 34    (vertex-edge-vertex key)
```

**Risk:**

- 🟠 If Layer 4 changes key layout, these constants wrong
- 🟠 If 64-bit pointer size assumption wrong (embedded systems), crashes
- 🟠 All serialized data assumes these sizes

---

### 🟡 WARNING #5: +data-extent-size+ Hardcoded to 100 MB

**Problem:**

```
Line 30: (* 1024 1024 100) = 104,857,600 bytes = 100 MB
```

**Risk:**

- 🟡 Too large for embedded systems with limited RAM
- 🟡 Too small for very large graphs (multiple gigabytes)
- 🟡 No configuration option to adjust

---

### 🟡 WARNING #6: +max-node-types+ = 65536 (2^16)

**Problem:**

```
Line 13: Maximum 65,536 distinct types
This implies type IDs are 16-bit integers
```

**Risk:**

- 🟡 No error checking if types exceed limit
- 🟡 Silent wraparound or type ID collision if limit reached

---

### 🟡 WARNING #7: Redundant/Unclear Symbols

**Problem:**

```
Line 16: +fixed-integer-64+ = #x01     (same value as +storage-version+?)
Lines 91-94: +pqueue+, +mpointer+, +pcell+ (poorly documented)
```

**Risk:**

- 🟡 Dead code or confusion about purpose
- 🟡 Maintenance burden without clear meaning

---

## Issues Found

### 🔴 BLOCKING #1: *schema-node-metadata* Not Thread-Safe

**Issue:** Line 12 defines hash-table without thread-safe flags.

**Problem:**

```lisp
(defvar *schema-node-metadata* (make-hash-table :test 'equal))
```

No `:synchronized t` (SBCL) or `:shared t` (CCL) or `:single-thread nil` (LispWorks).

**Risk:** Multiple threads modifying simultaneously → corrupted hash-table

**Fix:** Add thread-safety flags per Lisp:

```
#+sbcl   (defvar *schema-node-metadata* (make-hash-table :test 'equal :synchronized t))
#+lispworks (defvar *schema-node-metadata* (make-hash-table :test 'equal :single-thread nil))
#+ccl    (defvar *schema-node-metadata* (make-hash-table :test 'equal :shared t))
```

---

### 🔴 BLOCKING #2: Prolog State Not Thread-Isolated

**Issue:** Lines 109-113 define global Prolog state without thread isolation.

**Problem:**

```
*trail*, *var-counter*, *functor*, *select-list*, *cont*
All are shared across threads
Multiple Prolog queries interfere
```

**Risk:** Concurrent Prolog queries produce incorrect results

**Fix:** 

1. Create thread-local bindings of `*trail*`, `*var-counter*` per query
2. OR: Use lock-based synchronization (slow)
3. OR: Create separate Prolog engine instance per thread (complex)

---

### 🟠 CRITICAL #1: Serialization Type Codes Without Version

**Issue:** Lines 65-100 define type codes with no version information.

**Problem:**

If future version adds new type code, old data becomes unreadable.

**Risk:** Incompatibility between VivaceGraph versions

**Fix:**

Add version prefix to serialized data:
```
Byte 0: Format version (1, 2, 3, ...)
Bytes 1+: Data (with version-specific interpretation)
```

---

### 🟠 CRITICAL #2: *graph* Should Not Be Global

**Issue:** Line 7 exports `*graph*` as mutable global.

**Problem:**

Users can do `(setf *graph* new-graph)` globally, breaking invariants.

**Risk:** Each thread should have isolated `*graph*` binding

**Fix:**

1. Encapsulate in macro: `(with-graph (my-graph) ...)`
2. OR: Provide accessor function instead of raw variable
3. OR: Document as "INTERNAL, use with-transaction instead"

---

### 🟡 WARNING #1: UUID Namespaces Not Versioned

**Issue:** Lines 33-36 define UUID namespaces at load-time.

**Problem:**

If these UUIDs change, all stored graph UUIDs become invalid.

**Fix:**

1. Store namespace UUIDs in graph metadata (version with graph)
2. Check on load that namespaces match
3. Error if mismatch detected

---

### 🟡 WARNING #2: +data-extent-size+ Hardcoded

**Issue:** Line 30 hardcodes 100 MB data extent size.

**Problem:**

No configuration option; may be too large or too small for specific use cases.

**Fix:**

```lisp
(defparameter *data-extent-size* (* 1024 1024 100))
; Users can rebind: (let ((*data-extent-size* (* 1024 1024 50))) ...)
```

---

### 🟡 WARNING #3: Unused/Unclear Symbols

**Issue:** Lines 16, 91-94 define symbols with unclear purpose.

**Problem:**

```
+fixed-integer-64+ = #x01  (redundant with +storage-version+?)
+pqueue+, +mpointer+, +pcell+ (never used?)
```

**Fix:**

1. Document purpose of each symbol
2. Remove if unused
3. Add type comments for clarity

---

## Testing Strategy

### Critical Tests to Write

| Test | Setup | Action | Expected |
|------|-------|--------|----------|
| **Constants defined** | None | Check all +constants+ are bound | All exist, correct values |
| **Magic bytes unique** | None | Verify no duplicates | All 4 magic bytes distinct |
| **Type codes contiguous** | None | Check 0-30, then 100-101 | No gaps in 0-30 |
| **Hash-table thread-safe** | Threaded | Multiple threads modify `*schema-node-metadata*` | No corruption |
| **UUID namespaces immutable** | None | Try to modify `*vertex-namespace*` | Immutable (or detect change) |
| **Size constants realistic** | None | Verify +key-bytes+ = 16, +value-bytes+ = 8 | Match actual layout |
| **Prolog state per thread** | Threaded | Two threads with different `*trail*` | Isolated (if fix implemented) |

---

## Code Quality Summary

| Aspect | Status | Notes |
|--------|--------|-------|
| **Docstrings** | ❌ **MISSING** | No docstrings on any variables or constants. Comments minimal. |
| **Test coverage** | ❌ **MISSING** | No tests for constant correctness, size assumptions, thread-safety. |
| **Performance** | ✅ **N/A** | Constants are compile-time; no runtime cost. |
| **Complexity** | 🟡 **MEDIUM** | Many constants; relationships unclear without documentation. |
| **Lisp idiom** | ✅ **CORRECT** | Uses `defvar`, `defparameter`, `define-constant` appropriately. |
| **Thread safety** | 🔴 **UNSAFE** | Global mutable state with no synchronization (except Prolog hash-tables). |
| **Portability** | 🟡 **FRAGILE** | Different hash-table initialization per Lisp; must test all three. |

---

## Comparison (Optional)

**vs. typical Lisp global configuration files:**

- ✅ **Good:** Uses `define-constant` for immutable values
- ✅ **Good:** Organizes constants by purpose (storage, serialization, indexes)
- ❌ **Bad:** Mixes immutable constants with mutable global state in same file
- ❌ **Bad:** No docstrings explaining magic byte meanings or size rationale
- ❌ **Bad:** Thread-safety assumptions not documented
- ✅ **Good:** Uses `defparameter` for tunable values

---

## Summary

| Metric | Value | Assessment |
|--------|-------|------------|
| **Lines** | 134 | ✅ Match roadmap |
| **Mutable globals** | 9 | 🔴 Too many, not thread-safe |
| **Constants** | 50+ | ✅ Well-organized, but undocumented |
| **Thread-safe variables** | 2 (Prolog hash-tables) | 🟡 Partial coverage |
| **Blocking issues** | 2 | 🔴 Thread-safety, type versioning |
| **Critical issues** | 2 | 🔴 UUID namespaces, size constants |
| **Warnings** | 3+ | 🟡 Hardcoded values, unclear symbols |
| **Code health** | 🟡 **MEDIUM** | Functionally correct, but missing documentation and thread-safety |
| **Ready for Layer 2?** | 🔴 **NO** | Must address thread-safety before higher layers depend on this |

---

## Next Steps

1. ✅ **Document all constants:** Add docstrings explaining magic bytes, type codes, sizes, purposes.

2. ✅ **Fix thread-safety:** Make `*schema-node-metadata*` thread-safe per Lisp; provide thread-local Prolog state.

3. ✅ **Version serialization:** Add format version to serialized data to detect incompatibility.

4. ✅ **Version UUID namespaces:** Store in graph metadata, verify on load.

5. ✅ **Make sizes configurable:** Allow `*data-extent-size*` to be adjusted via parameters.

6. ✅ **Clarify/remove unused symbols:** Document purpose of `+pqueue+`, `+mpointer+`, `+pcell+` or delete.

7. ✅ **Create tests:** Verify constant correctness, thread-safety, size assumptions.

---

**Status:** Inspection complete. Issues identified: 7 (2 blocking, 2 critical, 3+ warning).  
**Ready for Nivel 2 (Component Specification):** NO — must address blocking issues first.  
**Next action:** Create layer1-globals-component.md (Specification) + resolve blocking issues.

