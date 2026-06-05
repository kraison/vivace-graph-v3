# Layer 1 Component Specification: package.lisp

**Component:** VivaceGraph Package Definition & Public API Contract  
**File:** src/package.lisp  
**Lines:** 188  
**Type:** Interface (pure namespace configuration + public API contract)  
**Purpose:** Define the :graph-db package namespace and enumerate all 166 publicly-accessible symbols  
**Scope:**
- ✓ Defines namespace binding
- ✓ Exports public symbols across 6 functional domains
- ✓ Normalizes MOP across SBCL, CCL, LispWorks
- ✗ Does NOT implement any functionality
- ✗ Does NOT validate symbol correctness

---

## Conceptual Model

### Core Idea

**`package.lisp` is the API contract between VivaceGraph internals and external consumers.**

It serves two purposes:
1. **Namespace isolation:** All VivaceGraph code lives in `:graph-db` package, not in user's namespace
2. **API enumeration:** Lists every symbol a user is allowed to call (the "public surface")

### Motivation

Without `package.lisp`:
- ❌ All symbols would pollute `:cl-user` or user's package
- ❌ Name collisions with other libraries (e.g., multiple graph libraries define `vertex`)
- ❌ No clear boundary between "stable API" and "internal implementation"
- ❌ Cross-Lisp MOP differences would require users to handle conditionals

With `package.lisp`:
- ✅ Clean namespace (`:graph-db` is isolated)
- ✅ Explicit API surface (users know what's public)
- ✅ Cross-Lisp compatibility abstracted away (package handles MOP differences)

### Abstraction Boundary

**What it exposes:**
- 166 symbols organized into 6 functional domains (Graph, TX, Schema, CRUD, Views, Prolog)
- Global dynamic variables (`*graph*`, `*transaction*`, etc.)
- MOP access (via `:closer-mop`, `:sb-mop`, `:clos`)

**What it hides:**
- Internal implementation details (e.g., Layer 2-7 files)
- SBCL/CCL/LispWorks specific MOP variants (shadows normalize them)
- Thread-safety mechanisms (exposed via public macros, not primitives)

**What it should hide but currently doesn't:** ⚠️
- Prolog internals (51 symbols, including `*trail*`, `*var-counter*`, `*functor*`)
- Internal global state (e.g., `*schema-node-metadata*` is exposed but should be internal)

### Key Properties

Property | Value | Rationale
----------|-------|----------
**Namespace isolation** | ✅ Complete | All VivaceGraph code in `:graph-db`
**API visibility** | ⚠️ Partial | 166 exports, no separation of stable/experimental
**Cross-Lisp compatibility** | 🟡 Fragile | Works but MOP shadows create coupling risk
**Symbol uniqueness** | ✅ Assumed | No tests verify uniqueness
**Global state encapsulation** | ❌ Missing | Raw `*graph*`, `*transaction*` exposed
**Backward compatibility** | ⚠️ At risk | Phase 3 will require freezing these 166 symbols forever

---

## Interface Definition

### Operation 1: Package Definition

**Signature:**

Defpackage graph-db with conditional imports of base libraries (CL, bordeaux-threads, local-time) and MOP implementations (SBCL, CCL, LispWorks specific).

**Semantics:**
- **:use dependencies:**
  - `:cl` — Standard Common Lisp (always available)
  - `:bordeaux-threads` — Cross-platform thread primitives
  - `:local-time` — Date/time handling library
  - `:closer-mop` (CCL only) — Portable MOP for CCL
  - `:clos` (LispWorks only) — Native CLOS for LispWorks
  - `:sb-mop` (SBCL only) — SBCL's MOP implementation
  - `:sb-pcl` (SBCL only) — SBCL's portable common loops (legacy, may be redundant)

**Returns:**
- Package object with 166 exported symbols
- Compilation-time effect: creates `:graph-db` namespace

**Edge Cases:**
1. **Lisp implementation mismatch:**
   - User loads in SBCL → gets SBCL MOP
   - User loads in CCL → gets CLOSER-MOP
   - If CLOSER-MOP not installed on CCL → compile error on lines 7, 12-22
   - If `bordeaux-threads` not installed → compile error on line 5

2. **Symbol name collision:**
   - If user's package also defines `vertex`, `edge`, etc. → must use package prefix
   - Example: `(graph-db:vertex ...)` vs. `(my-package:vertex ...)`

3. **Conditional compilation order:**
   - File must be compiled with correct `*features*` set
   - `#+sbcl` blocks ignored on CCL/LispWorks
   - `#+ccl` blocks ignored on SBCL/LispWorks

**Performance:**
- **Time:** O(1) at compile-time (package definition is static)
- **Space:** O(166) symbols in package hashtable (negligible)

---

### Operation 2: Shadowing Imports

**Signature (SBCL):** Conditionally import WORD from SB-EXT

**Semantics:**
- Import symbol `WORD` from `SB-EXT` module
- "Shadow" means: if `:cl` also exports `WORD`, use the `SB-EXT` version instead
- **Risk:** This import is **NOT EXPORTED**, so external code cannot see it

**Returns:**
- `WORD` is now available in `:graph-db` package for internal code to use

**Edge Cases:**
1. **Unused symbol:** If code never uses `WORD`, the import wastes a line (dead code)
2. **Breaking change:** If future SBCL version removes `WORD` → compile error

---

**Signature (CCL):** Conditionally import 11 MOP symbols from CLOSER-MOP

**Semantics:**
- Import 11 MOP-related symbols from `CLOSER-MOP` library
- Each shadows conflicting symbols in native CCL CLOS
- Goal: normalize CCL's MOP to behave like SBCL/standard MOP

**Returns:**
- `defmethod`, `defgeneric`, `standard-generic-function`, etc. now use CLOSER-MOP semantics on CCL
- Ensures consistent metaclass behavior across Lisps

**Edge Cases:**
1. **CLOSER-MOP version skew:** If CLOSER-MOP version differs, shadowing may fail silently
2. **Semantic divergence:** CLOSER-MOP may not perfectly match native CLOS behavior
3. **Missing shadowing:** If user adds new MOP usage but forgets to shadow it → hidden inconsistency

---

### Operation 3: Export Symbol (Generic)

**Signature (repeated 166 times):** Export individual symbol names

**Semantics:**
- Each symbol makes that symbol **publicly accessible** to external code
- External code can write: `(graph-db:symbol-name ...)`
- Or with `(use-package :graph-db)`: `(symbol-name ...)`

**Guarantee:**
- `(find-symbol "MAKE-GRAPH" :graph-db)` returns the symbol
- `(find-symbol "MAKE-GRAPH" :cl-user)` returns `NIL` (not polluted)

**Edge Cases:**
1. **Duplicate exports:** If `:make-graph` appears twice → error at load time
2. **Symbol does not exist:** If implementation file never defines `make-graph` → runtime error when user calls it, but package definition succeeds
3. **Re-export confusion:** User imports `:graph-db` but `make-graph` references undefined symbol → error at compile-time of user code, not at package definition time

---

## Variants / Specializations

VivaceGraph's package definition has **3 variants** depending on Lisp implementation:

### Variant A: SBCL (lines 9-10, 11)

**Characteristics:**
- Uses SBCL's native MOP (`sb-mop`, `sb-pcl`)
- Shadows `WORD` from `SB-EXT` (SBCL-specific extension library)
- Exports RW-lock primitives (lines 77-84)

**Differences from others:**
- Only variant that exports low-level RW-lock functions
- No CLOSER-MOP dependency
- Direct access to SBCL internals (SB-EXT)

**Use Cases:**
- When running VivaceGraph on SBCL
- When performance-critical code needs access to SBCL-specific features (e.g., `sb-ext:word`)

---

### Variant B: CCL (lines 7, 12-22)

**Characteristics:**
- Uses `CLOSER-MOP` (portable MOP library) instead of native CCL CLOS
- Heavy shadowing (11 symbols) to normalize MOP behavior
- Does NOT export RW-lock primitives

**Differences from others:**
- Largest set of shadowing imports (11 symbols)
- Depends on external library (CLOSER-MOP)
- Relies on CLOSER-MOP correctness for MOP semantics

**Use Cases:**
- When running VivaceGraph on CCL (Clozure CL)
- When portable MOP semantics are critical

---

### Variant C: LispWorks (lines 8)

**Characteristics:**
- Uses LispWorks' native `#:clos` module
- Minimal shadowing (none)
- Does NOT export RW-lock primitives

**Differences from others:**
- Simplest variant (no shadowing, no extra dependencies)
- Assumes LispWorks' CLOS is standard-compliant
- No explicit MOP module (CLOS is standard)

**Use Cases:**
- When running VivaceGraph on LispWorks
- When running on LispWorks with expected CLOS semantics

---

## Usage Patterns

### Pattern 1: Using VivaceGraph in User Code

**Context:** User writes code that imports VivaceGraph functionality.

**Example:**

User at REPL:
```
(in-package :my-application)
(use-package :graph-db)
(let ((g (make-graph "mydb" :location "/tmp/mydb")))
  (with-transaction
    (let ((v (make-vertex g "Person" '(("name" . "Alice")))))
      (save v))))
```

**How it works:**
1. `(use-package :graph-db)` makes all 166 exported symbols available in `:my-application` package
2. `(make-graph ...)` resolves to `graph-db:make-graph`
3. No package prefix needed because symbols are imported

**Notes:**
- ✅ Cleaner than `graph-db:make-graph` everywhere
- ❌ Pollutes user's namespace (166 new symbols)
- ⚠️ Name collisions if user also defines `vertex`, `edge`, etc.

---

### Pattern 2: Selective Import (Package Prefix)

**Context:** User wants to avoid namespace pollution.

**Example:**

```
(in-package :my-application)
(let ((g (graph-db:make-graph "mydb")))
  (graph-db:with-transaction
    (let ((v (graph-db:make-vertex g "Person" ...)))
      (graph-db:save v))))
```

**How it works:**
1. Each symbol accessed via `graph-db:symbol-name` prefix
2. User's package stays clean
3. VivaceGraph symbols explicitly scoped

**Notes:**
- ✅ No namespace pollution
- ❌ Verbose (prefix on every call)
- ✅ Crystal clear which functions come from VivaceGraph

---

### Pattern 3: Conditional Import (Lisp-Specific)

**Context:** Code must handle multiple Lisp implementations.

**Example:**

```
(in-package :my-application)

#+sbcl
(let ((lock (graph-db:make-rw-lock)))
  (graph-db:with-read-lock lock
    ... read data ...))

#+ccl
(let ((lock (bordeaux-threads:make-lock)))
  (bordeaux-threads:with-lock lock
    ... read data ...))
```

**How it works:**
1. SBCL code uses VivaceGraph's RW-lock primitives
2. CCL code uses bordeaux-threads as fallback
3. Compilation selects correct branch based on `*features*`

**Notes:**
- ✅ Handles Lisp-specific APIs gracefully
- ❌ User code must also be conditional
- ⚠️ Risk: inconsistent behavior across Lisps

---

### Pattern 4: Prolog Query (Advanced)

**Context:** User executes Prolog queries on graph data.

**Example:**

```
(in-package :my-application)
(use-package :graph-db)
(def-global-prolog-functor 'ancestor '(x y)
  '((parent x y))
  '((ancestor x z) (ancestor z y)))
(? (ancestor 'alice 'bob))
```

**How it works:**
1. `def-global-prolog-functor` registers Prolog rule in `:graph-db` package
2. `?` macro executes Prolog query
3. Returns unification results

**Notes:**
- ✅ Integrates graph queries with logic programming
- ❌ Exposes Prolog internals (51 symbols, including `*trail*`, `*var-counter*`, `*functor*`)
- ⚠️ Complex interaction with global state

---

## Implementation Guide

### How VivaceGraph Initializes Its Package

**Step 1: Define Package (compile-time)**

`src/package.lisp` loads:
- Lisp reads defpackage
- Creates :graph-db package
- Registers 166 exported symbols
- (At this point, implementations don't exist yet)

**Step 2: Load Implementation Files (compile-time)**

`src/globals.lisp` loads:
- Defines *graph*, *transaction*, etc. in :graph-db
- These are now accessible as :graph-db/*graph*

`src/utilities.lisp` loads:
- Defines make-graph, open-graph, etc. in :graph-db
- Package already created, so definitions auto-register

**Step 3: User Code Links (load-time + runtime)**

User does `(use-package :graph-db)`:
- Lisp copies all 166 symbols into user's package
- User can now call `(make-graph ...)` directly

### Internal Structure

**Package object contains:**
- **external-symbols:** 166 symbols (the exports)
- **internal-symbols:** symbols used only by VivaceGraph code (not exported)
- **shadowing-symbols:** symbols from `:closer-mop`, `:sb-mop`, etc. that override `:cl`

**When code does `(in-package :graph-db)` at top of each file:**
- Reader looks up symbol names in `:graph-db` package
- Finds exported symbols, uses them
- Can also access internal symbols (same package)

### Extension Points

**To add new public symbol:**
1. Add `#:new-symbol` to `:export` list in `package.lisp`
2. Define implementation in appropriate Layer 1-7 file
3. Symbol auto-registers in package (same name)

**To add Lisp-specific export:**

`#+sbcl #:sbcl-only-symbol`
`#+ccl #:ccl-only-symbol`

**To shadow a symbol:**

`(:shadowing-import-from "MODULE-NAME" "SYMBOL")`

---

## Performance Characteristics

### Compile-Time Cost (Package Definition)

Operation | Complexity | Notes
-----------|-----------|-------
**Package creation** | O(1) | Constant time
**Register 166 exports** | O(166) | Linear, negligible
**MOP shadowing** | O(11) | CCL only, linear
**Total definition load** | O(1) | Dominated by I/O, not computation

**Observation:** Package definition is **compile-time only**. No runtime cost.

---

### Runtime Cost (Symbol Lookup)

Operation | Complexity | Notes
-----------|-----------|-------
**Symbol lookup (`:graph-db:make-graph`)** | O(1) | Hash table lookup in package
**Symbol resolution (imported via `use-package`)** | O(1) | Direct reference after import
**Package-qualified name** | O(1) | No ambiguity resolution needed

**Observation:** Symbol resolution is **negligible**. Compiler inlines these lookups.

---

### Memory Cost

Item | Bytes | Notes
------|-------|-------
**Package object** | ~1 KB | Metadata
**166 symbols + names** | ~10 KB | Symbol table entries
**Shadowing table (CCL)** | ~1 KB | 11 shadowed symbols
**Total** | ~12 KB | Negligible for modern systems

---

## Constraints & Safety

### Safe Operations

Operation | Safe? | Reason
-----------|-------|-------
**`(use-package :graph-db)`** | ✅ | Merges symbols; overwrites user shadows if any
**`(in-package :graph-db)`** | ✅ | Switches package context; no global mutation
**`(find-symbol "MAKE-GRAPH" :graph-db)`** | ✅ | Lookup only, read-only
**`(package-symbols (find-package :graph-db))`** | ✅ | Enumerates exports, read-only
**Calling exported functions** | ✅ | Function calls follow normal Lisp rules

---

### Unsafe Operations

Operation | Safe? | Reason | Risk
-----------|-------|--------|------
**`(setf (symbol-value '*graph*) my-graph)`** | ❌ | Modifies global state | Breaks transaction isolation if other threads reading `*graph*`
**`(setf (symbol-value '*transaction*) my-txn)`** | ❌ | Modifies global state | Race condition with `with-transaction` macro
**`(set-functor-fn 'my-rule '(lambda ...))`** | ❌ | Modifies Prolog global state | Affects all queries, no isolation
**Shadowing imports diverge from spec** | ❌ | MOP semantic mismatch | Results in different metaclass behavior across Lisps
**Removing symbols from exports** | ❌ | Breaks external code | Phase 3 requires these symbols frozen forever

---

### Concurrency Model

**Thread Safety:**
- ✅ Package definition itself is thread-safe (compile-time only)
- ❌ **Global variables** (`*graph*`, `*transaction*`, `*trail*`) are **NOT thread-safe**
  - Each thread should have its own binding via `let` + dynamic scope
  - If threads modify `*graph*` directly, data race

**Recommended Pattern (Thread-Safe):**

```
(let ((graph-db:*graph* (graph-db:open-graph "/tmp/mydb")))
  (with-transaction
    ...))
```

Each thread gets its own binding

**Pattern to AVOID (Thread-Unsafe):**

```
(setf graph-db:*graph* (graph-db:open-graph "/tmp/mydb"))
(thread:make-thread (lambda () (access-graph)))
```

Different thread sees same *graph* (shared!)

---

### Recommended Patterns

Pattern | Rationale
--------|----------
**`let` bind `*graph*` per thread** | Isolates graph context to thread-local scope
**`with-transaction` macro** | Automatically binds `*transaction*` correctly
**Use package prefixes** | Avoids namespace pollution when symbols defined elsewhere
**Avoid modifying exported variables directly** | Let macros/functions manage state
**Test across all 3 Lisps** | Catch MOP shadowing issues early

---

## Edge Cases & Gotchas

### Gotcha 1: Namespace Collision

**Problem:**

```
(in-package :my-app)
(defclass vertex () ...)
(use-package :graph-db)
ERROR: symbol conflict!
```

**Solution:**
- Import before defining conflicting symbols
- Or use package prefix: `(graph-db:vertex ...)`
- Or rename on import: `(use-package :graph-db :except '(vertex edge))`

---

### Gotcha 2: Global State Contamination

**Problem:**

```
(let ((graph-db:*graph* g1))
  (thread:make-thread
    (lambda ()
      (let ((graph-db:*graph* g2))
        (def-global-prolog-functor ...)))))
```

The g2 binding is thread-local, but *prolog-global-functors* is still shared globally!

**Issue:** Different threads can have different `*graph*` bindings, but `*prolog-global-functors*` is globally shared.

**Solution:**
- Reset Prolog state before threads diverge
- Or use separate Prolog contexts per thread (Layer 5 feature, check if available)

---

### Gotcha 3: Lisp-Specific API Unavailable

**Problem:**

Code written on SBCL:
```
(graph-db:with-read-lock my-lock ...)
```

Loaded on CCL:
```
ERROR: UNDEFINED FUNCTION `WITH-READ-LOCK`
```

**Reason:** `with-read-lock` is SBCL-only (line 78).

**Solution:**

```
#+sbcl
(graph-db:with-read-lock my-lock ...)
#+ccl
(bordeaux-threads:with-lock my-lock ...)
```

---

### Gotcha 4: MOP Shadowing Silent Failure

**Problem:**

User's code on CCL, but CLOSER-MOP not installed:
```
(defpackage :graph-db ...)
LOADS SUCCESSFULLY!
But shadowing imports fail silently?
```

**Issue:** If `closer-mop` library not installed, CCL can't find symbols to shadow.

**Risk:** Code compiles, but MOP behavior differs from SBCL.

**Solution:**
- Test Layer 1 MOP compatibility on each Lisp during CI/CD
- Fail fast if shadowing imports missing

---

### Gotcha 5: Prolog Symbol Leakage

**Problem:**

```
(in-package :my-app)
(use-package :graph-db)
(defvar *trail* (make-hash-table))
```

User's code now has a different `*trail*` than Prolog code, causing queries to fail mysteriously.

**Issue:** User shadows graph-db:*trail* with their own variable

**Solution:**
- Move Prolog symbols to separate `:graph-db.prolog` package
- Or document that these are internal (Layer 2 responsibility)

---

### Gotcha 6: Conditional Compilation Mistakes

**Problem:**

File contains:
```
#+sbcl (defvar *sbcl-only* 123)
```

Loaded on CCL:
```
:make-rw-lock not exported! (CCL skipped the line)
ERROR: UNDEFINED SYMBOL
```

**Issue:** Symbol defined with `#+` guard only exists on matching Lisp.

**Solution:**
- Document which symbols are Lisp-specific
- Provide fallback implementations for non-SBCL platforms
- Or mark as `:sbcl-only` in name

---

## Integration Context

### Upstream Dependencies

Dependency | Purpose | Mandatory | Layer
-----------|---------|-----------|-------
`:cl` | Common Lisp standard library | ✅ Yes | System
`:bordeaux-threads` | Cross-platform threading | ✅ Yes | External
`:local-time` | Date/time utilities | ✅ Yes | External
`:closer-mop` | Portable MOP (CCL) | ❌ CCL only | External
`:clos` | Native CLOS (LispWorks) | ❌ LispWorks only | System
`:sb-mop`, `:sb-pcl` | SBCL MOP (SBCL) | ❌ SBCL only | System

**Critical dependency:** If `bordeaux-threads` or `local-time` not installed, package fails to load.

---

### Downstream Usage

Consumer | How | Layer
----------|-----|------
**globals.lisp** | Defines `*graph*`, `*transaction*`, etc. in `:graph-db` | Layer 1
**utilities.lisp** | Defines `make-graph`, `open-graph`, etc. | Layer 1
**All higher layers** | Import from `:graph-db` | Layer 2-7
**User code** | `(use-package :graph-db)` or `(graph-db:symbol)` | External

---

### Layer Position

**Layer 1 (Infrastructure & Base Utilities)** is where `package.lisp` lives.

Layer hierarchy:
```
Layer 1 (package.lisp — namespace definition)
  ↓ (all code in Layer 2-7 depends on this)
Layer 2 (memory/concurrency — uses symbols from Layer 1)
  ↓
Layer 3 (transactions — uses symbols from Layer 1-2)
  ↓
(and so on)
```

**Why Layer 1?** Because Layer 2-7 code needs to be compiled **after** package is defined. Otherwise, when Layer 2 does `(in-package :graph-db)`, the package doesn't exist yet.

---

## When to Use What

### Decision Table: How to Import VivaceGraph

Scenario | Recommendation | Rationale
----------|---|---
**Writing quick script/REPL** | `(use-package :graph-db)` | Convenience, no name collision risk
**Writing production code** | `#:graph-db` prefix or selective import | Explicit, self-documenting, safe
**Writing library** | Package prefix on all calls | Avoid polluting downstream consumers
**Cross-Lisp code** | Conditional (`#+sbcl` etc.) for Lisp-specific features | Handles platform differences
**Prolog queries** | Import via `:graph-db` | Prolog symbols part of API
**Internal VivaceGraph layers** | `(in-package :graph-db)` at top of file | Same package, direct access

---

### Decision: Organizing Exports

**Current situation:** 166 symbols in one flat list.

**Decision A: Keep as-is** (current state)
- ✅ Simple
- ❌ Unclear which are stable, experimental, internal
- ❌ Difficult to enforce backward compatibility (Phase 3)

**Decision B: Separate packages**
```
:graph-db              ; Core (Graph, TX, CRUD)
:graph-db.views       ; Views subsystem
:graph-db.prolog      ; Prolog subsystem
:graph-db.internal    ; Internal/experimental
```
- ✅ Clear separation
- ✅ User imports only what they need
- ❌ User must know which package to import
- ❌ Breaks backward compatibility if changed

**Decision C: Symbol prefixes** (recommended)
```
#:make-graph        ; Core: stable
#:~def-view         ; Experimental: may change
#:%*prolog-trail*   ; Internal: use at own risk
#:@sbcl-rw-lock     ; Lisp-specific: SBCL only
```
- ✅ Single package, clear intent
- ✅ Backward compatible
- ⚠️ Users must learn convention

---

## Summary

Aspect | Status | Details
--------|--------|--------
**Core purpose** | ✅ Clear | Define `:graph-db` namespace and enumerate 166 public symbols
**API contract** | ⚠️ Implicit | 166 symbols listed but not categorized (stable/experimental/internal)
**Cross-Lisp support** | 🟡 Fragile | Conditional compilation + MOP shadowing work but require careful maintenance
**Namespace isolation** | ✅ Complete | All symbols in `:graph-db`, user namespace unpolluted (if using `graph-db:` prefix)
**Global state management** | ❌ Unsafe | `*graph*`, `*transaction*`, Prolog variables exposed directly; no encapsulation
**Threading safety** | ❌ Risky | Dynamic variables not thread-safe without proper `let` binding per thread
**Documentation** | ❌ Missing | No docstrings explaining which symbols are stable, experimental, internal
**Testing** | ❌ Missing | No tests for symbol uniqueness, MOP compatibility, or export completeness
**Backward compatibility** | ⚠️ At risk | Phase 3 will require freezing 166 symbols; currently no mechanism to enforce this

---

### Key Insights

1. **`package.lisp` is the API contract.** Every symbol listed is a promise to keep it available forever (Phase 3: Byzantine Compatibility).

2. **166 symbols is large.** Most Lisp packages export 20-50 symbols. Unclear categorization creates maintenance risk.

3. **MOP shadowing is necessary but fragile.** Each Lisp has different MOP semantics; shadows mask differences but create coupling to external libraries (CLOSER-MOP).

4. **Global state should be encapsulated.** Exposing `*graph*`, `*transaction*` directly invites misuse. Macros like `with-transaction` should handle this.

5. **Prolog doesn't belong in Layer 1.** 51 symbols (31% of exports) are Prolog-specific. Should be in separate package or Layer 5.

6. **Conditional compilation adds complexity.** Three different MOP implementations (SBCL, CCL, LispWorks) require testing all three paths.

---

### Critical Decision: Phase 3 Readiness

**Before Phase 3 (API Surface Definition), must resolve:**

1. ☐ **Categorize 166 symbols** into stable/experimental/internal
2. ☐ **Encapsulate global state** or mark as internal-only
3. ☐ **Relocate Prolog subsystem** to separate package or freeze at current layer
4. ☐ **Test MOP compatibility** across all three Lisps
5. ☐ **Document backward-compatibility rules** (e.g., "these symbols frozen forever")

---

**Status:** Specification complete.  
**Blocking issues identified:** 3 (categorization, global state, Prolog layering)  
**Ready for Nivel 3 (Docstrings & Inline Comments):** YES  
**Ready for Nivel 4 (Execution Mental / Diagrams):** YES (after Nivel 3)  
**Next action:** Create layer1-package-docstrings.lisp with comprehensive annotations

