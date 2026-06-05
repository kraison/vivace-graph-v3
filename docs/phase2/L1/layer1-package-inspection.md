# Layer 1 Inspection Report: package.lisp

**File:** src/package.lisp  
**Lines:** 188 (actual) | 188 (roadmap) — ✅ EXACT MATCH  
**Date:** March 2026  
**Priority:** HIGH (defines entire public API surface)  
**Complexity:** LOW (pure namespace configuration)  
**Type:** Interface/Metadata  

---

## Executive Summary

`package.lisp` defines the `:graph-db` package namespace and exports **166 public symbols** across 6 functional domains:

**Key Characteristics:**
- ✅ **No implementations** — pure package definition
- ✅ **Conditional compilation** for multi-Lisp compatibility (SBCL, CCL, LispWorks)
- ✅ **Strategic shadowing imports** to normalize MOP across Lisp implementations
- ⚠️ **166 exported symbols** — large API surface (concentration risk)
- ⚠️ **Prolog subsystem heavily exposed** (51 symbols, ~31% of exports)

**Why It Matters:**
This file is the **binding contract** between VivaceGraph and external code. Every symbol here is a promise to maintain API stability (cf. Phase 3: Byzantine Compatibility).

---

## Line Count Breakdown

Lines | Section | Type | Notes
──────|---------|------|-------
1 | Context switch (`in-package #:cl-user`) | Meta | Required preamble
3-10 | `defpackage` signature + `:use` declarations | Meta | 8 lines; imports 3 base + 4 conditional MOP modules
11-22 | `:shadowing-import-from` (12 lines) | Meta | SBCL: 1 symbol; CCL: 11 symbols
23-51 | Graph lifecycle exports (29 symbols) | Interface | make-graph, open-graph, replication, snapshot, etc.
53-56 | REST API exports (4 symbols) | Interface | start-rest, stop-rest, def-rest-procedure, *rest-procedures*
58-65 | Transaction exports (8 symbols) | Interface | with-transaction, commit, rollback, *transaction*, etc.
67-84 | Schema & Type exports (18 symbols + SBCL-conditional RW-lock functions) | Interface | def-vertex, def-edge, schema locks, RW-lock operations
86-116 | Vertex/Edge CRUD exports (31 symbols) | Interface | make-vertex, lookup-edge, traverse, copy, save, etc.
118-134 | View system exports (17 symbols) | Interface | def-view, map-view, invoke-graph-view, view locks
136-187 | Prolog exports (51 symbols) | Interface | def-global-prolog-functor, ?, ?-, unify, cut, etc.
188 | Closing `))` | Meta | Package definition end

**Total:** 188 lines exactly as documented in roadmap.

---

## Core Components

### A. Package Declaration (Lines 3-10)

**Structure:**
```
(defpackage #:graph-db
  (:use #:cl
        #:bordeaux-threads
        #:local-time
        #+ccl #:closer-mop
        #+lispworks #:clos
        #+sbcl #:sb-mop
        #+sbcl #:sb-pcl)
```

- **`:use #:cl`** — Standard Common Lisp symbols
- **`:use #:bordeaux-threads`** — Cross-platform threading primitives
- **`:use #:local-time`** — Timestamp/date handling
- **`:use #:closer-mop` (CCL only)** — Portable MOP (meta-object protocol)
- **`:use #:clos` (LispWorks only)** — Native CLOS
- **`:use #:sb-mop` (SBCL only)** — SBCL's MOP implementation
- **`:use #:sb-pcl` (SBCL only)** — SBCL's Portable Common Loops

**Observations:**
- Three **different MOP implementations** being conditionally loaded suggests significant portability effort
- `sb-pcl` is redundant with `sb-mop` in modern SBCL (PCL is the legacy name)
- **No direct import of UUIDs library** — must be handled elsewhere

---

### B. Shadowing Imports (Lines 11-22)

#### SBCL Shadowing (Line 11)

```
#+sbcl (:shadowing-import-from "SB-EXT" "WORD")
```

**Issue #1 — UNUSED SYMBOL**
- `"WORD"` is imported from `SB-EXT` but **NOT EXPORTED**
- Search through codebase needed to confirm if actually used
- If unused: **delete this line**
- If used internally: OK, but should be marked as internal

---

#### CCL Shadowing (Lines 12-22)

```
#+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-METHOD")
#+ccl (:shadowing-import-from "CLOSER-MOP" "FINALIZE-INHERITANCE")
... (9 more symbols)
```

**11 shadowing imports for CCL only.**

**Observations:**
- **Symbol count:** 11 MOP-related symbols
- **Pattern:** All are core CLOS/MOP protocol functions
- **Risk:** If CLOSER-MOP behavior diverges from native CCL CLOS, this masks the divergence
- **Health indicator:** Large number of shadows suggests **fragile cross-Lisp compatibility**

---

### C. Export Categories (Lines 23-188, 166 Symbols)

#### Category 1: Graph Lifecycle (29 symbols, lines 23-51)

Symbol | Purpose
--------|----------
`make-graph` | Create new graph instance
`open-graph` | Open existing graph from disk
`close-graph` | Close graph and flush state
`lookup-graph` | Retrieve graph by name from registry
`graph-stats` | Query performance metrics
`check-data-integrity` | Verify data consistency
`snapshot` | Create persistent backup
`replay` | Replay transaction log
`restore` | Restore from backup
`location` | Graph storage path
`schema` | Graph schema object
`indexes` | Index registry
`*graph*` | **Dynamic variable** — current graph context
`execute-tx` | Execute transaction block
`transaction-p` | Predicate: is transaction?
`graph-name` | Graph identifier
`transaction-error` | Exception type for tx failures
`master-host`, `replication-port`, etc. | Replication state (6 symbols)
`execute-tx-action`, `write-last-txn-id`, etc. | Replication operations (5 symbols)
`stop-buffer-pool` | Shutdown buffer pool

**Risk:** `*graph*` is global mutable state. No guard against concurrent modification without proper locking.

---

#### Category 2: REST API (4 symbols, lines 53-56)

- `start-rest`
- `stop-rest`
- `def-rest-procedure`
- `*rest-procedures*`

**Observation:** Minimal REST surface. Likely mapped to Layer 7 HTTP endpoints.

---

#### Category 3: Transactions (8 symbols, lines 58-65)

- `with-transaction` (Macro)
- `lookup-object`
- `update-node`
- `delete-node`
- `commit`
- `rollback`
- `*transaction*` (Dynamic var — current transaction)
- `no-transaction-in-progress` (Exception)

**Risk:** `*transaction*` is global mutable state. Threading implications if not properly isolated.

---

#### Category 4: Schema & Types (18 symbols + SBCL-only, lines 67-84)

- `def-node-type`
- `def-vertex`
- `def-edge`
- `edge-exists-p`
- `lookup-node-type-by-name`
- `instantiate-node-type`
- `*schema-node-metadata*`
- `with-write-locked-class`
- `with-read-locked-class`
- `schema-class-locks`
- `make-rw-lock` (SBCL only)
- `with-read-lock` (SBCL only)
- `with-write-lock` (SBCL only)
- `acquire-read-lock` (SBCL only)
- `release-read-lock` (SBCL only)
- `acquire-write-lock` (SBCL only)
- `release-write-lock` (SBCL only)
- `rw-lock-p` (SBCL only)

**Issue #2 — SBCL-ONLY THREADING**
- 7 read-write lock operations exported **ONLY for SBCL** (lines 77-84)
- **Critical question:** How do CCL and LispWorks manage schema locking?
- **Risk:** API surface inconsistency across Lisp implementations
- This may be documented in `clos.lisp` or other Layer 1 files

---

#### Category 5: Vertex/Edge CRUD (31 symbols, lines 86-116)

- `vertex` (Generic vertex)
- `edge` (Generic edge)
- `generic-edge`
- `generic-vertex`
- `make-vertex` (Constructor)
- `make-edge` (Constructor)
- `lookup-vertex` (Query by ID)
- `lookup-edge` (Query by ID)
- `to`, `from`, `weight` (Edge accessors)
- `id`, `string-id` (ID accessors)
- `node-to-alist` (Serialization)
- `type-id`, `revision` (Metadata)
- `deleted-p` (Soft delete flag)
- `active-edge-p` (Edge state predicate)
- `data` (Custom slot data)
- `traverse` (Graph traversal)
- `traversal-path` (Path result)
- `end-vertex` (Traversal endpoint)
- `map-vertices`, `map-edges` (Iteration)
- `outgoing-edges`, `incoming-edges` (Navigation)
- `node-slot-value` (Reflective access)
- `copy`, `save` (Persistence)
- `mark-deleted` (Soft delete)
- `stale-revision-error` (Exception)

**Observation:** Standard CRUD + traversal. Well-organized.

---

#### Category 6: Views (17 symbols, lines 118-134)

- `def-view` (Define materialized view)
- `*view-rv*` (View result variable)
- `yield` (Emit result)
- `map-view`, `map-reduced-view` (Query results)
- `invoke-graph-view`
- `make-view`, `delete-view`
- `save-views`, `restore-views`
- `get-view-table-for-class`
- `regenerate-view`
- `lookup-view-group`, `lookup-view`
- `with-write-locked-view-group`
- `with-read-locked-view-group`
- `view-group-lock`

**Observation:** Views are **stateful materializations**. Locking primitives suggest concurrent access.

---

#### Category 7: Prolog (51 symbols, lines 136-187)

- Core operations: `?`, `?-`, `q-`, `!`, `cut`
- Unification: `unify`, `deref-exp`
- Compilation: `compile-body`, `compile-clause`, `def-global-prolog-functor`
- Variable management: `var-deref`, `undo-bindings`, `replace-?-vars`, `variables-in`
- Query interface: `select`, `select-one`, `select-flat`, `select-first`
- Global state: `*prolog-global-functors*`, `*trail*`, `*var-counter*`, `*functor*`
- Query execution: `do-query`, `map-query`
- Infrastructure: `init-prolog`, `*prolog-graph*`, `trace-prolog`, `untrace-prolog`
- Utilities: `make-functor`, `delete-functor`, `make-functor-symbol`, `node-equal`

**Issue #3 — PROLOG SUBSYSTEM EXPOSURE**
- **51 symbols = 31% of total exports**
- **Global state variables exposed:** `*trail*`, `*var-counter*`, `*functor*`, `*prolog-global-functors*`, `*seen-table*`
- **Prolog is a major subsystem** — but should it be in Layer 1?
- Per the roadmap, Prolog logic is Layer 5 (indexing/advanced), not Layer 1
- **Possible architecture issue:** Prolog may belong in Layer 5, not exported from Layer 1

---

## Dependencies

### Imports (":use" declarations)

Module | Purpose | Conditional
--------|---------|----------
`#:cl` | Common Lisp standard library | ✅ Always
`#:bordeaux-threads` | Thread primitives (abstract threading API) | ✅ Always
`#:local-time` | Date/time handling | ✅ Always
`#:closer-mop` | Portable MOP (CCL) | ❌ CCL only
`#:clos` | Native CLOS (LispWorks) | ❌ LispWorks only
`#:sb-mop` | SBCL MOP | ❌ SBCL only
`#:sb-pcl` | SBCL Portable Common Loops | ❌ SBCL only

**Observations:**
- `bordeaux-threads` provides **cross-platform threading abstraction**
- `local-time` adds **temporal semantics** (critical for timestamps, replication, etc.)
- Three different MOP paths suggest **significant portability complexity**

### Exports

166 symbols exported. Consumed by:
- **Layer 2+:** All higher layers depend on this package namespace
- **External consumers:** User code linking against VivaceGraph

---

## Complexity Assessment & Hotspots

### 🟠 CRITICAL: Multi-Lisp Compatibility Fragmentation

**Problem:**
- **SBCL:** Uses `sb-mop` + `sb-pcl` (redundant?)
- **CCL:** Requires 11 shadowing imports from `closer-mop`
- **LispWorks:** Uses native `#:clos`

Each Lisp has **different MOP semantics**. The shadowing imports mask these differences but create coupling to CLOSER-MOP.

**Risk:**
- If CLOSER-MOP or native CLOS changes, the shadowing may silently break
- Testing must cover all three Lisps to catch deviations
- No abstraction layer to manage MOP differences

**Fix:**
- Consolidate MOP access into a single internal module (e.g., `mop-compat.lisp`)
- Have `package.lisp` import from that module, not directly from implementation-specific MOP

---

### 🟠 CRITICAL: Global Mutable State Exposure

**Problem:**
Three global dynamic variables exported directly:
- `*graph*` (line 35) — Current graph context
- `*transaction*` (line 64) — Current transaction context
- `*view-rv*` (line 119) — View result variable
- `*prolog-global-functors*` (line 141) — Prolog functor table
- `*trail*`, `*var-counter*`, `*functor*` (Prolog internal state)
- `*seen-table*` (Prolog visited set)

**Risk:**
- Direct modification by external code bypasses any invariant checking
- In multi-threaded scenarios, no isolation guarantee
- `dynamically-bound` pattern can clash with thread-local storage

**Recommendation:**
- Mark these as `INTERNAL:` in documentation
- Provide accessor functions instead of raw variable access
- Document thread-safety guarantees

---

### 🟡 WARNING: Prolog Subsystem Layering

**Problem:**
- 51 Prolog symbols (31% of exports) in what should be a **foundational package**
- Prolog implementation details exposed: `*trail*`, `*var-counter*`, `*functor*`
- Prolog belongs in Layer 5 (advanced indexing/query), not Layer 1 (infrastructure)

**Risk:**
- Layer 1 should be **minimal and stable**
- Moving Prolog to a separate package later breaks backward compatibility
- Users expecting Layer 1 = foundational are confused by Prolog features

**Question:**
- Should Prolog be in `:graph-db` or `:graph-db.prolog`?

---

### 🟡 WARNING: SBCL-Only Threading Primitives

**Problem:**
- Lines 77-84: `make-rw-lock`, `with-read-lock`, `with-write-lock`, `acquire-read-lock`, `release-read-lock`, `acquire-write-lock`, `release-write-lock`, `rw-lock-p` are **SBCL-only**
- Yet `with-write-locked-class`, `with-read-locked-class` (lines 74-76) are exported unconditionally
- **Disconnect:** High-level locking API (unconditional) vs. low-level primitives (SBCL-only)

**Risk:**
- Users on CCL/LispWorks cannot access RW-lock primitives
- No abstraction to provide equivalent functionality on other Lisps

**Question:**
- How does schema locking work on CCL/LispWorks?
- Is `with-write-locked-class` implemented differently on each Lisp?

---

### 🟡 WARNING: Unused Import (SB-EXT "WORD")

**Problem:**
- Line 11: `#+sbcl (:shadowing-import-from "SB-EXT" "WORD")`
- `"WORD"` is imported but **not exported**
- No evidence of internal use (needs codebase search)

**Risk:**
- Dead code clutters the package definition
- May indicate copy-paste error or obsolete dependency

**Action:**
- Search for usage of `WORD` in Layer 1+ code
- If unused: remove

---

## Issues Found

### 🔴 BLOCKING #1: Conditional MOP Shadowing Not Validated

**Issue:** Lines 11-22 shadow multiple MOP symbols conditionally.

**Problem:**
- No test to verify shadowing succeeds on each Lisp
- No test to verify exported MOP behavior is consistent across Lisps
- If shadowing fails silently, Layer 1+ code may use wrong MOP implementation

**Risk:** HIGH — affects entire metaclass system (Layer 1 foundation)

**Fix:**
Add to Layer 1 tests (when created):

```
(deftest layer1.package.mop-compatibility
  :lisp-specific
  (is (functionp 'defmethod))
  (is (functionp 'defgeneric))
  (is (functionp 'standard-generic-function))
  (is (functionp 'standard-method)))
```

---

### 🔴 BLOCKING #2: API Surface Too Large (166 symbols)

**Issue:** Package exports 166 symbols.

**Problem:**
- No clear separation between public API and internal implementation details
- No clear distinction between stable (frozen) and experimental symbols
- High maintenance burden for backward compatibility (Phase 3)

**Risk:** When moving to Phase 3 (API stability), impossible to know which 166 symbols are actually **critical** vs. implementation artifacts

**Fix:**
1. Categorize exports into:
   - ✅ **Stable** (backward-compatible forever)
   - ⚠️ **Experimental** (may change)
   - 🔒 **Internal** (use at own risk)

2. Use prefix convention:
   - `public-*` or no prefix for stable
   - `%internal-*` for internal use
   - `~experimental-*` for experimental

---

### 🟠 CRITICAL #3: Prolog Subsystem Layering Violation

**Issue:** Lines 136-187 export 51 Prolog symbols from Layer 1.

**Problem:**
- Prolog is an **advanced query language**, not foundational infrastructure
- Belongs in Layer 5 (indexing/advanced features), not Layer 1
- Violates layer abstraction: Layer 1 should be minimal

**Risk:**
- Users confuse Layer 1 purpose (core infrastructure vs. query language)
- Difficult to evolve Prolog independently (tied to Layer 1 API stability)

**Fix:**
Create separate package `:graph-db.prolog` with Prolog symbols. Import into `:graph-db` **only if needed**. Better: move Prolog exports to Layer 5.

---

### 🟠 CRITICAL #4: Global Mutable State Not Documented

**Issue:** Lines 35, 64, 119, and Prolog variables export mutable global state without documentation.

**Problem:**
- `*graph*` (line 35), `*transaction*` (line 64), `*view-rv*` (line 119) are dynamically-scoped globals
- No docstring indicating thread-safety, scope, or intended usage
- In multi-threaded code, these create data races without proper binding

**Risk:**
- Users may modify `*graph*` directly, corrupting state
- Transaction isolation broken if `*transaction*` is shared across threads

**Fix:**
Document in Level 2 (Specification) that these are **internal use only** or provide accessor macros.

---

### 🟡 WARNING #5: Unused Import (SBCL "WORD")

**Issue:** Line 11 imports `"WORD"` from `SB-EXT` but never exports it.

**Problem:**
- Dead code or missed refactoring
- Increases package definition complexity

**Action:**
- Grep entire codebase for `WORD` usage
- If absent: remove line 11
- If present: document why it's needed but not exported

---

### 🟡 WARNING #6: RW-Lock Primitives SBCL-Only

**Issue:** Lines 77-84 export RW-lock functions **only for SBCL**.

**Problem:**
- Creates platform-specific API surface
- Users on CCL/LispWorks cannot access low-level RW-lock primitives
- `with-write-locked-class` (line 75) is unconditional but may rely on SBCL-only primitives

**Risk:**
- Incomplete cross-platform support
- Hidden dependencies on SBCL-specific implementation

**Action:**
- Document how schema locking is achieved on CCL/LispWorks
- Provide portable RW-lock abstraction in Layer 1 (in `clos.lisp` or similar)
- Export abstraction from both SBCL and other Lisps

---

## Testing Strategy

### Critical Tests to Write

Test | Setup | Action | Expected
------|--------|---------|----------
**Package exists** | None | `(find-package :graph-db)` | Returns package object
**All symbols exported** | None | For each of 166 symbols: `(find-symbol (symbol-name sym) :graph-db)` | Returns symbol
**No symbol collision** | None | Check all exports are unique | All distinct
**MOP compatibility (SBCL)** | SBCL only | `(functionp 'defmethod)`, `(functionp 'defgeneric)` | All return T
**MOP compatibility (CCL)** | CCL only | Verify CLOSER-MOP shadowing worked | All MOP functions callable
**MOP compatibility (LispWorks)** | LispWorks only | Verify native CLOS accessible | All CLOS functions callable
**Global variables accessible** | None | `(boundp '*graph*)`, `(boundp '*transaction*)` | All return T

---

## Code Quality Summary

Aspect | Status | Notes
--------|--------|-------
**Docstrings** | ❌ **MISSING** | No docstrings on package definition. Missing documentation for 166 symbols.
**Test coverage** | ❌ **MISSING** | No tests for package exports, MOP compatibility, symbol uniqueness.
**Performance** | ✅ **N/A** | Package definition is compile-time, negligible runtime cost.
**Complexity** | 🟡 **HIGH** | Conditional compilation across 3 Lisps + 11 CCL shadowing imports creates mental overhead.
**Lisp idiom** | ✅ **CORRECT** | Uses `defpackage` correctly, appropriate use of `#+` conditionals.
**Portability** | 🟡 **FRAGILE** | Works on SBCL/CCL/LispWorks but requires exact MOP compatibility. One breaking change to CLOSER-MOP or native CLOS = breakage.

---

## Comparison (Optional)

**vs. typical Lisp package definitions:**
- ✅ **Good:** Uses conditional compilation instead of duplicating package definitions
- ❌ **Bad:** 166 exports is large; most packages export 20-50 symbols
- ❌ **Bad:** Mixes core infrastructure (Graph, TX) with advanced features (Prolog, Views) at same namespace level
- ✅ **Good:** Uses `:shadowing-import-from` correctly to mask implementation differences

---

## Summary

Metric | Value | Assessment
--------|-------|----------
**Lines** | 188 | ✅ Exact match with roadmap
**Symbols Exported** | 166 | 🟡 Larger than typical; needs categorization
**Blocking Issues** | 2 | 🔴 MOP validation + API categorization required
**Critical Issues** | 2 | 🔴 Prolog layering + global state documentation
**Warnings** | 2 | 🟡 Unused import + RW-lock portability
**Code Health** | 🟡 **MEDIUM** | Structurally sound, but documentation gaps and portability risks
**Ready for Layer 2?** | 🔴 **NO** | Must address blocking issues and add docstrings

---

## Next Steps

1. ✅ **Search codebase:** Find all usages of `WORD` (line 11). If zero: remove.

2. ✅ **Document MOP compatibility:** Add test suite for Layer 1 MOP to verify shadowing works on all three Lisps (SBCL, CCL, LispWorks).

3. ✅ **Categorize exports:** Divide 166 symbols into:
   - **Core API** (stable, backward-compatible)
   - **Experimental** (may change in future versions)
   - **Internal** (implementation details, use at own risk)

4. ✅ **Prolog extraction:** Decide whether to move Prolog symbols to separate package (`:graph-db.prolog`) or keep in `:graph-db`. Document rationale.

5. ✅ **RW-lock abstraction:** Provide portable read-write lock primitives for CCL/LispWorks OR document how schema locking is achieved on non-SBCL platforms.

6. ✅ **Add package docstring:** Explain what `:graph-db` provides, which symbols are stable, and cross-Lisp expectations.

---

**Status:** Inspection complete. Issues identified: 6 (2 blocking, 2 critical, 2 warning).  
**Ready for Nivel 2 (Component Specification):** NO — must address blocking issues first.  
**Next action:** Create layer1-package-component.md (Specification) + resolve blocking issues.

