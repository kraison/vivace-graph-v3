# VivaceGraph Contribution Roadmap

## From Documentation to "The SQLite of Graph Databases"

**Author:** David  
**Started:** March 2026  
**Status:** Phase 1 complete, Phase 2 ready to begin  

## Current State

### What exists
- VivaceGraph v3: ~12,088 lines of Common Lisp, 7-layer architecture
- Original author in Ukraine, seeking maintainers
- 11 open issues, near-zero documentation in upstream repo
- No active maintainer for API stability
- Existing tests: `test.lisp`, `test-lhash.lisp`, `xach-test.lisp` (minimal, no layer structure)

### What we built (Phase 1)
- 9 documentation files (~8,900 lines) in Spanish
- 9 translated documentation files (~8,900 lines) in English
- Architecture overview with dependency maps, load order, and cross-references
- All hosted at `github.com/kraison/vivace-graph/docs/`

### What's missing for the C API goal
- Source code annotations (docstrings, inline comments)
- Detailed diagrams per layer (pipelines, communication, data flow)
- Unit tests per layer (currently near-zero coverage)
- Identification of public vs internal API surface
- Stability guarantees ("byzantine compatibility")
- C-callable function signatures

## Project Directory Structure

```
VivaceGraph/
│
├─ graph-db.asd ..................... ASDF system definition
├─ README.md
├─ ROADMAP.md ....................... This document
│
├─ src/ ............................. (existing .lisp files, flat for now)
│  ├─ package.lisp
│  ├─ globals.lisp
│  ├─ ... (all 44 source files)
│  └─ rest.lisp
│
├─ docs/
│  ├─ readme.md / readme-en.md
│  ├─ esArchitecture.md / enArchitecture.md
│  ├─ es01-LAYER1-INFRA.md / en01-LAYER1-INFRA.md
│  ├─ ... (all 7 layer docs, es + en)
│  └─ diagrams/ ..................... Phase 2 deliverables
│     ├─ layer1/
│     ├─ layer2/
│     ├─ layer3/
│     ├─ layer4/
│     ├─ layer5/
│     ├─ layer6/
│     └─ layer7/
│
├─ tests/
│  ├─ test-suite.asd ............... ASDF test system definition
│  ├─ test-package.lisp ............ Test package definition
│  ├─ test-runner.lisp ............. Run all tests, report results
│  ├─ helpers.lisp ................. Shared test utilities
│  │
│  ├─ layer1/
│  │  ├─ test-globals.lisp
│  │  ├─ test-conditions.lisp
│  │  ├─ test-utilities.lisp
│  │  ├─ test-uuid.lisp
│  │  ├─ test-random.lisp
│  │  ├─ test-clos.lisp
│  │  ├─ test-graph-class.lisp
│  │  └─ test-node-class.lisp
│  │
│  ├─ layer2/
│  │  ├─ test-pcons.lisp
│  │  ├─ test-pmem.lisp
│  │  ├─ test-mmap.lisp
│  │  ├─ test-rw-lock.lisp
│  │  ├─ test-queue.lisp
│  │  ├─ test-mailbox.lisp
│  │  └─ test-cursors.lisp
│  │
│  ├─ layer3/
│  │  ├─ test-transactions.lisp
│  │  ├─ test-transaction-restore.lisp
│  │  ├─ test-backup.lisp
│  │  ├─ test-gc.lisp
│  │  └─ test-txn-log.lisp
│  │
│  ├─ layer4/
│  │  ├─ test-skip-list.lisp
│  │  ├─ test-skip-list-cursors.lisp
│  │  ├─ test-linear-hash.lisp
│  │  ├─ test-allocator.lisp
│  │  ├─ test-buffer-pool.lisp
│  │  ├─ test-serialize.lisp
│  │  ├─ test-node-id.lisp
│  │  ├─ test-index.lisp
│  │  ├─ test-index-list.lisp
│  │  └─ test-index-vector.lisp
│  │
│  ├─ layer5/
│  │  ├─ test-ve-index.lisp
│  │  ├─ test-vev-index.lisp
│  │  ├─ test-type-index.lisp
│  │  ├─ test-functor.lisp
│  │  ├─ test-prologc.lisp
│  │  └─ test-views.lisp
│  │
│  ├─ layer6/
│  │  ├─ test-primitive-node.lisp
│  │  ├─ test-vertex.lisp
│  │  ├─ test-edge.lisp
│  │  └─ test-schema.lisp
│  │
│  ├─ layer7/
│  │  ├─ test-interface.lisp
│  │  ├─ test-traverse.lisp
│  │  └─ test-rest.lisp
│  │
│  └─ integration/
│     ├─ test-acid-compliance.lisp .. Full ACID scenario tests
│     ├─ test-concurrent-access.lisp  Multi-thread stress tests
│     ├─ test-crash-recovery.lisp ... Kill-and-recover scenarios
│     ├─ test-replication.lisp ...... Master-slave sync tests
│     └─ test-end-to-end.lisp ...... Full create→query→delete cycles
│
├─ benchmarks/
│  ├─ bench-skip-list.lisp
│  ├─ bench-linear-hash.lisp
│  ├─ bench-transactions.lisp
│  ├─ bench-traversal.lisp
│  └─ bench-serialize.lisp
│
└─ bindings/ ........................ Phase 4 deliverables
   ├─ c/
   │  ├─ vivacegraph.h
   │  ├─ vivacegraph-impl.lisp
   │  └─ Makefile
   ├─ python/
   ├─ go/
   └─ rust/
```

## Phase 2: Source Code Annotation + Unit Tests

**Goal:** Every function has inline documentation AND a corresponding test. Each layer ends with diagrams that capture the complete internal logic.

**Testing framework:** `fiveam` (standard CL test framework, already in Quicklisp)

**Test convention:**
- One test file per source file
- Test names follow pattern: `layer.file.function` (e.g., `l1.utilities.less-than`)
- Each test file can run independently
- `(asdf:test-system :vivacegraph-tests)` runs everything

### Layer 1: Infrastructure (~1,685 lines, 10 files)

#### Annotation targets

| File | Lines | Annotation Priority |
|------|-------|---------------------|
| `package.lisp` | 188 | Low — list of exports |
| `globals.lisp` | 133 | Low — constants |
| `conditions.lisp` | 83 | Low — exception defs |
| `utilities.lisp` | 483 | **High** — core helpers used everywhere |
| `clos.lisp` | 88 | **High** — MOP interceptors need detail |
| `uuid.lisp` | 121 | Medium |
| `random.lisp` | 254 | Low — standalone Mersenne Twister |
| `stats.lisp` | 77 | Low |
| `graph-class.lisp` | 84 | Medium — root class |
| `node-class.lisp` | 174 | **High** — metaclass drives persistence |

#### Unit tests for Layer 1

| Test File | What it covers |
|-----------|----------------|
| `test-globals.lisp` | Constants have correct values and types, namespaces don't collide |
| `test-conditions.lisp` | Each condition signals correctly, hierarchy is intact |
| `test-utilities.lisp` | `less-than`/`greater-than` cross-type ordering, `key-vector<` correctness, time conversion round-trips, `flatten`/`find-all`/`find-anywhere`, lock macros don't deadlock on single thread |
| `test-uuid.lisp` | `uuid?` predicate, `uuid-eql` reflexivity/symmetry, byte-array round-trip, mmap read/write of UUIDs |
| `test-random.lisp` | Mersenne Twister period sanity check, distribution uniformity (chi-squared), reproducibility with same seed |
| `test-clos.lisp` | `slot-value-using-class` interception works, meta-slots vs data-slots distinction, modification tracking |
| `test-graph-class.lisp` | `make-graph` creates valid instance, `lookup-graph` retrieves by name, master/slave class hierarchy |
| `test-node-class.lisp` | Persistent/ephemeral/meta slot classification, `data-slots` returns correct list, `find-all-subclasses` traversal, base NODE has all expected slots |

#### Diagrams

- `diagrams/layer1/dependencies.md` — internal dependency graph
- `diagrams/layer1/mop-flow.md` — slot-value interception pipeline
- `diagrams/layer1/uuid-namespaces.md` — UUID generation flow

### Layer 2: Memory & Synchronization (~702 lines, 8 files)

#### Annotation targets

| File | Lines | Annotation Priority |
|------|-------|---------------------|
| `pcons.lisp` | 37 | Low |
| `pmem.lisp` | 78 | Medium |
| `pstruct.lisp` | 50 | **High** — decide: fix or remove |
| `mmap.lisp` | 266 | **High** — FFI boundary, segfault handling |
| `rw-lock.lisp` | 185 | Medium — verify edge cases |
| `queue.lisp` | 43 | Low |
| `mailbox.lisp` | 31 | Low |
| `cursors.lisp` | 12 | Low |

#### Unit tests for Layer 2

| Test File | What it covers |
|-----------|----------------|
| `test-pcons.lisp` | Serialize/deserialize round-trip (25 bytes), deleted-p flag toggle, CAR/CDR integrity after write |
| `test-pmem.lisp` | Stack grows upward correctly, heap grows downward correctly, collision detection (SPP >= HPP), lock prevents concurrent corruption |
| `test-mmap.lisp` | File creation with correct size, `set-byte`/`get-byte` round-trip, `get-bytes`/`set-bytes` for multi-byte, `serialize-uint64`/`deserialize-uint64` round-trip for edge values (0, 1, max), `extend-mapped-file` preserves existing data, `munmap-file` with save-p flushes to disk |
| `test-rw-lock.lisp` | Multiple readers concurrent (no block), writer blocks new readers, writer waits for existing readers, reader→writer upgrade (`reading-p t`), recursive write lock ownership, release-by-non-owner signals error |
| `test-queue.lisp` | `enqueue`/`dequeue` FIFO order, `enqueue-front` prepends, `empty-queue-p` on empty and non-empty, `queue-length` accuracy |
| `test-mailbox.lisp` | `send-message`/`receive-message` round-trip, timeout returns nil (not hang), cross-thread message passing |
| `test-cursors.lisp` | Generic protocol: `cursor-next` returns elements then EOC sentinel |

#### Diagrams

- `diagrams/layer2/memory-model.md` — stack/heap layout with pointer arithmetic
- `diagrams/layer2/mmap-lifecycle.md` — create → extend → sync → munmap flow
- `diagrams/layer2/rwlock-states.md` — state machine for reader/writer transitions
- Decision document on `pstruct.lisp` — keep, complete, or deprecate

### Layer 3: Persistence & Transactions (~2,336 lines, 8 files)

#### Annotation targets

| File | Lines | Annotation Priority |
|------|-------|---------------------|
| `transactions.lisp` | 1,402 | **Critical** — largest file, ACID core |
| `transaction-restore.lisp` | 71 | Low |
| `transaction-streaming.lisp` | 484 | **High** — replication protocol |
| `transaction-log-streaming.lisp` | 112 | Medium |
| `txn-log.lisp` | 51 | Low |
| `backup.lisp` | 80 | Low |
| `replication.lisp` | 3 | Low — placeholder |
| `gc.lisp` | 133 | Medium |

#### Unit tests for Layer 3

| Test File | What it covers |
|-----------|----------------|
| `test-transactions.lisp` | **Object sets:** create, add, member-p, intersection detection. **Transaction lifecycle:** create → active state, commit → committed state, rollback → aborted state. **Validation:** conflicting write-sets detected, non-conflicting write-sets pass. **Retry:** validation-conflict triggers retry (up to 8), exclusive lock on max retries. **Copy-on-write:** lookup-node adds to read-set, update-node adds to write-set. **Isolation:** concurrent transactions see consistent snapshots |
| `test-transaction-restore.lisp` | Snapshot file parsed correctly, vertex/edge plists reconstructed, highest-txn-id persisted after restore |
| `test-backup.lisp` | Vertex serializes to correct plist format, edge serializes with from/to/weight, full graph backup produces readable file, `check-data-integrity` finds no problems on clean graph, `check-data-integrity` finds corrupted node |
| `test-gc.lisp` | Referenced allocations survive GC, unreferenced allocations freed, index-list addresses mapped correctly, GC on empty graph is no-op |
| `test-txn-log.lisp` | Snapshot creates file with correct name format, `find-newest-snapshot` returns most recent, replay restores state from snapshot + logs |

#### Diagrams

- `diagrams/layer3/txn-lifecycle.md` — create → validate → commit/rollback state machine
- `diagrams/layer3/conflict-detection.md` — read-set/write-set intersection logic
- `diagrams/layer3/replication-protocol.md` — packet format, handshake, streaming
- `diagrams/layer3/recovery-sequence.md` — crash → snapshot → replay → consistent state
- `diagrams/layer3/gc-mark-sweep.md` — allocation table → mark → sweep flow

### Layer 4: Data Structures (~3,501 lines, 10 files)

#### Annotation targets

| File | Lines | Annotation Priority |
|------|-------|---------------------|
| `skip-list.lisp` | 888 | **Critical** — core index structure |
| `skip-list-cursors.lisp` | 122 | Low |
| `linear-hash.lisp` | 764 | **Critical** — core storage |
| `allocator.lisp` | 334 | **High** — memory management |
| `buffer-pool.lisp` | 424 | Medium |
| `serialize.lisp` | 470 | **High** — type system for disk |
| `node-id.lisp` | 54 | Low |
| `index.lisp` | 56 | Low |
| `index-list.lisp` | 192 | Medium |
| `index-vector.lisp` | 197 | Medium |

#### Unit tests for Layer 4

| Test File | What it covers |
|-----------|----------------|
| `test-skip-list.lisp` | Insert single element and find it. Insert N elements, find all. Insert duplicates (when allowed). Insert duplicates rejected (when disallowed). Remove element, confirm not found. Remove non-existent key returns nil. Ordering preserved after random insertions. Concurrent reads don't block. Write lock excludes readers. `random-level` distribution roughly follows 1/4 probability. Head/tail sentinels never returned as data. Persistence: create → close → reopen → data intact |
| `test-skip-list-cursors.lisp` | Forward iteration visits all elements in order. Value cursor returns values only. Key cursor returns keys only. Range cursor `[start, end)` boundaries correct. Empty range returns EOC immediately |
| `test-linear-hash.lisp` | `lhash-put`/`lhash-get` round-trip. Overwrite existing key. Get non-existent key returns nil. Auto-split triggers at threshold. Split preserves all entries. Count accurate after inserts/deletes. Concurrent access with locks. Persistence: create → close → reopen → data intact |
| `test-allocator.lisp` | Allocate returns valid address. Free returns to correct bin. Re-allocate reuses freed address. `grow-memory` extends file. Out-of-memory detection |
| `test-buffer-pool.lisp` | `get-buffer` returns correct size. `return-buffer` makes it available for reuse. Pool pre-fills on low water mark. GC triggered on low memory |
| `test-serialize.lisp` | Round-trip for every type: nil, t, positive-integer, negative-integer, single-float, double-float, character, string (ASCII + UTF-8), symbol, list (nested), vector, UUID, timestamp. Unknown type signals error. Extensibility: custom type serializes/deserializes |
| `test-node-id.lisp` | `gen-vertex-id` returns 16 bytes. `gen-edge-id` returns 16 bytes. Vertex and edge IDs never collide (different namespaces). Two calls return different IDs |
| `test-index.lisp` | String index sorts correctly. Number index sorts correctly. Unique index rejects duplicates |
| `test-index-list.lisp` | Push adds to front. Map iterates all in order. Deleted pcons skipped during map. Persistence round-trip |
| `test-index-vector.lisp` | Create with N elements. `push-extend` grows correctly. Old allocation freed after extend. Persistence round-trip |

#### Diagrams

- `diagrams/layer4/skiplist-operations.md` — insert/find/remove with level management
- `diagrams/layer4/linear-hash-split.md` — bucket splitting step by step
- `diagrams/layer4/allocator-bins.md` — size-to-bin mapping, free-list management
- `diagrams/layer4/serialization-format.md` — complete type tag table with byte layouts
- `diagrams/layer4/buffer-pool-monitor.md` — monitor thread, low-water marks, GC triggers

### Layer 5: Indexing (~1,996 lines, 6 files)

#### Annotation targets

| File | Lines | Annotation Priority |
|------|-------|---------------------|
| `ve-index.lisp` | 194 | Medium |
| `vev-index.lisp` | 219 | Medium |
| `type-index.lisp` | 70 | Low |
| `functor.lisp` | 64 | Medium |
| `views.lisp` | 732 | **High** — map-reduce internals |
| `prologc.lisp` | 717 | **High** — compiler internals |

#### Unit tests for Layer 5

| Test File | What it covers |
|-----------|----------------|
| `test-ve-index.lisp` | Add edge registers in VE-index. Lookup returns correct index-list. Multiple edge types on same vertex separated. Cache hit returns same object. Remove edge updates index |
| `test-vev-index.lisp` | Add edge registers V1→type→V2. Lookup specific edge by endpoints + type. Non-existent edge returns nil. Bidirectional edges stored independently |
| `test-type-index.lisp` | Push adds UUID to correct type slot. `get-all-of-type` returns all nodes. `unless-present` prevents duplicates. Per-type lock doesn't cross-block |
| `test-functor.lisp` | `make-functor` creates and compiles. `add-functor-clause` extends existing. `reset-functor` clears all clauses. `delete-functor` removes from table. Thread-safe: concurrent clause addition |
| `test-prologc.lisp` | **Variables:** create unbound, bind, dereference chain. **Unification:** identical atoms unify, different atoms fail, variable unifies with atom, two variables unify and share binding, list unification recursive, nested list unification. **Backtracking:** trail records bindings, `undo-bindings` restores to checkpoint, multiple backtrack points nested correctly. **Prove:** single fact proves, rule with body proves, mutual recursion terminates (cut), no matching clause fails. **Compilation:** functor compiles to callable function, compiled functor produces same results as interpreted |
| `test-views.lisp` | `make-view` creates with map-fn. `regenerate-view` populates skip-list. `map-view` iterates results. Range query with start-key/end-key. Count limit respected. View update after data change. `restore-views` loads from disk |

#### Diagrams

- `diagrams/layer5/ve-vev-lookup.md` — key construction → hash → index-list traversal
- `diagrams/layer5/prolog-compilation.md` — clause → compiled function pipeline
- `diagrams/layer5/prolog-execution.md` — unification → backtracking → trail management
- `diagrams/layer5/view-lifecycle.md` — define → regenerate → query → update cycle

### Layer 6: Data Model (~1,351 lines, 4 files)

#### Annotation targets

| File | Lines | Annotation Priority |
|------|-------|---------------------|
| `primitive-node.lisp` | 319 | Medium — verify header format |
| `vertex.lisp` | 194 | Low |
| `edge.lisp` | 427 | Medium |
| `schema.lisp` | 411 | **High** — runtime type creation |

#### Unit tests for Layer 6

| Test File | What it covers |
|-----------|----------------|
| `test-primitive-node.lisp` | Header serialization: flags byte encodes all 7 flags correctly. Header deserialization round-trip. `lookup-node` checks cache first. `lookup-node` falls back to lhash. `save-node` writes to heap and updates pointer. `save-node` caches result |
| `test-vertex.lisp` | `make-vertex` with `:generic` type. `make-vertex` with custom type changes class. `lookup-vertex` by byte-array ID. `lookup-vertex` by string UUID. `delete-vertex` sets deleted-p. Double-delete signals `vertex-already-deleted-error`. `map-vertices` iterates all. `map-vertices` with type filter. `map-vertices` excludes deleted (default). `map-vertices` includes deleted when requested |
| `test-edge.lisp` | `make-edge` with from/to vertices. `make-edge` with from/to UUIDs. Edge header is 35 bytes (15 node + 16 from + 16 to + 8 weight - wait, that's 55 — verify). Edge indexes in VE, VEV, and type-index on creation. `outgoing-edges` returns correct set. `incoming-edges` returns correct set. `map-edges` with edge-type filter. `map-edges` with direction filter. Weight stored as IEEE 754 double |
| `test-schema.lisp` | `init-schema` creates empty tables. `def-vertex` registers type with correct ID. `def-edge` registers type with correct ID. `def-vertex` generates CLOS class. `def-vertex` generates Prolog predicate. `save-schema`/load round-trip. `schema-digest` changes when schema changes. `list-vertex-types` includes generic + custom. `list-edge-types` includes generic + custom |

#### Diagrams

- `diagrams/layer6/node-header-format.md` — byte-level layout (15 bytes node, 55 bytes edge)
- `diagrams/layer6/schema-registration.md` — def-vertex → CLOS class → type-index → Prolog predicate
- `diagrams/layer6/crud-flow.md` — create/read/update/delete with transaction integration

### Layer 7: User API (~517 lines, 3 files)

#### Annotation targets

| File | Lines | Annotation Priority |
|------|-------|---------------------|
| `interface.lisp` | 27 | Low |
| `traverse.lisp` | 81 | Low |
| `rest.lisp` | 409 | Medium — endpoint audit |

#### Unit tests for Layer 7

| Test File | What it covers |
|-----------|----------------|
| `test-interface.lisp` | `copy` vertex produces independent clone. `copy` edge produces independent clone. `mark-deleted` sets deleted-p. `save` persists changes. Non-node argument signals error |
| `test-traverse.lisp` | BFS from isolated vertex returns empty. BFS depth-1 returns direct neighbors. BFS depth-2 returns neighbors-of-neighbors. Direction `:out` excludes incoming. Direction `:in` excludes outgoing. Direction `:both` includes all. `edge-type` filter works. `return-paths` returns traversal objects with correct paths. Visited set prevents cycles. Disconnected components not reached |
| `test-rest.lisp` | `start-rest`/`stop-rest` lifecycle. Auth with valid credentials succeeds. Auth with invalid credentials returns 401. GET vertex returns JSON with correct fields. POST vertex creates and returns with ID. PUT vertex updates and returns. DELETE vertex marks deleted. GET non-existent returns 404. Edge CRUD mirrors vertex CRUD. GET vertex edges returns incoming + outgoing. GET graph returns schema JSON |

#### Diagrams

- `diagrams/layer7/rest-routes.md` — complete endpoint table with auth flow
- `diagrams/layer7/bfs-traversal.md` — queue management, visited tracking, path construction

### Integration Tests

After all layers are individually tested:

| Test File | What it covers |
|-----------|----------------|
| `test-acid-compliance.lisp` | **Atomicity:** partial failure rolls back completely. **Consistency:** invalid state rejected. **Isolation:** concurrent transactions see consistent views. **Durability:** committed data survives simulated crash (kill process → restart → verify) |
| `test-concurrent-access.lisp` | N threads writing different vertices simultaneously. N threads writing SAME vertex (conflict detection). Reader threads concurrent with writer thread. Deadlock detection (timeout-based) |
| `test-crash-recovery.lisp` | Write 1000 nodes → snapshot → write 500 more → simulate crash → replay → verify all 1500 present. Crash mid-transaction → verify rollback on recovery |
| `test-replication.lisp` | Master-slave handshake. Schema verification. Streaming catch-up from log. Real-time replication of new transactions. Slave contains same data as master after sync |
| `test-end-to-end.lisp` | Create graph → define schema → create vertices → create edges → traverse → query Prolog → create view → query view → delete edge → delete vertex → verify indexes updated → close graph → reopen → verify state |

## Phase 2 Summary

| Layer | Files | Lines | Annotation sprints | Test sprints | Diagram sprints | Total |
|-------|-------|-------|---------------------|---------------|------------------|-------|
| 1 | 10 | 1,685 | 2 | 1 | 1 | **4** |
| 2 | 8 | 702 | 1 | 1 | 1 | **3** |
| 3 | 8 | 2,336 | 3 | 2 | 1 | **6** |
| 4 | 10 | 3,501 | 3 | 3 | 1 | **7** |
| 5 | 6 | 1,996 | 2 | 2 | 1 | **5** |
| 6 | 4 | 1,351 | 2 | 1 | 1 | **4** |
| 7 | 3 | 517 | 1 | 1 | 1 | **3** |
| Integration | — | — | — | 4 | — | **4** |
| **Total** | **49** | **12,088** | **14** | **15** | **7** | **36** |

## Phase 3: API Surface Definition

**Goal:** Define which functions become part of the stable public API ("byzantine compatibility") and which remain internal.

### 3.1 API Inventory

- Catalog every exported symbol from `package.lisp` (147 symbols)
- Classify each as: **stable** (never changes), **experimental** (may change), or **internal** (not exposed)
- Document preconditions, postconditions, and error behavior for each stable symbol

### 3.2 API Stability Policy

- Define versioning scheme (semantic versioning)
- Define deprecation process (announce → warn → remove, minimum 2 versions)
- Define backward compatibility rules (SQLite-style: "your code from 2026 works in 2036")

### 3.3 Regression Test Suite

- Every stable API function has at least one test that NEVER gets deleted
- These tests become the "compatibility contract"
- Breaking a regression test = breaking compatibility = requires major version bump

## Phase 4: C API Design (Inverse FFI)

**Goal:** Expose VivaceGraph as a C shared library (`.so` / `.dylib` / `.dll`) callable from any language.

### 4.1 C Header Design

```c
// vivacegraph.h — target API surface
typedef struct vg_graph vg_graph;
typedef struct vg_vertex vg_vertex;
typedef struct vg_edge vg_edge;
typedef struct vg_transaction vg_transaction;

// Lifecycle
vg_graph* vg_open(const char* path, int flags);
int       vg_close(vg_graph* g);

// Vertices
vg_vertex* vg_vertex_create(vg_graph* g, const char* type, const char* json_data);
vg_vertex* vg_vertex_lookup(vg_graph* g, const char* uuid);
int        vg_vertex_delete(vg_graph* g, const char* uuid);

// Edges
vg_edge* vg_edge_create(vg_graph* g, const char* from, const char* to, 
                         const char* type, double weight);

// Transactions
vg_transaction* vg_txn_begin(vg_graph* g);
int             vg_txn_commit(vg_transaction* txn);
int             vg_txn_rollback(vg_transaction* txn);

// Query
int vg_query_prolog(vg_graph* g, const char* query, vg_callback cb, void* ctx);

// Traversal
int vg_traverse_bfs(vg_graph* g, const char* start_uuid, 
                    int max_depth, vg_callback cb, void* ctx);
```

### 4.2 Implementation Strategy

- Use SBCL's `sb-alien` or CFFI's callback mechanism to expose Lisp functions as C symbols
- Compile VivaceGraph as a shared library using `sb-ext:save-lisp-and-die` with `:callable` option
- Handle Lisp conditions (exceptions) by converting to C error codes
- Manage Lisp GC interaction with C pointers via pinning or handle tables

### 4.3 C API Tests

- Mirror every Lisp test as a C test using the C API
- These prove the C API is functionally equivalent to the Lisp API
- Use a minimal C test framework (or just assert + return codes)

### 4.4 Build System

- Makefile that produces `libvg3.so` from SBCL
- pkg-config file for easy integration
- CMake find module for C/C++ projects

## Phase 5: Language Bindings

Built on the C API from Phase 4:

| Language | Mechanism | Priority | Effort |
|----------|-----------|----------|--------|
| Python | ctypes / cffi | **High** — largest potential user base | 2-3 sprints |
| Go | cgo | **High** — natural fit (embedded, no server) | 2-3 sprints |
| Rust | bindgen / FFI | Medium | 2-3 sprints |
| Julia | ccall | Medium | 1-2 sprints |

Each binding includes:
- Idiomatic wrapper (not raw FFI)
- Language-native error handling
- Test suite mirroring the C API tests
- Package manager distribution (PyPI, Go modules, crates.io)

## Phase 6: Packaging & Distribution

### 6.1 Binary Distribution

- Prebuilt shared libraries for Linux (x86_64, aarch64), macOS (arm64), Windows (x86_64)
- Static linking option for embedding
- Size target: < 50MB for the shared library (SBCL runtime + VivaceGraph)

### 6.2 Package Managers

- PyPI package (`pip install vivacegraph`)
- Go module (`go get github.com/kraison/vg3-go`)
- Cargo crate (if Rust bindings)
- Quicklisp (native CL, already possible)

### 6.3 Documentation Site

- API reference generated from annotated source
- Getting started guides per language
- Architecture deep-dive (from our Phase 1/2 docs)
- Performance benchmarks and comparisons

**Each step is useful on its own.** 

- Annotated code is valuable even without the C API. 
- Tests are valuable even without bindings. 
- The C API is valuable even with just one language binding. 

This is not all-or-nothing — it's a ramp.

*VivaceGraph Contribution Roadmap*  
*March 2026*
