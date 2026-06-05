# VivaceGraph - Modular Architecture and Library Structure

## Table of Contents

1. [Overview](#overview)
2. [7-Layer Architecture](#7-layer-architecture)
3. [Vertical Dependencies](#vertical-dependencies)
4. [Special Files](#special-files)
5. [Initialization Flow](#initialization-flow)
6. [Recommended Reading Guide](#recommended-reading-guide)
7. [Key Points on Modularization](#key-points-on-modularization)

## Overview

**VivaceGraph** is an ACID (Atomicity, Consistency, Isolation, Durability) graph database written in pure **Common Lisp**.

### Key Features:
- ACID-compliant object graph model
- User-customizable indexes
- Map-reduce style views
- Master/slave replication for redundancy and horizontal read scaling
- Interface via Lisp methods and a Prolog-like language
- Compatible with: SBCL >= 1.045, LispWorks, Clozure CL

### Inspiration:
The design draws inspiration from CouchDB, Neo4j, and AllegroGraph.

## 7-Layer Architecture

```
VIVACEGRAPH - MODULAR LAYERED ARCHITECTURE
=============================================

┌─────────────────────────────────────────────────────────────────────┐
│                     LAYER 7: API & APPLICATIONS                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  REST         PROLOG        TRAVERSE      INTERFACE      EXAMPLE    │
│  (HTTP API)   (Queries)     (Graph)       (Methods)      (Demo)     │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│                    LAYER 6: DATA SYSTEM                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  VERTEX         EDGE           SCHEMA         VIEWS                 │
│  (Nodes)        (Edges)        (Definitions)  (Projections)         │
│    │              │                │               │                 │
│    └──────────────┴────────────────┴───────────────┘                 │
│                     PRIMITIVE-NODE                                   │
│               (Base data class)                                      │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│              LAYER 5: INDEXING & SEARCH                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  VE-INDEX      VEV-INDEX      TYPE-INDEX     FUNCTOR                │
│  (Vertex→      (Vertex→       (by Type)      (Predicates)           │
│   Edge)         Vertex→                                              │
│                 Edge)                                                │
│    │              │              │              │                    │
│    └──────────────┴──────────────┴──────────────┘                    │
│           INDEX-LIST (Value Indexes)                                 │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│           LAYER 4: PERSISTENT DATA STRUCTURES                       │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  SKIP-LIST       SKIP-LIST-CURSORS    LINEAR-HASH      ALLOCATOR   │
│  (Ordered        (Iteration)          (Hash Table)     (Memory)     │
│   indexes)                                                           │
│                                                                       │
│        BUFFER-POOL      SERIALIZE         NODE-ID                   │
│        (Cache)          (Serialization)   (Identifiers)              │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│         LAYER 3: PERSISTENCE & TRANSACTIONS                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  TRANSACTIONS      TRANSACTION-RESTORE    TRANSACTION-STREAMING     │
│  (Txn Control)     (Recovery)             (Txn Replication)         │
│                                                                       │
│         BACKUP          REPLICATION       TXN-LOG                   │
│         (Snapshots)     (Master/Slave)    (Txn Log)                 │
│                                                                       │
│  TRANSACTION-LOG-STREAMING    GC                                     │
│  (Replication stream)         (Garbage Collection)                   │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│        LAYER 2: MEMORY & SYNCHRONIZATION                            │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  PCONS         PMEM          PSTRUCT      MMAP                      │
│  (Persistent   (Memory       (Structs)    (Memory Mapped           │
│   Cons Cells)   Model)                     Files)                    │
│                                                                       │
│     RW-LOCK       QUEUE       MAILBOX      CURSORS                  │
│     (Mutex)       (Queues)    (IPC)        (Iterators)              │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│       LAYER 1: INFRASTRUCTURE & BASE UTILITIES                      │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  PACKAGE      GLOBALS       CONDITIONS      UTILITIES                │
│  (Packages)   (Global Vars) (Exceptions)    (Helpers)               │
│                                                                       │
│         CLOS          UUID          RANDOM        STATS              │
│         (CLOS exts)   (Ids)         (RNG)         (Metrics)         │
│                                                                       │
│                   GRAPH-CLASS  NODE-CLASS                            │
│                   (Main class definitions)                           │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

### Detailed Description of Each Layer

#### LAYER 1: Infrastructure & Base Utilities

This is the **foundational layer** upon which everything else is built.

**Components:**

| File | Purpose | Responsibility |
|------|---------|----------------|
| `package.lisp` | Defines the `:graph-db` package | Namespace and exports |
| `globals.lisp` | System global variables | Shared global state |
| `conditions.lisp` | Exceptions and conditions | Custom error handling |
| `utilities.lisp` | Common utility functions | Reusable helpers |
| `clos.lisp` | Custom CLOS extensions | Metaclasses and MOP (Meta-Object Protocol) |
| `uuid.lisp` | Unique identifier generation | UUID creation for nodes/edges |
| `random.lisp` | Random number generator | Randomization (if needed) |
| `stats.lisp` | Metrics and statistics | Performance data collection |
| `graph-class.lisp` | GRAPH class definition | Main graph structure |
| `node-class.lisp` | Metaclass for persistent nodes | CLOS base for nodes |

**Entry point:** `package.lisp` → `globals.lisp` → `conditions.lisp` → `utilities.lisp`

#### LAYER 2: Memory & Synchronization

**Purpose:** Manage persistent memory and inter-process synchronization.

**Components:**

| File | Purpose | Responsibility |
|------|---------|----------------|
| `pcons.lisp` | Persistent Cons Cells | Persistent Lisp cells in memory |
| `pmem.lisp` | Persistent Memory Model | Persistent memory model |
| `pstruct.lisp` | Persistent Structures | Persistent structures |
| `mmap.lisp` | Memory Mapped Files | File-to-memory mapping |
| `rw-lock.lisp` | Read-Write Locks | Reader-writer synchronization |
| `queue.lisp` | Thread-safe queues | Queues for asynchronous communication |
| `mailbox.lisp` | Message Passing | Message mailboxes for IPC |
| `cursors.lisp` | General iterators | Abstraction for traversing structures |

**Key feature:** Memory is **persistent** and **file-mapped**, allowing data to survive between sessions.

#### LAYER 3: Persistence & Transactions

**Purpose:** Guarantee ACID-compliance and data recoverability.

**Components:**

| File | Purpose | Responsibility |
|------|---------|----------------|
| `transactions.lisp` | Transaction control | Begin, commit, rollback |
| `transaction-restore.lisp` | Txn recovery | Restore from transaction logs |
| `transaction-streaming.lisp` | Txn streaming | Real-time transaction replication |
| `transaction-log-streaming.lisp` | Log streaming | Transaction log streaming |
| `txn-log.lisp` | Transaction Log | Persistent log of all transactions |
| `backup.lisp` | Snapshots | Point-in-time backups |
| `replication.lisp` | Master/Slave Replication | Replication for redundancy |
| `gc.lisp` | Garbage Collection | Cleanup of unreferenced data |

**Key concept:** All operations are recorded in a log to recover state in case of failure.

#### LAYER 4: Persistent Data Structures

**Purpose:** Provide efficient data structures that support persistence.

**Components:**

| File | Purpose | Responsibility |
|------|---------|----------------|
| `skip-list.lisp` | Skip Lists | Ordered structures for indexes |
| `skip-list-cursors.lisp` | Skip List Iterators | Iteration over skip lists |
| `linear-hash.lisp` | Persistent Hash Table | Dynamic and persistent hash table |
| `allocator.lisp` | Memory Allocator | Persistent memory allocator |
| `buffer-pool.lisp` | Buffer Pool | In-memory object cache |
| `serialize.lisp` | Serialization | Object-to/from-bytes conversion |
| `node-id.lisp` | Node Identifiers | Unique ID management for nodes |
| `index.lisp` | General Indexes | Base abstraction for indexes |
| `index-list.lisp` | List-Based Indexes | Indexes on value attributes |
| `index-vector.lisp` | Vector Indexes | Indexes for vector searches |

**Critical point:** Skip lists enable O(log n) search even on persistent data.

#### LAYER 5: Indexing & Search

**Purpose:** Provide efficient graph search through specialized indexes.

**Components:**

| File | Purpose | Responsibility |
|------|---------|----------------|
| `ve-index.lisp` | Vertex→Edge Index | Index of which edges enter/leave each vertex |
| `vev-index.lisp` | Vertex→Edge→Vertex Index | Direct connectivity index between vertices |
| `type-index.lisp` | Type-based Index | Index of vertices/edges by type |
| `functor.lisp` | Functors | Predicates for queries |
| `prolog-functors.lisp` | Prolog Predicates | Prolog-style predicates |
| `prologc.lisp` | Prolog Compiler | Prolog engine compiler |

**Specialized indexes:**
- **VE-Index:** `(vertex-id) → list of edge-ids` (inbound and outbound)
- **VEV-Index:** `(vertex-id-1, vertex-id-2) → edge-id` (direct access)
- **Type-Index:** `(type) → list of node-ids` (type-based filtering)

#### LAYER 6: Data System

**Purpose:** Define the object model (vertices and edges) that form the graph.

**Components:**

| File | Purpose | Responsibility |
|------|---------|----------------|
| `vertex.lisp` | Vertex Class | Node/vertex definition |
| `edge.lisp` | Edge Class | Edge/relationship definition |
| `primitive-node.lisp` | Node Base Class | Persistent base class for all nodes |
| `schema.lisp` | Schema Definition | Type and property definitions |
| `views.lisp` | Map-Reduce Views | Graph projections and views |

**Structure:**
```
PRIMITIVE-NODE (persistent base class)
    ↓
    ├─→ VERTEX (graph nodes)
    └─→ EDGE (graph edges)
```

**Vertex properties:**
- `id`: Unique identifier (UUID)
- `type-id`: Node type (for polymorphism)
- `revision`: Version number
- `deleted-p`: Logical deletion flag
- Custom data according to schema

**Edge properties:**
- All Vertex properties, plus:
- `from`: Source vertex ID
- `to`: Destination vertex ID
- `weight`: Numeric weight (float)

#### LAYER 7: API & Applications

**Purpose:** Provide data access interfaces.

**Components:**

| File | Purpose | Responsibility |
|------|---------|----------------|
| `interface.lisp` | Lisp methods API | CLOS functions to manipulate graph |
| `traverse.lisp` | Graph Traversal | Traversal algorithms (BFS, DFS) |
| `rest.lisp` | REST API | HTTP server with Hunchentoot |
| `prologc.lisp` + `prolog-functors.lisp` | Prolog Interface | Prolog-like query language |
| `example.lisp` | Usage examples | Demonstrations |

**Ways to use VivaceGraph:**
1. **Lisp methods:** `(add-vertex graph :type "User" :name "Alice")`
2. **REST API:** `POST /api/vertices { "type": "User", "name": "Alice" }`
3. **Prolog:** `?- vertex(X), type(X, 'User')`

## Vertical Dependencies

Dependencies always go **upward** (from lower layers to higher layers).

```
DEPENDENCY FLOW
=====================

Layer 7 (API)     ← Depends on: Layer 6, 5, 4
                    Needs: methods for access, indexes for search,
                           structures for storage

Layer 6 (Data)    ← Depends on: Layer 5, 4, 3, 1
                    Needs: indexes for vertex/edge search,
                           transactions, infrastructure

Layer 5 (Indexes) ← Depends on: Layer 4, 1
                    Needs: data structures, base utilities

Layer 4 (DS)      ← Depends on: Layer 3, 2, 1
                    Needs: persistence, memory, infrastructure

Layer 3 (Txn)     ← Depends on: Layer 2, 1
                    Needs: synchronization, utilities

Layer 2 (Mem)     ← Depends on: Layer 1
                    Needs: utilities, infrastructure

Layer 1 (Base)    ← Does not depend on anything internal
                    It is the ROOT of the dependency tree
```

**Fundamental rule:** There are never circular dependencies.

## Special Files

Some files are **outside the main flow** or have cross-cutting roles:

### Outside the flow:

| File | Role |
|------|------|
| `chart.lisp` | Graph visualization (no internal dependencies) |
| `example.lisp` | Usage examples |
| `test.lisp` | Unit tests |
| `test-lhash.lisp` | Linear-hash tests |
| `xach-test.lisp` | Additional tests |

### Demo directory (`/demo/`):

```
social-shopping/ (demo project)
├─ social-shopping.asd
├─ package.lisp
├─ schema.lisp
├─ collections.lisp
├─ customer.lisp
├─ want-list.lisp
└─ graph-views.lisp
```

This is a complete example of how to use VivaceGraph for a social e-commerce application.

## Initialization Flow

When you load VivaceGraph in SBCL/Clozure, the ASDF system loads files in this **exact order**:

```
LOAD ORDER (defined in graph-db.asd)
==========================================

1. package.lisp ...................... :graph-db package
2. globals.lisp ...................... Global variables
3. conditions.lisp ................... Exceptions
4. utilities.lisp .................... Utility functions
5. queue.lisp ........................ Queues
6. mailbox.lisp ...................... Message mailboxes
7. rw-lock.lisp ...................... Locks (if SBCL/LispWorks)
8. mmap.lisp ......................... Memory-mapped files
9. pcons.lisp ........................ Persistent cons cells
10. node-id.lisp ..................... ID generation
11. buffer-pool.lisp ................. Buffer pool/cache
12. serialize.lisp ................... Serialization
13. linear-hash.lisp ................. Persistent hash table
14. allocator.lisp ................... Memory allocator
15. graph-class.lisp ................. GRAPH class
16. cursors.lisp ..................... Iterators
17. skip-list.lisp ................... Skip lists for indexes
18. skip-list-cursors.lisp ........... Skip list iterators
19. index-list.lisp .................. List-based indexes
20. ve-index.lisp .................... Vertex→Edge indexes
21. vev-index.lisp ................... Vertex→Edge→Vertex indexes
22. type-index.lisp .................. Type-based indexes
23. graph.lisp ....................... Complete GRAPH class with methods
24. stats.lisp ....................... Statistics
25. schema.lisp ...................... Schema definition
26. node-class.lisp .................. Node metaclass
27. views.lisp ....................... Map-reduce views
28. primitive-node.lisp .............. Persistent node base class
29. vertex.lisp ...................... VERTEX class
30. edge.lisp ........................ EDGE class
31. gc.lisp .......................... Garbage collection
32. transactions.lisp ................ Transaction control
33. transaction-restore.lisp ......... Txn recovery
34. transaction-log-streaming.lisp ... Log streaming
35. transaction-streaming.lisp ....... Txn replication
36. backup.lisp ...................... Snapshots
37. replication.lisp ................. Master/slave replication
38. txn-log.lisp ..................... Transaction log
39. functor.lisp ..................... Query functors
40. prologc.lisp ..................... Prolog compiler
41. prolog-functors.lisp ............. Prolog predicates
42. interface.lisp ................... Lisp methods API
43. traverse.lisp .................... Graph traversal algorithms
44. rest.lisp ........................ REST API

DEMO:
social-shopping.asd (separate project depending on graph-db)
```

**Important note:** This order is **critical**. If you change the order, there may be unresolved dependency errors.

## Recommended Reading Guide

Before documenting or modifying VivaceGraph, read in this order:

### Phase 1: Fundamentals (2-3 hours)
1. `package.lisp` - Understand the namespace
2. `globals.lisp` - Shared variables
3. `utilities.lisp` - Common helpers
4. `conditions.lisp` - Error handling

**Goal:** Understand the base infrastructure.

### Phase 2: Memory (2-3 hours)
5. `node-id.lisp` - ID system
6. `pcons.lisp` - Persistent structures
7. `mmap.lisp` - Memory mapping
8. `rw-lock.lisp` - Synchronization

**Goal:** How data is stored and synchronized.

### Phase 3: Persistence (3-4 hours)
9. `serialize.lisp` - Conversion to/from disk
10. `allocator.lisp` - Memory management
11. `buffer-pool.lisp` - RAM cache
12. `transactions.lisp` - ACID control

**Goal:** How durability is guaranteed.

### Phase 4: Data Structures (3-4 hours)
13. `linear-hash.lisp` - Hash tables
14. `skip-list.lisp` - Ordered lists
15. `index.lisp` - Index abstraction
16. `index-list.lisp` - Value indexes

**Goal:** What internal structures it uses to be fast.

### Phase 5: Graph Indexes (2-3 hours)
17. `ve-index.lisp` - Edges by vertex
18. `vev-index.lisp` - Vertex-to-vertex connectivity
19. `type-index.lisp` - Type-based search

**Goal:** How graph search is optimized.

### Phase 6: Data Model (2-3 hours)
20. `node-class.lisp` - Base metaclass
21. `vertex.lisp` - Nodes
22. `edge.lisp` - Edges
23. `schema.lisp` - Node types

**Goal:** What you can store.

### Phase 7: User Interface (2-3 hours)
24. `interface.lisp` - Lisp methods
25. `traverse.lisp` - Traversals
26. `prologc.lisp` - Prolog language
27. `rest.lisp` - REST API

**Goal:** How data is accessed.

**Total recommended time:** 20-25 hours of careful reading.

## Key Points on Modularization

### ✓ Strengths

1. **Well-defined layered architecture**
   - Each layer has clear responsibilities
   - Dependencies are unidirectional
   - There are no dependency cycles

2. **Separation of concerns**
   - Memory ↔ Persistence ↔ Indexes ↔ Data ↔ API
   - Each part can be studied in isolation

3. **Explicit load order**
   - The `graph-db.asd` file perfectly defines dependencies
   - ASDF guarantees they are loaded in the correct order

4. **Reuse**
   - Lower layers (indexes, structures) are reused in multiple contexts
   - The design favors composition

### ✗ Areas for Improvement

1. **Lack of subdirectories**
   - 52 `.lisp` files in the root
   - It would be clearer with: `mem/`, `persist/`, `index/`, `data/`, `api/`

2. **Inconsistent naming**
   - Missing common prefix for related names
   - For example: `buffer-pool.lisp` vs `skip-list.lisp` vs `ve-index.lisp`
   - Better would be: `mem-buffer-pool.lisp`, `ds-skip-list.lisp`, `idx-ve-index.lisp`

3. **Some files with multiple responsibilities**
   - `graph.lisp` contains main GRAPH methods
   - `transactions.lisp` mixes txn control with recovery
   - Could be split into more than one file

4. **Non-existent documentation**
   - Very few architecture comments
   - Missing docstrings in many functions
   - README is minimal

### Recommendation for Documentation

**Do not reorganize the code.** Instead:

1. **Create a documentation structure** that mirrors the layers:
   ```
   docs/
   ├─ 01-FUNDAMENTALS.md       (Layer 1)
   ├─ 02-MEMORY.md              (Layer 2)
   ├─ 03-TRANSACTIONS.md        (Layer 3)
   ├─ 04-DATA-STRUCTURES.md     (Layer 4)
   ├─ 05-INDEXING.md            (Layer 5)
   ├─ 06-DATA-MODEL.md          (Layer 6)
   ├─ 07-API.md                 (Layer 7)
   ├─ ARCHITECTURE.md           (this document)
   ├─ GETTING-STARTED.md
   └─ API-REFERENCE.md
   ```

2. **In each document**, map which files belong to which concept

3. **Create a cross-reference index** file → concept

4. **Add diagrams** specific to each subsystem

## Complete File Structure

```
VivaceGraph/ (root)
│
├─ graph-db.asd ..................... ASDF system definition
├─ README.md ........................ General description
├─ example.lisp ..................... Usage example
│
├─ LAYER 1: Infrastructure
│  ├─ package.lisp
│  ├─ globals.lisp
│  ├─ conditions.lisp
│  ├─ utilities.lisp
│  ├─ clos.lisp
│  ├─ uuid.lisp
│  ├─ random.lisp
│  ├─ stats.lisp
│  ├─ graph-class.lisp
│  └─ node-class.lisp
│
├─ LAYER 2: Memory & Synchronization
│  ├─ pcons.lisp
│  ├─ pmem.lisp
│  ├─ pstruct.lisp
│  ├─ mmap.lisp
│  ├─ rw-lock.lisp
│  ├─ queue.lisp
│  ├─ mailbox.lisp
│  └─ cursors.lisp
│
├─ LAYER 3: Persistence & Transactions
│  ├─ transactions.lisp
│  ├─ transaction-restore.lisp
│  ├─ transaction-streaming.lisp
│  ├─ transaction-log-streaming.lisp
│  ├─ txn-log.lisp
│  ├─ backup.lisp
│  ├─ replication.lisp
│  └─ gc.lisp
│
├─ LAYER 4: Persistent Data Structures
│  ├─ skip-list.lisp
│  ├─ skip-list-cursors.lisp
│  ├─ linear-hash.lisp
│  ├─ allocator.lisp
│  ├─ buffer-pool.lisp
│  ├─ serialize.lisp
│  ├─ node-id.lisp
│  ├─ index.lisp
│  ├─ index-list.lisp
│  └─ index-vector.lisp
│
├─ LAYER 5: Indexing & Search
│  ├─ ve-index.lisp
│  ├─ vev-index.lisp
│  ├─ type-index.lisp
│  ├─ functor.lisp
│  ├─ prolog-functors.lisp
│  └─ prologc.lisp
│
├─ LAYER 6: Data System
│  ├─ vertex.lisp
│  ├─ edge.lisp
│  ├─ primitive-node.lisp
│  ├─ schema.lisp
│  └─ views.lisp
│
├─ LAYER 7: API & Applications
│  ├─ interface.lisp
│  ├─ traverse.lisp
│  └─ rest.lisp
│
├─ Utilities & Tests
│  ├─ chart.lisp ..................... Visualization
│  ├─ test.lisp ...................... General tests
│  ├─ test-lhash.lisp ................ Hash tests
│  └─ xach-test.lisp ................. Additional tests
│
└─ demo/ ............................. Demo project
   ├─ social-shopping.asd
   ├─ package.lisp
   ├─ schema.lisp
   ├─ collections.lisp
   ├─ customer.lisp
   ├─ want-list.lisp
   └─ graph-views.lisp
```

## Executive Summary

**VivaceGraph** is an ACID graph database written in Common Lisp, with a well-structured 7-layer architecture:

1. **Base infrastructure** - Namespace, variables, exceptions, utilities
2. **Memory and synchronization** - Persistence, locks, queues
3. **Transactions** - ACID guarantees, logs, recovery
4. **Data structures** - Hash tables, skip lists, serialization
5. **Specialized indexing** - Graph indexes (VE, VEV, by type)
6. **Data model** - Vertices, edges, schemas
7. **Access APIs** - Lisp methods, Prolog, REST

The apparent "lack of modularization" (52 files in root) is **cosmetic**. The architecture is actually **very well structured** with clear, unidirectional dependencies.

**For documenting:** Read following the 7 recommended phases (20-25 hours of careful reading) and organize documentation by layers.

*Document generated to facilitate VivaceGraph documentation*
*Architecture analyzed: March 2026*
