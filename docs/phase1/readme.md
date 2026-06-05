# VivaceGraph


[español](esReadme.md) | [english](readme.md)

<br><br>



## Documentation

**VivaceGraph** is an ACID graph database written **100% in pure Common Lisp**, with no external system dependencies (except standard Lisp libraries). It implements:

- **Full ACID** - Transactions with isolation, durability
- **Persistence** - Memory-mapped files with crash recovery
- **Indexes** - Skip-lists, linear hashing, type-based indexing
- **Logic Programming** - Full Prolog engine based on PAIP
- **Dynamic Types** - Runtime-extensible type system
- **REST API** - HTTP server with authentication
- **Replication** - Master-slave (prototype)
- **Views** - Materialized views with map-reduce

**Size:** ~12,200 lines of Core code  
**Layers:** 7 levels of modular architecture  
**Documentation:** 8,600+ lines  

## Integrated Table of Contents

### General Architecture

1. **[Architecture](./enArchitecture.md)**
   - Overview of the 7-layer system
   - Load order and dependencies
   - Initialization flow
   - Architectural patterns

### Layer 1: Infrastructure (1,685 lines)

**Documented Files:**
- `package.lisp` - Package and exports
- `globals.lisp` - Global constants
- `conditions.lisp` - Exception system
- `utilities.lisp` - Utilities (UUID, time, locks)
- `clos.lisp` - node-class metaclass
- `uuid.lisp` - UUID operations
- `random.lisp` - Mersenne Twister PRNG
- `stats.lisp` - Read/write statistics
- `graph-class.lisp` - Graph class and subgraphs
- `node-class.lisp` - Node metaclass

**Documentation:** 
[INFRASTRUCTURE](./en01-LAYER1-INFRA.md)

**Responsibilities:**
- Define package and exports
- Configuration constants
- Exceptions and error handling
- Generic utilities (UUID, time, locks)
- CLOS metaclasses for nodes
- Graph class and variants (master/slave)

### Layer 2: Memory & Synchronization (702 lines)

**Documented Files:**
- `pcons.lisp` - Persistent cons cells
- `pmem.lisp` - Persistent memory model
- `pstruct.lisp` - DSL for persistent structures
- `mmap.lisp` - Memory-mapped files
- `rw-lock.lisp` - Reader-writer locks
- `queue.lisp` - FIFO queues
- `mailbox.lisp` - IPC mailboxes
- `cursors.lisp` - Iterator interface

**Documentation:** 
[SYNCHRONIZATION](./en02-LAYER2-MEMORY-SYNCHRONIZATION.md)

**Responsibilities:**
- Persistent cons cells (pcons)
- Memory model with stack/heap
- Memory-mapped file I/O
- Reader-writer locks for concurrency
- FIFO queues
- Message passing (mailbox)
- Iterator interface


### Layer 3: Persistence & Transactions (2,336 lines)

**Documented Files:**
- `transactions.lisp` - Core ACID implementation
- `transaction-restore.lisp` - Snapshot restoration
- `transaction-streaming.lisp` - Master-slave replication
- `transaction-log-streaming.lisp` - Log management
- `txn-log.lisp` - Transaction logging
- `backup.lisp` - Snapshot creation
- `replication.lisp` - Replication (stub)
- `gc.lisp` - Mark-and-sweep garbage collection

**Documentation:** 
[PERSISTENCE & TRANSACTIONS](./en03-LAYER3-PERSISTENCY-TRANSACTION.md)

**Responsibilities:**
- ACID transactions with optimistic locking
- Transaction lifecycle management
- Snapshot creation & restoration
- Replication protocol (master-slave)
- Log-based durability
- Garbage collection

### Layer 4: Data Structures (3,501 lines)

**Documented Files:**
- `skip-list.lisp` - Ordered indexes O(log n)
- `skip-list-cursors.lisp` - Skip-list iterators
- `linear-hash.lisp` - Dynamic hash table
- `allocator.lisp` - Memory manager with bins
- `buffer-pool.lisp` - Reusable object pool
- `serialize.lisp` - Type-agnostic serialization
- `node-id.lisp` - UUID v5 generation
- `index.lisp` - Skip-list wrapper
- `index-list.lisp` - Persistent lists
- `index-vector.lisp` - Dynamic vectors

**Documentation:** 
[DATA STRUCTURES](./en04-LAYER4-DATA-STRUCTURES.md)

**Responsibilities:**
- Skip lists for O(log n) search
- Linear hashing for dynamic growth
- Memory allocator with fragmentation
- Buffer pool to avoid GC
- Type-extensible serialization
- UUID v5 generation

### Layer 5: Indexing (1,996 lines)

**Documented Files:**
- `ve-index.lisp` - Vertex→edges index
- `vev-index.lisp` - V1→edge→V2 index
- `type-index.lisp` - Type-based index
- `functor.lisp` - Prolog predicates
- `views.lisp` - Materialized views
- `prologc.lisp` - Full Prolog engine

**Documentation:** 
[INDEXING](./en05-LAYER5-INDEXING.md)

**Responsibilities:**
- Specialized indexes for fast search
- VE-Index: O(log n) vertex edges
- VEV-Index: O(log n) specific edge
- Type-Index: O(1) nodes by type
- Prolog engine with unification and backtracking
- Materialized map-reduce views


### Layer 6: Data Model (1,351 lines)

**Documented Files:**
- `primitive-node.lisp` - Base Node class
- `vertex.lisp` - Edgeless nodes
- `edge.lisp` - Directed edges
- `schema.lisp` - Dynamic type system

**Documentation:** 
[DATA MODEL](./en06-LAYER6-DATA-MODEL.md)

**Responsibilities:**
- Primitive node with flags and serialization
- Vertices as simple nodes
- Edges with from/to/weight
- Extensible type system (def-vertex/def-edge)
- Automatic CRUD
- Lazy data loading

### Layer 7: User API (517 lines)

**Documented Files:**
- `interface.lisp` - CRUD generics
- `traverse.lisp` - BFS traversal
- `rest.lisp` - HTTP REST server

**Documentation:** 
[USER API](./en07-LAYER-USER-API.md)

**Responsibilities:**
- copy, save, mark-deleted generics
- BFS graph traversal
- REST HTTP server with JSON
- htpasswd authentication
- Schema introspection

## Architecture Diagram

```
VIVACEGRAPH - 7 LAYERS
════════════════════════════════════════════════════════════

┌─────────────────────────────────────────────────────────┐
│ Layer 7: USER API (517 lines)                           │
│ └─ REST HTTP, BFS Traversal, generic Interface          │
├─────────────────────────────────────────────────────────┤
│ Layer 6: DATA MODEL (1,351 lines)                       │
│ └─ Node, Vertex, Edge, dynamic Schema                   │
├─────────────────────────────────────────────────────────┤
│ Layer 5: INDEXING (1,996 lines)                         │
│ └─ VE/VEV/Type indexes, Prolog, Views                   │
├─────────────────────────────────────────────────────────┤
│ Layer 4: DATA STRUCTURES (3,501 lines)                  │
│ └─ Skip-lists, Linear hash, Allocator, Serialization    │
├─────────────────────────────────────────────────────────┤
│ Layer 3: PERSISTENCE & TRANSACTIONS (2,336 lines)       │
│ └─ ACID, Backup, Replication, GC                        │
├─────────────────────────────────────────────────────────┤
│ Layer 2: MEMORY & SYNCHRONIZATION (702 lines)           │
│ └─ Mmap, RW-locks, Pcons, Mailbox                       │
├─────────────────────────────────────────────────────────┤
│ Layer 1: INFRASTRUCTURE (1,685 lines)                   │
│ └─ Package, constants, UUID, Prolog vars, stats         │
├─────────────────────────────────────────────────────────┤
│ TOTAL: 12,088 lines of CORE code                       │
│        8,652 lines of documentation                     │
│        100% pure Common Lisp                            │
└─────────────────────────────────────────────────────────┘
```

## Quick Topic Search

### ACID Transactions
- **File:** `transactions.lisp` (Layer 3)
- **Documentation:** [PERSISTENCE & TRANSACTIONS](./en03-LAYER3-PERSISTENCY-TRANSACTION.md)
- **Key concepts:**
  - Optimistic locking with read-set/write-set
  - Serialization isolation
  - Log-based durability
  - Crash recovery
  - Automatic retry (max 8 attempts)

### Indexes and Search
- **Files:** `skip-list.lisp`, `ve-index.lisp`, `vev-index.lisp`, `type-index.lisp` (Layers 4-5)
- **Documentation:** [DATA STRUCTURES](./en04-LAYER4-DATA-STRUCTURES.md), [INDEXING](./en05-LAYER5-INDEXING.md)
- **Complexities:**
  - Skip-list search: O(log n)
  - VE-index: O(log n)
  - VEV-index: O(log n)
  - Type-index: O(1)

### Logic Programming (Prolog)
- **Files:** `prologc.lisp`, `functor.lisp` (Layer 5)
- **Documentation:** [INDEXING](./en05-LAYER5-INDEXING.md)
- **Features:**
  - Unification with variables
  - Backtracking with trail
  - Clausification and compilation
  - Graph predicates

### Type System
- **Files:** `schema.lisp`, `node-class.lisp` (Layer 6, Layer 1)
- **Documentation:** [PERSISTENCE & TRANSACTIONS](./en03-LAYER3-PERSISTENCY-TRANSACTION.md)
- **Macros:** `def-vertex`, `def-edge`
- **Extensibility:** Runtime, no recompilation needed

### Master-Slave Replication
- **Files:** `transaction-streaming.lisp`, `replication.lisp` (Layer 3)
- **Documentation:** [PERSISTENCE & TRANSACTIONS](./en03-LAYER3-PERSISTENCY-TRANSACTION.md)
- **Protocol:** Binary packets, authentication, compression

### REST API
- **File:** `rest.lisp` (Layer 7)
- **Documentation:** [USER API](./en07-LAYER-USER-API.md)
- **Endpoints:** Full CRUD, JSON encoding, authentication


## Project Statistics

### Lines of Code

```
Layer 1 (Infrastructure):              1,685 lines
Layer 2 (Memory & Sync):                 702 lines
Layer 3 (Persistence & TX):            2,336 lines
Layer 4 (Data Structures):             3,501 lines
Layer 5 (Indexing):                    1,996 lines
Layer 6 (Data Model):                  1,351 lines
Layer 7 (User API):                      517 lines
────────────────────────────────────────────────────
TOTAL:                                 12,088 lines
```

### Documentation

```
General architecture:                    442 lines
Layer 1:                               1,156 lines
Layer 2:                                 849 lines
Layer 3:                               1,547 lines
Layer 4:                               1,823 lines
Layer 5:                               1,365 lines
Layer 6:                               1,471 lines
Layer 7:                                 948 lines
────────────────────────────────────────────────────
TOTAL:                                 8,652 lines
```

### Algorithm Complexities

```
Operation               Worst Case   Average      Space
────────────────────────────────────────────────────────
Vertex lookup           O(n)         O(1)         O(1)
Edge lookup (VEV)       O(n)         O(1)         O(1)
Skip-list search        O(n)         O(log n)     O(log n)
Skip-list insert        O(n)         O(log n)     O(log n)
Type lookup             O(1)         O(1)         O(1)
BFS Traversal           O(V+E)       O(V+E)       O(V)
Transaction commit      O(n)         O(log n)     O(n)
```

## Getting Started

### Recommended Reading Order

1. **First:** Read [Architecture](./esArchitecture.md) for the overall picture
2. **Then:** Layers 1-3 to understand the base infrastructure
3. **Next:** Layer 4 for efficient data structures
4. **After that:** Layers 5-6 for the graph model
5. **Finally:** Layer 7 for the user API

### By Specific Interest

**If you want to understand ACID:**
- Read [PERSISTENCE & TRANSACTIONS](./en03-LAYER3-PERSISTENCY-TRANSACTION.md)
- Then [SYNCHRONIZATION](./en02-LAYER2-MEMORY-SYNCHRONIZATION.md) for locks

**If you want to understand Prolog:**
- Read [Layer 5](./en05-LAYER5-INDEXING.md)
- Specifically the "Prolog Engine" section

**If you want to use the system:**
- Read [Layer 7](./en07-LAYER-USER-API.md)
- Then [Layer 6](./en06-LAYER6-DATA-MODEL.md) for types

**If you want to understand performance:**
- Read [Layer 4](./en04-LAYER4-DATA-STRUCTURES.md) for data structures
- Then [Layer 5](./en05-LAYER5-INDEXING.md) for indexes


## Cross-References

### Modules by Functionality

**Concurrency:**
- `rw-lock.lisp` (Layer 2) - Locks
- `queue.lisp` (Layer 2) - Queues
- `mailbox.lisp` (Layer 2) - IPC

**Persistence:**
- `mmap.lisp` (Layer 2) - Memory-mapped files
- `allocator.lisp` (Layer 4) - Memory manager
- `transactions.lisp` (Layer 3) - Transactions

**Indexes:**
- `skip-list.lisp` (Layer 4) - Skip lists
- `linear-hash.lisp` (Layer 4) - Dynamic hash
- `ve-index.lisp` (Layer 5) - Vertex-edge index
- `vev-index.lisp` (Layer 5) - Vertex-edge-vertex index
- `type-index.lisp` (Layer 5) - Type-based index

**Graph:**
- `vertex.lisp` (Layer 6) - Vertices
- `edge.lisp` (Layer 6) - Edges
- `schema.lisp` (Layer 6) - Dynamic types

**API:**
- `traverse.lisp` (Layer 7) - BFS traversal
- `rest.lisp` (Layer 7) - HTTP server
- `interface.lisp` (Layer 7) - CRUD generics

## Key Concepts

### Skip Lists
Probabilistic structure providing O(log n) search without rebalancing.
Used for: Ordered indexes, query results.
**Read:** [Layer 4 - Skip Lists](./en04-LAYER4-DATA-STRUCTURES.md)

### Linear Hashing
Hash table that grows incrementally without full rehashing.
Used for: Vertex/edge tables.
**Read:** [Layer 4 - Linear Hashing](./en04-LAYER4-DATA-STRUCTURES.md)

### MVCC (Multi-Version Concurrency Control)
Each node has a revision; multiple readers see a consistent version.
Used for: Transaction isolation.
**Read:** [Layer 3 - ACID](./en03-LAYER3-PERSISTENCY-TRANSACTION.md)

### Prolog Logic Programming
Declarative language with unification and backtracking.
Used for: Complex graph queries.
**Read:** [INDEXING](./en05-LAYER5-INDEXING.md)

### Materialized Views
Automatically updated query caches.
Used for: Optimizing frequent queries.
**Read:** [INDEXING](./en05-LAYER5-INDEXING.md)

## Implementation Notes

### External Dependencies

VivaceGraph depends ONLY on standard Lisp libraries:

- **Bordeaux-threads** - Threading portability (SBCL, CCL, LispWorks)
- **local-time** - Date/time handling
- **CFFI** - C interface (for mmap)
- **Ironclad** - SHA1 for UUID v5
- **Ningle** - Web framework (optional, for REST)
- **Clack** - Lisp WSGI (optional, for REST)

**Does NOT use:**
- PostgreSQL, MongoDB, Redis, etc.
- Python, Java, Go, etc.
- External compilers
- Separate servers

## Key Learnings from the Project

1. **Layered Architecture** - Each layer independent, clear responsibilities
2. **Persistence in Lisp** - Memory-mapped files + CLOS = database
3. **Skip Lists vs B-Trees** - Trade-offs of each structure
4. **ACID Transactions** - Optimistic locking, read-set/write-set
5. **Prolog in Lisp** - Unification, backtracking, trail management
6. **Specialized Indexes** - VE, VEV, Type indexes for graphs
7. **Master-Slave Replication** - Binary protocol, recovery
8. **REST in Lisp** - Ningle/Clack stack


**Last updated:** March 2026  
**Version:** 1.0 Documented  
**Total size:** 12,088 lines of code + 8,652 lines of documentation
