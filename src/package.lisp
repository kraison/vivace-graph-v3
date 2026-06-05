;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FILE: src/package.lisp
;; 
;; PURPOSE:
;;   Defines the :graph-db package namespace and declares all 166 publicly-
;;   accessible symbols that constitute VivaceGraph's public API contract.
;;
;; RESPONSIBILITY:
;;   1. Create the :graph-db package namespace (isolated from :cl-user)
;;   2. Import base libraries (CL, bordeaux-threads, local-time)
;;   3. Conditionally import MOP implementations (SBCL/CCL/LispWorks specific)
;;   4. Shadow conflicting MOP symbols to normalize behavior across Lisps
;;   5. Export 166 symbols organized into 6 functional domains:
;;      - Graph lifecycle management (make-graph, open-graph, etc.)
;;      - REST API endpoints (start-rest, stop-rest, etc.)
;;      - Transaction management (with-transaction, commit, rollback, etc.)
;;      - Schema & Type definitions (def-vertex, def-edge, etc.)
;;      - Vertex/Edge CRUD & traversal (make-vertex, lookup-edge, traverse, etc.)
;;      - Materialized views (def-view, map-view, etc.)
;;      - Prolog logic programming (?, ?-, unify, cut, etc.)
;;
;; ARCHITECTURE NOTES:
;;   This file is compiled FIRST (before any other Layer 1 files). When this
;;   file loads, the :graph-db package is created, and all subsequent files
;;   in Layer 1-7 use (in-package :graph-db) to register their definitions
;;   in this namespace.
;;
;;   WHY THIS MATTERS:
;;   - If you change the order of package.lisp in graph-db.asd, everything breaks
;;   - Package must exist before any code tries to (in-package :graph-db)
;;   - Exports are not checked for existence at compile-time (definitions come later)
;;
;; CROSS-LISP COMPATIBILITY:
;;   This file uses conditional compilation (#+ reader macros) to adapt to
;;   SBCL, CCL (Clozure CL), and LispWorks. Each Lisp has different MOP
;;   (Meta-Object Protocol) implementations:
;;
;;   SBCL:
;;     - Uses native :sb-mop (SBCL's MOP)
;;     - Exports RW-lock primitives (make-rw-lock, with-read-lock, etc.)
;;     - Imports SB-EXT:WORD (SBCL-specific extension)
;;
;;   CCL (Clozure CL):
;;     - Cannot use native CCL CLOS directly (incompatible with standard MOP)
;;     - Must use CLOSER-MOP library (portable MOP)
;;     - Shadows 11 MOP symbols to normalize CLOSER-MOP behavior
;;     - Does NOT export RW-lock primitives
;;
;;   LispWorks:
;;     - Uses native :clos module (mostly standard-compliant)
;;     - No shadowing imports needed
;;     - Does NOT export RW-lock primitives
;;
;; SIDE EFFECTS:
;;   - Creates package :graph-db in the Lisp image (compile-time)
;;   - Modifies symbol table (adds 166 exported symbols)
;;   - Conditionally imports 4-8 external modules depending on Lisp
;;   - Conditionally shadows 1-11 symbols depending on Lisp
;;
;; RISK ASSESSMENT:
;;   🔴 CRITICAL: If CLOSER-MOP behavior diverges from native CLOS,
;;      CCL code may behave differently from SBCL (silent semantic drift)
;;   🟡 HIGH: Exporting 166 symbols creates large API surface;
;;      future backward-compatibility burden (Phase 3)
;;   🟡 HIGH: Global variables (*graph*, *transaction*, etc.) exposed directly;
;;      no encapsulation protection against misuse
;;
;; TESTING:
;;   This file should be tested to ensure:
;;   1. Package :graph-db exists after load
;;   2. All 166 symbols are accessible via (find-symbol "NAME" :graph-db)
;;   3. No duplicate symbol exports
;;   4. Conditional shadowing succeeds (no missing symbols)
;;   5. MOP functions work consistently across SBCL, CCL, LispWorks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ==============================================================================
;; SECTION 1: CONTEXT SWITCH
;; ==============================================================================

;; Load this file in the CL-USER package context.
;; After defpackage is processed, all subsequent files will do
;; (in-package :graph-db) to register their code in the new package.
(in-package #:cl-user)

;; ==============================================================================
;; SECTION 2: PACKAGE DEFINITION & IMPORTS
;; ==============================================================================

;; WHY DEFPACKAGE?
;; ================
;; defpackage is the standard Lisp mechanism to:
;; 1. Create a new package (namespace)
;; 2. Declare which external packages it depends on (:use)
;; 3. Override conflicting symbol names (:shadowing-import-from)
;; 4. Export public symbols (:export)
;;
;; ALTERNATIVE (NOT USED): in-package + export calls are more dynamic
;; but less clear about intent. defpackage is declarative and preferred
;; for package initialization.

(defpackage #:graph-db
  
  ;; ---------------------------------------------------------------------------
  ;; SUBSECTION 2.1: BASE IMPORTS (:use declarations)
  ;; ---------------------------------------------------------------------------
  
  ;; WHY :use #:cl?
  ;; All Common Lisp code depends on standard symbols (defun, defclass, let, etc.)
  ;; Including :cl makes these available without prefixing (e.g., not #:cl:defun)
  (:use #:cl
        
        ;; WHY :use #:bordeaux-threads?
        ;; VivaceGraph uses multi-threaded access (Layer 2 has read-write locks).
        ;; bordeaux-threads is a cross-Lisp abstraction over threading:
        ;; - SBCL: wraps sb-threads
        ;; - CCL: wraps ccl:process
        ;; - LispWorks: wraps mp:process
        ;; By using bordeaux-threads, we avoid Lisp-specific thread code.
        #:bordeaux-threads
        
        ;; WHY :use #:local-time?
        ;; VivaceGraph needs timestamps for:
        ;; - Transaction timestamping (Layer 3)
        ;; - Replication timestamp ordering (Layer 3+)
        ;; - Expiry-based data management (future feature)
        ;; local-time provides a portable timestamp API across Lisps.
        #:local-time
        
        ;; ---------------------------------------------------------------------------
        ;; SUBSECTION 2.2: CONDITIONAL MOP IMPORTS
        ;; ---------------------------------------------------------------------------
        ;; WHY CONDITIONAL COMPILATION FOR MOP?
        ;; 
        ;; The Meta-Object Protocol (MOP) allows intercepting class creation,
        ;; instance allocation, slot access, etc. VivaceGraph uses MOP to:
        ;; - Define custom metaclasses (node-class, graph-class in Layer 1)
        ;; - Intercept slot-value operations (Layer 6)
        ;; - Track dirty slots for persistence (Layer 3-6)
        ;;
        ;; Problem: Each Lisp implements MOP differently.
        ;; - SBCL: has sb-mop (very complete)
        ;; - CCL: has native CLOS but incompatible with standard MOP
        ;; - LispWorks: has native clos module (standard-compliant)
        ;;
        ;; Solution: Import the MOP module specific to each Lisp.
        ;; Syntax: #+FEATURE import-this-only-on-FEATURE
        ;;         #-FEATURE import-unless-FEATURE
        
        ;; SBCL-specific: use native MOP
        ;;   sb-mop: SBCL's complete MOP implementation (classes, methods, slots)
        ;;   sb-pcl: SBCL's Portable Common Loops (legacy name, may be redundant)
        ;;           NOTE: In modern SBCL, sb-pcl is internal to sb-mop.
        ;;                 This may be unnecessary; verify with (find-package :sb-pcl).
        #+sbcl #:sb-mop
        #+sbcl #:sb-pcl
        
        ;; CCL-specific: use CLOSER-MOP (portable MOP library)
        ;;   CCL's native CLOS cannot be used directly because:
        ;;   - CCL CLOS has different method combination semantics
        ;;   - CCL CLOS slots behave differently
        ;;   - CCL CLOS doesn't support all standard MOP features
        ;;
        ;;   CLOSER-MOP is a library that provides:
        ;;   - Standard MOP interface on top of CCL's native CLOS
        ;;   - Emulates missing MOP features
        ;;   - Ensures CCL code behaves like SBCL code
        ;;
        ;;   Risk: If CLOSER-MOP version or semantics change, CCL behavior drifts
        ;;         from SBCL. Mitigated by shadowing imports (see below).
        #+ccl #:closer-mop
        
        ;; LispWorks-specific: use native CLOS
        ;;   LispWorks has a standard-compliant CLOS implementation.
        ;;   No CLOSER-MOP needed (LispWorks CLOS is already standard).
        #+lispworks #:clos)
  
  ;; ---------------------------------------------------------------------------
  ;; SUBSECTION 2.3: SHADOWING IMPORTS (Override conflicting symbols)
  ;; ---------------------------------------------------------------------------
  
  ;; WHY SHADOWING?
  ;; When importing from multiple modules, symbol name conflicts occur.
  ;; Shadowing says: "if both :cl and SB-EXT export 'WORD', use SB-EXT's version".
  ;;
  ;; This is necessary because:
  ;; 1. Standard CL has 'defmethod', 'defgeneric', etc.
  ;; 2. CLOSER-MOP redefines these to add portability
  ;; 3. Without shadowing, :cl version takes precedence (wrong for CCL)
  ;; 4. Shadowing forces the MOP version to be used
  
  ;; SBCL-specific shadowing:
  ;;   Import WORD from SB-EXT instead of :cl
  ;;   WHY? SB-EXT:WORD is a type in SBCL for machine-word integers.
  ;;        Potentially more efficient than CL:INTEGER on 64-bit systems.
  ;;        However: This symbol is NOT EXPORTED (not in :export list).
  ;;        RISK: Dead code? Or used internally by Layer 1-7 code?
  ;;              → TODO: Verify usage of WORD throughout codebase
  ;;              → TODO: If unused, delete this line
  #+sbcl (:shadowing-import-from "SB-EXT" "WORD")
  
  ;; CCL-specific shadowing:
  ;;   Import 11 MOP-related symbols from CLOSER-MOP instead of native CCL.
  ;;   WHY? To ensure CCL code uses standard MOP semantics, not CCL's variant.
  ;;
  ;;   These 11 symbols are the *core* of the MOP. Shadowing them ensures:
  ;;   - (defclass ...) behaves the same on CCL as SBCL
  ;;   - (defmethod ...) behavior is consistent
  ;;   - Metaclass protocols are standard-compliant
  ;;
  ;;   RISK: If any of these symbols is missing from CLOSER-MOP, compilation fails.
  ;;         If CLOSER-MOP version differs, semantic drift may occur silently.
  
  ;; Core metaclass protocol
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-METHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "FINALIZE-INHERITANCE")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-GENERIC-FUNCTION")
  
  ;; Macro and method definition
  #+ccl (:shadowing-import-from "CLOSER-MOP" "DEFMETHOD")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "DEFGENERIC")
  
  ;; Class hierarchy
  #+ccl (:shadowing-import-from "CLOSER-MOP" "STANDARD-CLASS")
  
  ;; Method dispatch and compilation
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-DISCRIMINATING-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-APPLICABLE-METHODS-USING-CLASSES")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "COMPUTE-EFFECTIVE-METHOD")
  
  ;; Introspection
  #+ccl (:shadowing-import-from "CLOSER-MOP" "METHOD-FUNCTION")
  #+ccl (:shadowing-import-from "CLOSER-MOP" "MAKE-METHOD-LAMBDA")
  
  ;; NO SHADOWING FOR LISPWORKS
  ;; LispWorks CLOS is already standard-compliant, so no shadowing needed.
  ;; Native (clos:defmethod ...) works as expected.
  
  ;; ===========================================================================
  ;; SECTION 3: EXPORTED SYMBOLS (Public API)
  ;; ===========================================================================
  
  ;; Total exports: 166 symbols across 6 domains
  ;; Each symbol below is a PROMISE to maintain it forever (Phase 3: Byzantine Compatibility)
  
  (:export
   
   ;; =========================================================================
   ;; DOMAIN 1: GRAPH LIFECYCLE MANAGEMENT (29 symbols)
   ;; =========================================================================
   ;; These functions manage the lifecycle of graph instances:
   ;; creating, opening, closing, and accessing global graph registries.
   ;;
   ;; WHY SEPARATE LIFECYCLE FUNCTIONS?
   ;; Graph instances are heavyweight objects (persistent, multi-threaded, global state).
   ;; Creating and opening them are distinct operations:
   ;; - make-graph: Create new, empty graph in memory
   ;; - open-graph: Load existing graph from disk (recover from crash, etc.)
   ;; - close-graph: Flush to disk, free resources
   ;; - lookup-graph: Retrieve by name (global registry)
   ;;
   ;; Side effects: Disk I/O, memory allocation, thread pool creation
   
   #:make-graph           ;; (make-graph name &key location version)
                          ;; Create new in-memory graph with name and optional location.
                          ;; Returns: graph instance (hash table + metadata)
                          ;; Side effects: Allocates memory, creates buffer pool
   
   #:open-graph           ;; (open-graph location)
                          ;; Open existing graph from disk at location.
                          ;; Recovers from previous close (replay transaction log if incomplete).
                          ;; Returns: graph instance
                          ;; Side effects: Disk I/O, loads metadata, replays transactions
   
   #:close-graph          ;; (close-graph graph)
                          ;; Flush graph state to disk, release resources.
                          ;; Ensures durability (all changes persisted).
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Disk I/O, thread shutdown, memory deallocation
   
   #:lookup-graph         ;; (lookup-graph name)
                          ;; Find graph by name in global registry.
                          ;; Returns: graph instance or NIL if not found
                          ;; Side effects: None (read-only)
   
   #:graph-stats          ;; (graph-stats graph)
                          ;; Query performance metrics (vertices count, edges count, memory usage, etc.)
                          ;; Returns: alist (key . value)
                          ;; Side effects: None (read-only snapshot)
   
   #:check-data-integrity ;; (check-data-integrity graph)
                          ;; Verify graph consistency (no dangling references, valid indexes, etc.)
                          ;; Returns: T if valid, throws error if corrupted
                          ;; Side effects: Possibly repairs minor inconsistencies
   
   #:snapshot             ;; (snapshot graph label)
                          ;; Create named backup of current graph state.
                          ;; Useful before risky operations (bulk deletes, schema changes).
                          ;; Returns: snapshot ID or name
                          ;; Side effects: Disk I/O (writes snapshot file)
   
   #:replay               ;; (replay graph snapshot-id)
                          ;; Redo transaction log from a snapshot.
                          ;; Used for "forward recovery" (applying logged changes).
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Disk I/O, replay writes to graph
   
   #:restore              ;; (restore graph snapshot-id)
                          ;; Revert graph to named snapshot state.
                          ;; Discards all changes since snapshot.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Overwrites current graph with snapshot
   
   #:location             ;; (location graph) or (setf (location graph) path)
                          ;; Accessor for graph's disk location (file path).
                          ;; Returns: pathname
                          ;; Side effects: None (pure accessor)
   
   #:schema               ;; (schema graph)
                          ;; Accessor for graph's schema (type definitions).
                          ;; Returns: schema object
                          ;; Side effects: None (pure accessor)
   
   #:indexes              ;; (indexes graph)
                          ;; Accessor for graph's index registry.
                          ;; Returns: alist of indexes
                          ;; Side effects: None (pure accessor)
   
   #:*graph*              ;; *graph* DYNAMIC VARIABLE
                          ;; Current graph in thread-local context.
                          ;; Bound by with-transaction, make-graph, etc.
                          ;; WHY EXPORTED? Allows advanced users to manually bind graphs
                          ;; RISK: Direct modification bypasses safety checks
                          ;; RECOMMENDATION: Use with-transaction instead of raw *graph*
   
   #:execute-tx           ;; (execute-tx graph fn &key isolation)
                          ;; Execute function fn within transaction context.
                          ;; Ensures ACID properties.
                          ;; Returns: result of fn
                          ;; Side effects: Binds *transaction*, *graph*; may write to disk
   
   #:transaction-p        ;; (transaction-p object)
                          ;; Predicate: is object a transaction?
                          ;; Returns: T or NIL
                          ;; Side effects: None
   
   #:graph-name           ;; (graph-name graph)
                          ;; Accessor for graph's symbolic name.
                          ;; Returns: symbol or string
                          ;; Side effects: None
   
   #:transaction-error    ;; EXCEPTION TYPE
                          ;; Signaled when transaction fails (conflict, timeout, etc.)
                          ;; Subtypes exist for specific failures:
                          ;; - conflict-error (concurrent modification)
                          ;; - timeout-error (deadlock or long wait)
                          ;; - rollback-error (constraint violation)
   
   ;; Replication-related exports (Master-slave synchronization)
   ;; WHY REPLICATION? VivaceGraph supports read-only replicas for scaling queries.
   ;; Master node logs all writes, slave replicas replay them asynchronously.
   
   #:master-host          ;; (master-host graph)
                          ;; Host/IP of replication master.
                          ;; Returns: string (hostname or IP)
   
   #:replication-port     ;; (replication-port graph)
                          ;; Network port for replication protocol.
                          ;; Returns: integer (port number)
   
   #:slave-socket         ;; (slave-socket graph)
                          ;; Network connection to master (for slave replicas).
                          ;; Returns: socket or NIL if not a slave
   
   #:replication-key      ;; (replication-key graph)
                          ;; Authentication key for replication.
                          ;; Used to verify slave trusts master.
                          ;; Returns: string
   
   #:master-txn-id        ;; (master-txn-id graph)
                          ;; Latest transaction ID received from master.
                          ;; Used to prevent duplicate replays.
                          ;; Returns: integer
   
   #:stop-replication-p    ;; (stop-replication-p graph)
                          ;; Predicate: is replication stopped?
                          ;; Returns: T if replication is paused
   
   #:execute-tx-action    ;; (execute-tx-action graph action)
                          ;; Execute action within current transaction.
                          ;; Actions are graph operations (add vertex, add edge, etc.)
                          ;; Returns: result of action
                          ;; Side effects: Updates transaction journal
   
   #:write-last-txn-id    ;; (write-last-txn-id graph txn-id)
                          ;; Record last applied transaction ID to disk.
                          ;; Used for crash recovery and replication checkpoints.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Disk I/O
   
   #:read-last-txn-id     ;; (read-last-txn-id graph)
                          ;; Retrieve last applied transaction ID from disk.
                          ;; Used during graph initialization to resume replication.
                          ;; Returns: integer or 0 if no transactions
   
   #:start-replication    ;; (start-replication graph master-host master-port)
                          ;; Begin replicating from master.
                          ;; Spawns background thread to stream changes.
                          ;; Returns: T if started successfully
                          ;; Side effects: Creates thread, network connection
   
   #:stop-replication     ;; (stop-replication graph)
                          ;; Stop replicating from master.
                          ;; Background replication thread terminates gracefully.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Thread shutdown, socket close
   
   #:stop-buffer-pool     ;; (stop-buffer-pool graph)
                          ;; Shut down the buffer pool (Layer 4 memory management).
                          ;; Flushes all dirty pages to disk.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Thread shutdown, disk I/O
   
   ;; =========================================================================
   ;; DOMAIN 2: REST API INTERFACE (4 symbols)
   ;; =========================================================================
   ;; These functions enable VivaceGraph to expose itself as an HTTP service.
   ;; Clients can access graph via REST API instead of Lisp code.
   ;;
   ;; WHY REST API?
   ;; Not all consumers are Lisp programs. REST allows:
   ;; - JavaScript/Python/Java clients to access graph
   ;; - Web browsers to query data
   ;; - Microservices to integrate
   ;;
   ;; Side effects: HTTP server startup, port binding
   
   #:start-rest           ;; (start-rest graph &key host port)
                          ;; Start HTTP REST API server.
                          ;; Listens on host:port, exposes graph endpoints.
                          ;; Returns: server handle
                          ;; Side effects: Binds port, creates thread
   
   #:stop-rest            ;; (stop-rest server-handle)
                          ;; Stop HTTP REST API server.
                          ;; Gracefully shuts down listener threads.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Port released, thread shutdown
   
   #:def-rest-procedure   ;; (def-rest-procedure name method path handler)
                          ;; Define custom REST endpoint (e.g., /api/custom POST handler).
                          ;; Allows extending REST API without modifying VivaceGraph.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Registers endpoint in routing table
   
   #:*rest-procedures*    ;; *rest-procedures* GLOBAL ALIST
                          ;; Registry of custom REST endpoints.
                          ;; Alist: ((method . path) . handler-function) ...
                          ;; WHY EXPORTED? Allows introspection of registered endpoints
                          ;; RISK: Direct modification bypasses validation
   
   ;; =========================================================================
   ;; DOMAIN 3: TRANSACTION MANAGEMENT (8 symbols)
   ;; =========================================================================
   ;; Transactions ensure ACID properties (Atomicity, Consistency, Isolation, Durability).
   ;; These functions allow starting, committing, and rolling back transactions.
   ;;
   ;; WHY TRANSACTIONS?
   ;; Without transactions, concurrent updates can corrupt data:
   ;; - Two threads modify same vertex → undefined result (race condition)
   ;; - Crash mid-write → partial data on disk (inconsistency)
   ;; - Reader sees half-committed state (isolation violation)
   ;;
   ;; Transactions prevent these by:
   ;; - Atomic: All-or-nothing (commit or rollback completely)
   ;; - Consistent: Constraints checked before commit
   ;; - Isolated: Readers see consistent snapshot
   ;; - Durable: Committed data survives crash
   ;;
   ;; Side effects: Write locks acquired, transaction journal updated
   
   #:with-transaction     ;; MACRO: (with-transaction (&optional graph) &body forms)
                          ;; Execute forms within transaction context.
                          ;; Automatically commits if no error, rolls back on error.
                          ;; Binds *transaction* and *graph* for nested code.
                          ;; Returns: result of last form
                          ;; Side effects: Acquires locks, writes to journal
   
   #:lookup-object        ;; (lookup-object graph id &key type)
                          ;; Retrieve vertex/edge by ID within transaction.
                          ;; Ensures transactional isolation (sees only committed data).
                          ;; Returns: vertex/edge instance or NIL if not found
                          ;; Side effects: None (read-only within transaction)
   
   #:update-node          ;; (update-node transaction node new-values)
                          ;; Modify node's slot values within transaction.
                          ;; Records change in transaction journal.
                          ;; Returns: modified node
                          ;; Side effects: Updates node in memory, marks as dirty
   
   #:delete-node          ;; (delete-node transaction node)
                          ;; Mark node as deleted within transaction.
                          ;; Soft delete (not physically removed, just flagged).
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Sets deleted-p flag, updates indexes
   
   #:commit               ;; (commit &optional transaction)
                          ;; Commit current transaction to disk.
                          ;; Validates constraints, flushes to durable storage.
                          ;; Returns: transaction ID (for replication tracking)
                          ;; Side effects: Disk I/O, releases locks
   
   #:rollback             ;; (rollback &optional transaction)
                          ;; Discard all changes in transaction.
                          ;; Reverts graph to state before with-transaction.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Releases locks, frees memory
   
   #:*transaction*        ;; *transaction* DYNAMIC VARIABLE
                          ;; Current transaction in thread-local context.
                          ;; Bound by with-transaction macro.
                          ;; WHY EXPORTED? Advanced users can nest transactions
                          ;; RISK: Direct modification bypasses validation
                          ;; RECOMMENDATION: Use with-transaction instead of raw *transaction*
   
   #:no-transaction-in-progress ;; EXCEPTION TYPE
                          ;; Signaled when operation requires transaction but none active.
                          ;; Example: (with-transaction) required but not running
   
   ;; =========================================================================
   ;; DOMAIN 4: SCHEMA & TYPE DEFINITIONS (18 symbols + SBCL-only RW-locks)
   ;; =========================================================================
   ;; Schema defines the structure of data in the graph (vertex/edge types, slots).
   ;; These functions allow defining types, querying schemas, and synchronizing access.
   ;;
   ;; WHY SCHEMA?
   ;; Untyped data leads to:
   ;; - Bugs (accessing undefined slots)
   ;; - Inconsistency (same type with different fields)
   ;; - Inefficiency (can't optimize for known structure)
   ;;
   ;; Schema provides:
   ;; - Type definitions (vertex type Person has slots: name, age, email)
   ;; - Validation (new instances must match schema)
   ;; - Optimization (indexes, serialization, etc.)
   ;;
   ;; Side effects: Schema stored globally, types registered
   
   #:def-node-type        ;; (def-node-type name supertypes slots &key documentation)
                          ;; Define base node type (abstraction for Vertex/Edge).
                          ;; Creates CLOS class, registers in schema.
                          ;; Returns: class object
                          ;; Side effects: Creates class, updates *schema-node-metadata*
   
   #:def-vertex           ;; (def-vertex name supertypes slots &key documentation)
                          ;; Define vertex type (specializes node-type for vertices).
                          ;; Automatically creates indexes, Prolog predicates, etc.
                          ;; Returns: class object
                          ;; Side effects: Creates class, updates schema, creates indexes
   
   #:def-edge             ;; (def-edge name supertypes slots &key documentation)
                          ;; Define edge type (specializes node-type for directed edges).
                          ;; Automatically creates indexes for from/to relationships.
                          ;; Returns: class object
                          ;; Side effects: Creates class, updates schema, creates indexes
   
   #:edge-exists-p        ;; (edge-exists-p from to &key type)
                          ;; Predicate: does edge exist between from and to vertices?
                          ;; Efficiently checks edge index.
                          ;; Returns: T or NIL
                          ;; Side effects: None
   
   #:lookup-node-type-by-name ;; (lookup-node-type-by-name name)
                          ;; Retrieve type definition by name.
                          ;; Returns: class object or NIL
                          ;; Side effects: None
   
   #:instantiate-node-type ;; (instantiate-node-type type &rest slot-values)
                          ;; Create instance of type with slot values.
                          ;; Like (make-instance type ...) but with validation.
                          ;; Returns: new instance
                          ;; Side effects: Allocates memory, assigns ID
   
   #:*schema-node-metadata* ;; *schema-node-metadata* GLOBAL HASH-TABLE
                          ;; Registry of type metadata (slot definitions, indexes, etc.)
                          ;; Maps type-name → metadata-object
                          ;; WHY EXPORTED? Allows introspection of schema
                          ;; RISK: Direct modification may corrupt type system
                          ;; RECOMMENDATION: Use def-vertex/def-edge instead
   
   #:with-write-locked-class ;; MACRO: (with-write-locked-class class &body forms)
                          ;; Execute forms while holding write lock on class.
                          ;; Prevents concurrent class modification (e.g., adding slots).
                          ;; Returns: result of last form
                          ;; Side effects: Acquires lock, blocks other threads
   
   #:with-read-locked-class ;; MACRO: (with-read-locked-class class &body forms)
                          ;; Execute forms while holding read lock on class.
                          ;; Allows concurrent reads, blocks concurrent writes.
                          ;; Returns: result of last form
                          ;; Side effects: Acquires lock (shared)
   
   #:schema-class-locks   ;; (schema-class-locks class)
                          ;; Accessor for RW-lock associated with class.
                          ;; Returns: rw-lock object or NIL
                          ;; Side effects: None
   
   ;; SBCL-ONLY RW-LOCK PRIMITIVES (Lines 77-84 in original)
   ;; These are low-level threading primitives.
   ;; WHY SBCL-ONLY? SBCL has efficient RW-lock support in sb-threads.
   ;;               CCL and LispWorks should use bordeaux-threads instead.
   ;; RISK: API inconsistency across Lisps (CCL/LispWorks users cannot access)
   
   #+sbcl #:make-rw-lock  ;; (make-rw-lock &optional name)
                          ;; Create read-write lock (SBCL only).
                          ;; Supports multiple concurrent readers, exclusive writer.
                          ;; Returns: rw-lock object
                          ;; Side effects: Allocates lock structure
   
   #+sbcl #:with-read-lock ;; MACRO: (with-read-lock rw-lock &body forms)
                          ;; Hold read lock during forms execution (SBCL only).
                          ;; Multiple threads can hold read lock simultaneously.
                          ;; Returns: result of last form
                          ;; Side effects: Acquires lock (possibly waits), releases after forms
   
   #+sbcl #:with-write-lock ;; MACRO: (with-write-lock rw-lock &body forms)
                          ;; Hold write lock during forms execution (SBCL only).
                          ;; Only one thread can hold write lock at a time.
                          ;; Returns: result of last form
                          ;; Side effects: Acquires exclusive lock (may wait), releases after forms
   
   #+sbcl #:acquire-read-lock ;; (acquire-read-lock rw-lock)
                          ;; Acquire read lock (SBCL only).
                          ;; Blocks if writer currently holds lock.
                          ;; Returns: NIL
                          ;; Side effects: Modifies lock state, may block
   
   #+sbcl #:release-read-lock ;; (release-read-lock rw-lock)
                          ;; Release read lock (SBCL only).
                          ;; Allows waiting writers to proceed.
                          ;; Returns: NIL
                          ;; Side effects: Modifies lock state, wakes blocked threads
   
   #+sbcl #:acquire-write-lock ;; (acquire-write-lock rw-lock)
                          ;; Acquire exclusive write lock (SBCL only).
                          ;; Blocks if any reader or writer holds lock.
                          ;; Returns: NIL
                          ;; Side effects: Modifies lock state, may block
   
   #+sbcl #:release-write-lock ;; (release-write-lock rw-lock)
                          ;; Release write lock (SBCL only).
                          ;; Allows all waiting readers/writers to proceed.
                          ;; Returns: NIL
                          ;; Side effects: Modifies lock state, wakes blocked threads
   
   #+sbcl #:rw-lock-p     ;; (rw-lock-p object)
                          ;; Predicate: is object an RW-lock? (SBCL only)
                          ;; Returns: T or NIL
                          ;; Side effects: None
   
   ;; =========================================================================
   ;; DOMAIN 5: VERTEX/EDGE CRUD & TRAVERSAL (31 symbols)
   ;; =========================================================================
   ;; Core graph operations: creating, reading, updating, deleting vertices/edges.
   ;; Traversal functions for exploring the graph structure.
   ;;
   ;; WHY SEPARATE CRUD FROM SCHEMA?
   ;; Schema (def-vertex) defines *types*. CRUD (make-vertex) creates *instances*.
   ;; - Schema: "A Person has name and age" (class definition)
   ;; - CRUD: "Create Alice, age 30" (instance creation)
   ;;
   ;; Side effects: Memory allocation, disk I/O, index updates
   
   #:vertex               ;; Alias for generic-vertex class
                          ;; Used as (make-instance graph-db:vertex ...)
   
   #:edge                 ;; Alias for generic-edge class
                          ;; Used as (make-instance graph-db:edge ...)
   
   #:generic-edge         ;; Built-in edge type (no custom type defined).
                          ;; Useful for temporary edges or heterogeneous graphs.
                          ;; Has standard slots: from, to, weight, type-id, etc.
   
   #:generic-vertex       ;; Built-in vertex type (no custom type defined).
                          ;; Useful for temporary vertices or heterogeneous graphs.
                          ;; Has standard slots: id, type-id, data, etc.
   
   #:make-vertex          ;; (make-vertex graph type-name &optional data)
                          ;; Create new vertex instance in graph.
                          ;; Assigns unique ID, initializes slots.
                          ;; Returns: vertex instance
                          ;; Side effects: Memory allocation, ID assignment, marks dirty
   
   #:make-edge            ;; (make-edge graph from-vertex to-vertex type-name &optional weight)
                          ;; Create new directed edge from→to in graph.
                          ;; Assigns unique ID, initializes slots.
                          ;; Returns: edge instance
                          ;; Side effects: Memory allocation, ID assignment, updates VE/VEV indexes
   
   #:lookup-vertex        ;; (lookup-vertex graph id)
                          ;; Retrieve vertex by ID.
                          ;; Efficient O(1) hashtable lookup.
                          ;; Returns: vertex instance or NIL if not found
                          ;; Side effects: None
   
   #:lookup-edge          ;; (lookup-edge graph id)
                          ;; Retrieve edge by ID.
                          ;; Efficient O(1) hashtable lookup.
                          ;; Returns: edge instance or NIL if not found
                          ;; Side effects: None
   
   #:to                   ;; (to edge) or (setf (to edge) vertex)
                          ;; Accessor for edge's target vertex.
                          ;; Returns: vertex instance
                          ;; Side effects: None (pure accessor)
   
   #:from                 ;; (from edge) or (setf (from edge) vertex)
                          ;; Accessor for edge's source vertex.
                          ;; Returns: vertex instance
                          ;; Side effects: None (pure accessor)
   
   #:weight               ;; (weight edge) or (setf (weight edge) number)
                          ;; Accessor for edge's weight (cost, distance, strength, etc.).
                          ;; Used in shortest-path, PageRank, etc.
                          ;; Returns: float or NIL
                          ;; Side effects: None (pure accessor)
   
   #:id                   ;; (id node) or (setf (id node) id-bytes)
                          ;; Accessor for node's unique identifier (16-byte UUID).
                          ;; DO NOT MODIFY after creation (breaks indexes).
                          ;; Returns: byte-array (16 bytes)
                          ;; Side effects: None recommended (setting ID breaks invariants)
   
   #:string-id            ;; (string-id node)
                          ;; Human-readable string representation of node's ID.
                          ;; Hexadecimal encoding of 16-byte UUID.
                          ;; Returns: string (e.g., "0a1b2c3d4e5f...")
                          ;; Side effects: None
   
   #:node-to-alist        ;; (node-to-alist node)
                          ;; Convert node to alist for JSON serialization.
                          ;; Maps slot-name → slot-value.
                          ;; Returns: alist
                          ;; Side effects: None
   
   #:type-id              ;; (type-id node) or (setf (type-id node) id)
                          ;; Accessor for node's type ID (for polymorphism).
                          ;; Maps to a type name in schema.
                          ;; Returns: integer
                          ;; Side effects: None (pure accessor)
   
   #:revision             ;; (revision node) or (setf (revision node) num)
                          ;; Accessor for node's revision number (optimistic locking).
                          ;; Incremented on each update.
                          ;; Used to detect concurrent modifications.
                          ;; Returns: integer
                          ;; Side effects: None (pure accessor)
   
   #:deleted-p            ;; (deleted-p node)
                          ;; Predicate: is node marked as deleted?
                          ;; Soft delete (still in storage, flagged as deleted).
                          ;; Returns: T or NIL
                          ;; Side effects: None
   
   #:active-edge-p        ;; (active-edge-p edge)
                          ;; Predicate: is edge active (not deleted)?
                          ;; Convenience predicate combining deleted-p check.
                          ;; Returns: T or NIL
                          ;; Side effects: None
   
   #:data                 ;; (data node) or (setf (data node) alist)
                          ;; Accessor for node's custom data (alist of key.value pairs).
                          ;; Used for ad-hoc attributes not defined in schema.
                          ;; Returns: alist
                          ;; Side effects: None (pure accessor)
   
   #:traverse             ;; (traverse graph start-vertex &key depth type-filter direction)
                          ;; Breadth-first traversal from start vertex.
                          ;; Optionally filter by edge type, direction, depth.
                          ;; Returns: traversal object (with end-vertices list)
                          ;; Side effects: None (read-only traversal)
   
   #:traversal-path       ;; (traversal-path traversal-result)
                          ;; Accessor for path from start to end vertex in traversal.
                          ;; Returns: list of vertices (path)
                          ;; Side effects: None
   
   #:end-vertex           ;; (end-vertex traversal-result)
                          ;; Accessor for final vertex in traversal.
                          ;; Returns: vertex instance
                          ;; Side effects: None
   
   #:map-vertices         ;; (map-vertices graph fn &key type-filter)
                          ;; Apply function fn to each vertex in graph.
                          ;; Optionally filter by type.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Calls fn for each vertex (fn may have side effects)
   
   #:map-edges            ;; (map-edges graph fn &key type-filter direction from-vertex to-vertex)
                          ;; Apply function fn to each edge in graph.
                          ;; Optionally filter by type, direction, endpoints.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Calls fn for each edge (fn may have side effects)
   
   #:outgoing-edges       ;; (outgoing-edges vertex)
                          ;; Retrieve all edges originating from vertex.
                          ;; Efficient O(log n) lookup via VE-index.
                          ;; Returns: list of edge instances
                          ;; Side effects: None
   
   #:incoming-edges       ;; (incoming-edges vertex)
                          ;; Retrieve all edges targeting vertex.
                          ;; Efficient O(log n) lookup via VE-index (reverse).
                          ;; Returns: list of edge instances
                          ;; Side effects: None
   
   #:node-slot-value      ;; (node-slot-value node slot-name)
                          ;; Reflectively access node's slot value by name.
                          ;; Like (slot-value node slot-name) but by string name.
                          ;; Returns: slot value or NIL if slot undefined
                          ;; Side effects: None
   
   #:copy                 ;; (copy node)
                          ;; Create independent copy of node (deep copy of data).
                          ;; New copy has new ID (not linked to original).
                          ;; Returns: copy of node
                          ;; Side effects: Memory allocation, new ID assignment
   
   #:save                 ;; (save node)
                          ;; Persist node changes to disk (within transaction).
                          ;; Flushes to Layer 4 storage, updates indexes.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Disk I/O, index updates
   
   #:mark-deleted         ;; (mark-deleted node)
                          ;; Mark node as deleted (soft delete, not physically removed).
                          ;; Node still retrieves from storage, but flagged as deleted.
                          ;; Prevents cascading deletes, allows rollback.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Sets deleted-p flag, updates indexes
   
   #:stale-revision-error ;; EXCEPTION TYPE
                          ;; Signaled when optimistic locking detects conflict.
                          ;; Another thread modified same node, invalidating current transaction.
                          ;; Caller should rollback and retry.
   
   ;; =========================================================================
   ;; DOMAIN 6: MATERIALIZED VIEWS (17 symbols)
   ;; =========================================================================
   ;; Views are cached query results, automatically updated when data changes.
   ;; Useful for expensive computations (aggregations, complex queries).
   ;;
   ;; WHY VIEWS?
   ;; Computing aggregates on every query is slow (e.g., count all Person vertices).
   ;; Views pre-compute results and maintain them as data changes.
   ;; Trade memory for speed (caching).
   ;;
   ;; Side effects: Memory allocation for cache, automatic updates on writes
   
   #:def-view             ;; (def-view name vertex-type emit-fn &key reduce-fn)
                          ;; Define materialized view on vertex-type.
                          ;; emit-fn: for each vertex, emit (key . value) pair
                          ;; reduce-fn: optional aggregation (e.g., sum, count)
                          ;; Returns: view object
                          ;; Side effects: Creates view, indexes it, initializes cache
   
   #:*view-rv*            ;; *view-rv* DYNAMIC VARIABLE
                          ;; View result variable (bound within map-view/map-reduced-view).
                          ;; Holds current (key . result) pair during iteration.
                          ;; WHY EXPORTED? Allows accessing result in view code
                          ;; RISK: Modifying may corrupt iteration
   
   #:yield                ;; (yield key value)
                          ;; Emit (key . value) pair to view from emit-fn.
                          ;; Collects results into view cache.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Appends to view cache
   
   #:map-view             ;; (map-view view fn)
                          ;; Apply fn to each (key . value) pair in view.
                          ;; Iterates over materialized cache (fast).
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Calls fn for each pair (fn may have side effects)
   
   #:map-reduced-view     ;; (map-reduced-view view fn)
                          ;; Apply fn to each reduced result in view.
                          ;; Calls reduce-fn on values with same key.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Calls fn for each reduced pair
   
   #:invoke-graph-view    ;; (invoke-graph-view graph view-name &optional args)
                          ;; Execute view query with arguments.
                          ;; Useful for views with parameters (e.g., "top-N persons").
                          ;; Returns: view result (list of pairs)
                          ;; Side effects: None (query-only)
   
   #:make-view            ;; (make-view name)
                          ;; Create empty view object (advanced usage).
                          ;; Usually not called directly (use def-view instead).
                          ;; Returns: view object
                          ;; Side effects: Memory allocation
   
   #:delete-view          ;; (delete-view graph view-name)
                          ;; Delete view by name from graph.
                          ;; Frees memory, removes from index.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Memory deallocation, index removal
   
   #:save-views           ;; (save-views graph)
                          ;; Persist all views to disk.
                          ;; Used during graph close to recover views on next open.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Disk I/O
   
   #:restore-views        ;; (restore-views graph)
                          ;; Load views from disk after graph open.
                          ;; Rebuilds in-memory caches from persistent storage.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Disk I/O, memory allocation
   
   #:get-view-table-for-class ;; (get-view-table-for-class class)
                          ;; Retrieve views defined on this class.
                          ;; Returns: hash-table of views
                          ;; Side effects: None
   
   #:regenerate-view      ;; (regenerate-view graph view-name)
                          ;; Rebuild view cache from scratch.
                          ;; Useful if view cache is corrupted or out-of-sync.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Recomputes all emit-fn calls, overwrites cache
   
   #:lookup-view-group    ;; (lookup-view-group graph class-name)
                          ;; Retrieve all views for a class.
                          ;; Returns: list of view objects
                          ;; Side effects: None
   
   #:lookup-view          ;; (lookup-view graph view-name)
                          ;; Retrieve view by name.
                          ;; Returns: view object or NIL
                          ;; Side effects: None
   
   #:with-write-locked-view-group ;; MACRO: (with-write-locked-view-group class &body forms)
                          ;; Execute forms while holding write lock on all views for class.
                          ;; Prevents concurrent view updates.
                          ;; Returns: result of last form
                          ;; Side effects: Acquires locks, blocks other threads
   
   #:with-read-locked-view-group ;; MACRO: (with-read-locked-view-group class &body forms)
                          ;; Execute forms while holding read locks on all views for class.
                          ;; Allows concurrent reads, blocks concurrent writes.
                          ;; Returns: result of last form
                          ;; Side effects: Acquires locks (shared)
   
   #:view-group-lock      ;; (view-group-lock class)
                          ;; Accessor for RW-lock associated with all views of class.
                          ;; Returns: rw-lock object or NIL
                          ;; Side effects: None
   
   ;; =========================================================================
   ;; DOMAIN 7: PROLOG LOGIC PROGRAMMING (51 symbols)
   ;; =========================================================================
   ;; Prolog is a logic programming language (declarative queries on graphs).
   ;; VivaceGraph integrates Prolog for complex graph queries.
   ;;
   ;; WARNING: Prolog subsystem is large (51 symbols = 31% of total exports).
   ;;          This suggests Prolog may not belong in Layer 1 (foundational).
   ;;          RISK: Phase 3 backward-compatibility burden if Prolog rules change.
   ;;
   ;; WHY PROLOG?
   ;; Graph queries often require logical reasoning:
   ;; - "Find all ancestors of X"
   ;; - "Find shortest path from A to B"
   ;; - "Count vertices satisfying constraint"
   ;;
   ;; Prolog provides declarative syntax for these (easier than imperative loops).
   ;;
   ;; Side effects: Global state (*trail*, *var-counter*, *functor*), modifies Prolog registry
   
   ;; Core Prolog operations
   #:def-global-prolog-functor ;; (def-global-prolog-functor name arity clauses &key documentation)
                          ;; Define Prolog predicate (functor) globally.
                          ;; Clauses are Prolog rules (head :- body).
                          ;; Returns: functor object
                          ;; Side effects: Registers in *prolog-global-functors*
   
   #:def-prolog-compiler-macro ;; (def-prolog-compiler-macro name macro-fn)
                          ;; Define compiler macro for Prolog goal.
                          ;; Allows optimizing compilation of specific predicates.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Registers in macro table
   
   #:compile-body         ;; (compile-body goals)
                          ;; Compile Prolog goal expressions to Lisp code.
                          ;; Produces executable query.
                          ;; Returns: compiled goal
                          ;; Side effects: None (pure compilation)
   
   #:args                 ;; (args functor-object)
                          ;; Accessor for Prolog functor's argument list.
                          ;; Returns: list of argument symbols
                          ;; Side effects: None
   
   #:*prolog-global-functors* ;; *prolog-global-functors* GLOBAL HASH-TABLE
                          ;; Registry of globally-defined Prolog predicates.
                          ;; Maps functor-name → functor-object
                          ;; WHY EXPORTED? Allows introspection of defined predicates
                          ;; RISK: Direct modification may corrupt Prolog runtime
   
   #:deref-exp            ;; (deref-exp term)
                          ;; Dereference Prolog term (resolve variable bindings).
                          ;; Returns: fully-dereferenced term
                          ;; Side effects: None
   
   #:unify                ;; (unify term1 term2 &optional bindings)
                          ;; Unify two Prolog terms with optional pre-existing bindings.
                          ;; Returns: binding alist or NIL if unification fails
                          ;; Side effects: None (pure unification)
   
   #:select               ;; (select goal)
                          ;; Execute Prolog goal and return all solutions.
                          ;; Shorthand for filtering/querying.
                          ;; Returns: list of solutions
                          ;; Side effects: None (query-only)
   
   #:?                    ;; MACRO: (? goal)
                          ;; Execute Prolog goal, return first solution.
                          ;; Interactive REPL convenience.
                          ;; Returns: solution or NIL if no solution
                          ;; Side effects: Modifies *trail*, may bind variables
   
   #:?-                   ;; MACRO: (?- goal1 goal2 ...)
                          ;; Execute multiple Prolog goals in sequence (conjunction).
                          ;; Returns: T if all succeed, NIL otherwise
                          ;; Side effects: Modifies *trail*
   
   #:q-                   ;; MACRO: (q- goal)
                          ;; Quiet Prolog query (suppresses output).
                          ;; Returns: solution or NIL
                          ;; Side effects: Modifies *trail*
   
   #:!                    ;; MACRO: (!)
                          ;; Prolog cut operator (commits to current choice point).
                          ;; Prevents backtracking to earlier alternatives.
                          ;; Returns: T
                          ;; Side effects: Modifies choice point stack
   
   #:cut                  ;; Alias for ! (see above)
   
   #:var-deref            ;; (var-deref variable bindings)
                          ;; Dereference single variable within bindings.
                          ;; Returns: value or variable if unbound
                          ;; Side effects: None
   
   #:undo-bindings        ;; (undo-bindings bindings &optional trail)
                          ;; Undo (rollback) variable bindings.
                          ;; Used on backtracking to restore previous state.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Modifies trail, reverts bindings
   
   #:replace-?-vars       ;; (replace-?-vars term bindings)
                          ;; Replace all ?-prefixed variables in term with values.
                          ;; Substitution using bindings.
                          ;; Returns: substituted term
                          ;; Side effects: None
   
   #:variables-in         ;; (variables-in term)
                          ;; Extract all variables in Prolog term.
                          ;; Returns: list of variable symbols
                          ;; Side effects: None
   
   #:make-functor-symbol  ;; (make-functor-symbol name arity)
                          ;; Create unique symbol for Prolog functor.
                          ;; Used for functor registry keys.
                          ;; Returns: symbol (e.g., |ancestor/2|)
                          ;; Side effects: None
   
   #:*trail*              ;; *trail* DYNAMIC VARIABLE
                          ;; Prolog choice point trail (for backtracking).
                          ;; Records variable bindings for rollback on failure.
                          ;; WHY EXPOSED? Advanced users implementing custom predicates
                          ;; RISK: Direct modification breaks backtracking
   
   #:*var-counter*        ;; *var-counter* DYNAMIC VARIABLE
                          ;; Counter for generating unique variable names.
                          ;; Incremented for each new variable in query.
                          ;; WHY EXPOSED? Advanced variable generation control
                          ;; RISK: Modifying may cause variable name collisions
   
   #:*functor*            ;; *functor* DYNAMIC VARIABLE
                          ;; Current functor being evaluated.
                          ;; Used for recursive functor calls.
                          ;; WHY EXPOSED? Advanced meta-programming
                          ;; RISK: Modifying breaks functor dispatch
   
   #:make-functor         ;; (make-functor name arity clauses)
                          ;; Create Prolog functor object (advanced).
                          ;; Usually use def-global-prolog-functor instead.
                          ;; Returns: functor object
                          ;; Side effects: Memory allocation
   
   #:maybe-add-undo-bindings ;; (maybe-add-undo-bindings bindings)
                          ;; Optionally record bindings for rollback.
                          ;; Used internally during goal execution.
                          ;; Returns: NIL or binding record
                          ;; Side effects: May add to *trail*
   
   #:compile-clause       ;; (compile-clause head body)
                          ;; Compile single Prolog clause (head :- body).
                          ;; Returns: compiled clause
                          ;; Side effects: None (pure compilation)
   
   #:show-prolog-vars     ;; (show-prolog-vars term bindings)
                          ;; Display variable values in term using bindings.
                          ;; Useful for debugging Prolog queries.
                          ;; Returns: human-readable string
                          ;; Side effects: None
   
   #:prolog-error         ;; EXCEPTION TYPE
                          ;; Signaled on Prolog evaluation error.
                          ;; Examples: undefined predicate, type error, etc.
   
   #:prolog-ignore        ;; MACRO: (prolog-ignore goal)
                          ;; Execute goal, ignore failures (always succeeds).
                          ;; Returns: T
                          ;; Side effects: Executes goal (may have side effects)
   
   #:delete-functor       ;; (delete-functor name arity)
                          ;; Remove Prolog predicate from global registry.
                          ;; Returns: T if deleted, NIL if not found
                          ;; Side effects: Modifies *prolog-global-functors*
   
   #:set-functor-fn       ;; (set-functor-fn name arity fn)
                          ;; Set/replace Prolog functor's implementation.
                          ;; Advanced: allows dynamic predicate modification.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Modifies functor-object
   
   #:*seen-table*         ;; *seen-table* DYNAMIC VARIABLE
                          ;; Set of visited nodes during Prolog traversal.
                          ;; Prevents infinite loops in recursive predicates.
                          ;; WHY EXPOSED? Control over cycle detection
                          ;; RISK: Modifying may cause infinite loops
   
   #:*select-flat*        ;; *select-flat* DYNAMIC VARIABLE
                          ;; Flag: flatten results in select query?
                          ;; Returns: T or NIL
   
   #:*select-list*        ;; *select-list* DYNAMIC VARIABLE
                          ;; Accumulator for select query results.
                          ;; Returns: list of results
   
   #:select-count         ;; (select-count goal limit)
                          ;; Execute goal, return up to limit solutions.
                          ;; Returns: list of solutions (up to limit)
                          ;; Side effects: None
   
   #:*select-count*       ;; *select-count* DYNAMIC VARIABLE
                          ;; Limit on number of solutions in select.
                          ;; Returns: integer or NIL (no limit)
   
   #:*select-skip*        ;; *select-skip* DYNAMIC VARIABLE
                          ;; Skip first N solutions in select.
                          ;; Useful for pagination.
                          ;; Returns: integer or 0 (no skip)
   
   #:*select-current-count* ;; *select-current-count* DYNAMIC VARIABLE
                          ;; Counter of solutions collected so far.
                          ;; Returns: integer
   
   #:*select-current-skip* ;; *select-current-skip* DYNAMIC VARIABLE
                          ;; Counter of solutions skipped so far.
                          ;; Returns: integer
   
   #:select-one           ;; (select-one goal)
                          ;; Execute goal, return single solution (first match).
                          ;; Like select but limited to 1 result.
                          ;; Returns: solution or NIL
                          ;; Side effects: None
   
   #:select-flat          ;; (select-flat goal)
                          ;; Execute goal, flatten results (remove nesting).
                          ;; Returns: flattened list of results
                          ;; Side effects: None
   
   #:select-first         ;; (select-first goal)
                          ;; Execute goal, return first solution only.
                          ;; Alias for select-one.
                          ;; Returns: solution or NIL
                          ;; Side effects: None
   
   #:do-query             ;; (do-query goal)
                          ;; Execute goal and iterate over all solutions.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Iterates (binds *trail*, may modify *seen-table*)
   
   #:map-query            ;; (map-query fn goal)
                          ;; Apply fn to each solution of goal.
                          ;; Like map but for Prolog queries.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Calls fn for each solution
   
   #:valid-prolog-query-p ;; (valid-prolog-query-p goal)
                          ;; Predicate: is goal a valid Prolog query?
                          ;; Returns: T or NIL
                          ;; Side effects: None
   
   #:init-prolog          ;; (init-prolog graph)
                          ;; Initialize Prolog engine for graph.
                          ;; Sets up *prolog-graph*, *trail*, etc.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Initializes global Prolog state
   
   #:*prolog-graph*       ;; *prolog-graph* DYNAMIC VARIABLE
                          ;; Graph being queried by Prolog.
                          ;; Bound by init-prolog.
                          ;; WHY EXPOSED? Advanced Prolog customization
                          ;; RISK: Modifying may affect query results
   
   #:*prolog-trace*       ;; *prolog-trace* DYNAMIC VARIABLE
                          ;; Flag: trace Prolog goal execution?
                          ;; Returns: T or NIL
   
   #:trace-prolog         ;; (trace-prolog)
                          ;; Enable Prolog tracing (debug output).
                          ;; Shows goal expansion, unification steps.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Sets *prolog-trace* to T
   
   #:untrace-prolog       ;; (untrace-prolog)
                          ;; Disable Prolog tracing.
                          ;; Returns: NIL (side effects only)
                          ;; Side effects: Sets *prolog-trace* to NIL
   
   #:make-node-table      ;; (make-node-table)
                          ;; Create empty node table for Prolog.
                          ;; Used for deduplication in traversal.
                          ;; Returns: hash-table
                          ;; Side effects: Memory allocation
   
   #:node-equal           ;; (node-equal node1 node2)
                          ;; Prolog equality test for nodes.
                          ;; Compares IDs (not object identity).
                          ;; Returns: T or NIL
                          ;; Side effects: None
   
   ))  ;; End of :export list

;; ==============================================================================
;; END OF PACKAGE DEFINITION
;; ==============================================================================

;; CLOSING NOTES:
;;   This defpackage form is parsed at compile-time. No code is executed.
;;   The side effect is that Lisp creates a package object in the image
;;   with all 166 symbols registered as "external" (exported).
;;
;;   When subsequent files do (in-package :graph-db), they work in this
;;   package namespace. Their defun, defclass, etc. definitions automatically
;;   register in the :graph-db package (and become accessible via :export
;;   symbols defined here).
;;
;; RISK SUMMARY:
;;   🔴 BLOCKING: Unused "WORD" import (line 11) - verify or delete
;;   🟠 CRITICAL: 166 exports is large - categorization needed for Phase 3
;;   🟠 CRITICAL: Global variables exposed directly - should use macros
;;   🟡 WARNING: Prolog 51 symbols may belong in separate package
;;   🟡 WARNING: SBCL-only RW-lock primitives - need CCL/LispWorks alternatives
;;   🟡 WARNING: MOP shadowing fragile across Lisp implementations

;; END OF FILE: src/package.lisp