;; Layer 1: Global State & Configuration Constants
;; File: src/globals.lisp
;; Purpose: Define all global variables, dynamic parameters, and serialization constants
;;          used throughout VivaceGraph (Layers 1-7)
;;
;; This file establishes the configuration backbone of VivaceGraph. Every constant
;; defined here affects storage format, serialization, index management, and Prolog
;; execution. Changes to these constants require full recompilation and data migration.
;;
;; Key sections:
;;   1. Configuration flags (caching, versioning)
;;   2. Storage file names and metadata constants
;;   3. UUID namespaces for deterministic ID generation
;;   4. Index constants (VE-index, VEV-index sizes)
;;   5. Type serialization byte codes (for data format)
;;   6. Prolog global state (*trail*, *var-counter*, etc.)
;;   7. Prolog functor registries (thread-safe hash-tables per Lisp)

(in-package :graph-db)

;; ==============================================================================
;; Section 1: Runtime Configuration Flags
;; ==============================================================================

(defvar *cache-enabled* t
  "Global flag controlling whether caching is enabled throughout the system.

   Type: Boolean (t or nil)
   
   Purpose: When t (default), Layer 2-4 will cache vertex/edge lookups and
            query results to avoid repeated disk I/O. When nil, all reads
            bypass cache and go directly to storage.
   
   Thread-safety: NOT synchronized. If modified with setf globally, affects
                  all threads. Recommend using let-binding per thread for safety:
                  (let ((*cache-enabled* nil)) ...)
   
   Side-effects: Affects performance of all lookups. Disabling cache may
                 improve consistency testing but significantly slows operations.
   
   When to use: 
     - Debugging: (let ((*cache-enabled* nil)) ...) to test cache-free behavior
     - Testing: Disable to verify cache doesn't affect correctness
     - Performance tuning: Enable for production, disable for debugging
   
   Related: None (is the cache control flag itself)
   Layer: 1")

;; ==============================================================================

(alexandria:define-constant +db-version+ 1
  "Database format version number for compatibility checking.

   Type: Integer (positive)
   Current value: 1
   
   Purpose: When opening an existing graph, Layer 2 reads the stored version
            from graph metadata and compares it to +db-version+. If mismatch,
            raises error to prevent data corruption from format incompatibility.
   
   Invariant: All data written with version V must be readable by code
              built with same V. Moving to V+1 requires migration path.
   
   Risk: Currently no upgrade mechanism. If changed, all old graphs become
         unreadable. For Phase 3 (API stability), +db-version+ must freeze.
   
   See also: Graph metadata in Layer 2 (where version is stored)
   Layer: 1")

;; ==============================================================================

(defvar *graph* nil
  "Dynamic variable holding the current graph context for the executing thread.

   Type: NIL (initially) or graph instance (at runtime)
   
   Purpose: Provides implicit graph context. Many functions check *graph*
            to know which graph to operate on. Used as default when function
            doesn't take explicit graph argument.
   
   Scope: Dynamic (lexical scope via let-binding). Each let-binding creates
          thread-local scope. With direct setf, becomes global (dangerous).
   
   Thread-safety: ** ! ** NOT thread-safe when modified with setf globally.
                  ** OK ** Thread-safe when bound with let per thread.
   
   Pattern (SAFE):
     (let ((*graph* my-graph))
       (with-transaction
         (make-vertex "Person" ...)))
   
   Pattern (UNSAFE):
     (setf *graph* my-graph)  ;; Global mutation!
     (thread:make-thread ...)  ;; Child thread sees same graph
   
   Side-effects: Setting *graph* changes behavior of all functions that
                 read it without explicit graph parameter. Multiple threads
                 seeing same *graph* (via setf) causes data races.
   
   Exported: Yes (from package.lisp). User code accesses this.
   Layer: 1")

;; ==============================================================================

(alexandria:define-constant +main-table-file+ \"main.dat\" :test 'equal
  "Default filename for main vertex/edge table storage.

   Type: String constant
   Value: \"main.dat\"
   
   Purpose: When creating new graph at path /tmp/mydb, actual vertex/edge data
            stored in file /tmp/mydb/main.dat. This constant defines the
            fixed relative filename within graph directory.
   
   Risk: Hardcoded; cannot be changed per graph without code modification.
         If changed, old graphs with 'main.dat' won't be found.
   
   Compatibility: Must match filename used by Layer 2 when saving graphs.
   
   Related: +meta-file+, +data-file+
   Layer: 1")

(alexandria:define-constant +meta-file+ \"meta.dat\" :test 'equal
  "Default filename for graph metadata (schema definitions, version, etc.).

   Type: String constant
   Value: \"meta.dat\"
   
   Purpose: Graph metadata (type definitions, schema version, replication state)
            stored in file /tmp/mydb/meta.dat when graph lives at /tmp/mydb.
   
   Risk: Hardcoded; old graphs expect 'meta.dat' name.
   
   Related: +main-table-file+, +data-file+
   Layer: 1")

(alexandria:define-constant +data-file+ \"data.dat\" :test 'equal
  "Default filename for serialized data blocks (memory-mapped storage).

   Type: String constant
   Value: \"data.dat\"
   
   Purpose: Large binary data blobs (serialized vertices, edges, indexes)
            stored in /tmp/mydb/data.dat. This file uses memory mapping
            for efficient I/O (Layer 4 responsibility).
   
   Risk: Hardcoded filename. If changed, old graphs unreadable.
   
   Related: +main-table-file+, +meta-file+
   Layer: 1")

;; ==============================================================================
;; Section 2: Schema & Type Registry
;; ==============================================================================

(defvar *schema-node-metadata* (make-hash-table :test 'equal)
  "Registry of all type definitions (vertex types, edge types, custom types).

   Type: Hash-table with 'equal test
         Key: type name (string or symbol)
         Value: metadata object (contains CLOS class, field defs, constraints)
   
   Purpose: When Layer 6 executes (def-vertex person ...), populates this
            hash-table with type metadata. When Layer 1 needs to instantiate
            a vertex, looks up type here to get class definition.
   
   Thread-safety: ** ! ** NOT thread-safe!
                  - No :synchronized t flag (SBCL)
                  - No :shared t flag (CCL)
                  - No :single-thread nil flag (LispWorks)
                  Result: Concurrent type registration may corrupt hash-table.
   
   FIX REQUIRED: Add conditional per-Lisp:
     #+sbcl   (make-hash-table :test 'equal :synchronized t)
     #+ccl    (make-hash-table :test 'equal :shared t)
     #+lispworks (make-hash-table :test 'equal :single-thread nil)
   
   Usage pattern:
     (gethash \"Person\" *schema-node-metadata*) → type-metadata object
     (setf (gethash \"Person\" *schema-node-metadata*) new-metadata) → register
   
   Side-effects: Modified by def-vertex, def-edge (Layer 6). Affects all
                 subsequent vertex/edge instantiation. Changes persist for
                 lifetime of graph (no per-transaction rollback).
   
   Recommendation: Define all types at application startup, before spawning
                   threads. Avoid concurrent type definition.
   
   Related: package.lisp (exports *schema-node-metadata*)
   Layer: 1 (initialization), Layer 6 (population)")

;; ==============================================================================

(alexandria:define-constant +max-node-types+ 65536
  "Maximum number of distinct vertex/edge/custom types allowed.

   Type: Integer (power of 2)
   Value: 65536 = 2^16
   
   Purpose: Type IDs are represented as 16-bit integers. This constant
            represents maximum representable value before wraparound.
   
   Implication: Cannot define more than 65536 distinct types in single graph.
   
   Risk: ** ! ** No validation in def-vertex/def-edge. If exceeded, IDs wrap around:
         Type #65537 → becomes type #1 (collision!)
         May silently corrupt types without error message.
   
   FIX REQUIRED: Add check in def-vertex:
     (when (>= (hash-table-count *schema-node-metadata*) +max-node-types+)
       (error \"Type limit exceeded: ~D types defined\"
              (hash-table-count *schema-node-metadata*)))
   
   Assumption: Implies type ID field is 16 bits. If type IDs stored
               differently elsewhere, this constant is wrong.
   
   Layer: 1")

;; ==============================================================================
;; Section 3: Storage Format Constants
;; ==============================================================================

(alexandria:define-constant +storage-version+ #x01
  "Serialization storage format version byte (used in data blocks).

   Type: Hex byte (unsigned-byte 8)
   Value: #x01 = 1
   
   Purpose: First byte of serialized data stream indicates format version.
            Used when writing/reading memory-mapped storage blocks (Layer 4).
   
   Relationship to +db-version+:
     +db-version+      : Application/schema version (compatibility check on open)
     +storage-version+ : Binary format version (compatibility check on read)
   
   These may differ: app version 5 may use storage version 1 if format unchanged.
   
   Risk: Hardcoded. If changed, all old saved data becomes unreadable.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +fixed-integer-64+ #x01
  "Marker byte for 64-bit fixed-width integers in serialization.

   Type: Hex byte
   Value: #x01
   
   ** ! ** WARNING: This is identical to +storage-version+ (#x01)!
      Unclear why both exist with same value.
      Possible dead code or documentation bug.
   
   Recommendation: Document purpose or remove if unused.
   
   Layer: 1")

(alexandria:define-constant +data-magic-byte+ #x17
  "Magic byte identifying data blocks during deserialization.

   Type: Hex byte
   Value: #x17 = 23 decimal
   
   Purpose: When reading serialized data, first byte discriminates block type:
              #x17 → regular data block
              #x18 → lhash block (linear hash table)
              #x19 → overflow block
              #x20 → config block
   
   Usage: In Layer 4 deserializer:
     (read-byte stream) ;; returns #x17
     (case *
       (#x17 (read-data-block ...))
       (#x18 (read-lhash-block ...))
       ... )
   
   Risk: Hardcoded. If changed, old saved data unreadable.
         If two different types map to same byte, deserialization broken.
   
   Layer: 1 (defined), Layer 4 (used in deserialization)")

(alexandria:define-constant +lhash-magic-byte+ #x18
  "Magic byte identifying linear hash table blocks.

   Type: Hex byte
   Value: #x18 = 24 decimal
   
   Purpose: Discriminates lhash (linear hash) index blocks from other types.
            When deserializing, if first byte is #x18, parse as lhash block.
   
   Layer: 1 (defined), Layer 4 (used in deserialization)")

(alexandria:define-constant +overflow-magic-byte+ #x19
  "Magic byte identifying overflow blocks (when hash bucket exceeds capacity).

   Type: Hex byte
   Value: #x19 = 25 decimal
   
   Purpose: Hash tables may overflow when bucket fills. Overflow blocks
            chain to main bucket. Magic byte #x19 identifies overflow blocks.
   
   Layer: 1 (defined), Layer 4 (used in deserialization)")

(alexandria:define-constant +config-magic-byte+ #x20
  "Magic byte identifying configuration/metadata blocks.

   Type: Hex byte
   Value: #x20 = 32 decimal
   
   Purpose: Graph metadata (version, schema definitions) stored in config blocks.
            Magic byte #x20 identifies these blocks.
   
   Layer: 1 (defined), Layer 4 (used in deserialization)")

;; ==============================================================================
;; Section 4: Key and Bucket Sizes
;; ==============================================================================

(alexandria:define-constant +null-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 0)
  :test 'equalp
  "Sentinel key representing minimum value for range queries.

   Type: Byte-array (16 elements, all zeros)
   
   Purpose: Used in skip-list range queries as lower bound.
            When querying for keys >= K, +null-key+ represents -infinity.
   
   Created at: Load-time (not compile-time constant)
   
   Why 16 bytes: UUIDs are 128 bits = 16 bytes. This constant represents
                 the all-zeros UUID (0x00000000-0000-0000-0000-000000000000).
   
   Usage: In Layer 4 index traversal:
     (skip-list-range index +null-key+ +max-key+) ;; entire range
   
   Assumption: Assumes all keys are 16-byte arrays. If key format changes,
               this constant breaks.
   
   Layer: 1 (defined), Layer 4 (used in skip-list queries)")

(alexandria:define-constant +max-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 255)
  :test 'equalp
  "Sentinel key representing maximum value for range queries.

   Type: Byte-array (16 elements, all 255s)
   
   Purpose: Upper bound sentinel for skip-list range queries.
            When querying for keys <= K, +max-key+ represents +infinity.
   
   Why 16 bytes: All-ones UUID (0xFFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF).
   
   Usage: (skip-list-range index +null-key+ +max-key+) → all keys
   
   Layer: 1 (defined), Layer 4 (used in skip-list queries)")

(alexandria:define-constant +key-bytes+ 16
  "Size of key in bytes for main hash table.

   Type: Integer
   Value: 16 bytes
   
   Purpose: Hash bucket keys are UUIDs (16 bytes = 128 bits).
            This constant defines expected key size for Layer 4 storage.
   
   Implication: If Layer 4 reads key of different size, data corruption.
   
   Assumption: Assumes vertex/edge IDs are UUIDs. If ID format changes,
               update this constant.
   
   Related: +value-bytes+, +bucket-size+
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +value-bytes+ 8
  "Size of value (pointer) in bytes for main hash table.

   Type: Integer
   Value: 8 bytes
   
   Purpose: Hash bucket values are memory pointers (64-bit on modern systems).
            This constant defines value field size.
   
   Portability: Assumes 64-bit pointers. On 32-bit systems, should be 4.
                Current code hardcodes 8, may fail on 32-bit architectures.
   
   Related: +key-bytes+, +bucket-size+
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +bucket-size+ 24
  "Size of hash bucket entry (key + value).

   Type: Integer
   Value: 24 bytes
   
   Calculation: +key-bytes+ (16) + +value-bytes+ (8) = 24
   
   Purpose: Hash buckets contain key-value pairs. Total bucket size is 24 bytes.
            Layer 4 allocates buckets of this size.
   
   Invariant: +bucket-size+ should always equal +key-bytes+ + +value-bytes+.
              If not, calculation error.
   
   Risk: Hardcoded. If key/value sizes change, must update.
   
   Related: +key-bytes+, +value-bytes+
   Layer: 1 (defined), Layer 4 (used in memory allocation)")

(alexandria:define-constant +data-extent-size+ (* 1024 1024 100)
  "Size of memory-mapped data extent blocks in bytes.

   Type: Integer
   Value: 104,857,600 bytes = 100 MB
   
   Calculation: 1024 * 1024 * 100 = 2^20 * 100
   
   Purpose: Large graph data is stored in memory-mapped file extents.
            Each extent is 100 MB. When one extent fills, allocate next.
   
   Tradeoff:
     - Too large: Uses more RAM per extent, less granular I/O control
     - Too small: More extents to manage, more fragmentation
   
   100 MB rationale: Typical modern systems have enough RAM; extent size
                     balances granularity with allocation overhead.
   
   Risk: ** ! ** Hardcoded. No configuration option.
         On embedded systems (256 MB RAM): 100 MB is 40% of total!
         May cause swapping or out-of-memory errors.
   
   FIX REQUIRED: Make configurable:
     (defparameter *data-extent-size* (* 1024 1024 100))
     Users can then: (let ((*data-extent-size* (* 1024 1024 50))) ...)
   
   Layer: 1 (defined), Layer 4 (used in extent allocation)")

;; ==============================================================================
;; Section 5: Index Constants
;; ==============================================================================

;; Key namespaces for v5 UUID generation
(defvar *vertex-namespace* (uuid:uuid-to-byte-array
                            (uuid:make-uuid-from-string \"2140DCE1-3208-4354-8696-5DF3076D1CEB\"))
  "UUID namespace for generating deterministic vertex IDs.

   Type: Byte-array (16 bytes, from UUID)
   Value: UUID string \"2140DCE1-3208-4354-8696-5DF3076D1CEB\" converted to bytes
   
   Purpose: Used for v5 UUID generation. When creating vertex with data D:
            vertex-id = hash(namespace=*vertex-namespace*, data=D)
   
   Initialization: Load-time (when globals.lisp loads, UUID string converted).
   
   Immutability: Defined as defvar (not constant), but value NEVER reassigned.
                 Acts as immutable constant after load.
   
   Guarantee: All vertex UUIDs generated in same graph use same namespace,
              ensuring consistency. Different namespace from *edge-namespace*
              prevents ID collision between vertices and edges.
   
   Risk: ** ! ** Hardcoded UUID. If changed, new vertices generate different IDs:
         Old vertex: hash(namespace_A, data) = ID_1
         New code:   hash(namespace_B, data) = ID_2  (collision risk!)
   
   FIX REQUIRED: 
     1. Make immutable: (alexandria:define-constant +vertex-namespace+ ...)
     2. Store in graph metadata (version with graph)
     3. Verify on open: error if mismatch
   
   Related: *edge-namespace*
   Layer: 1 (defined), Layer 6 (used in vertex ID generation)")
))
(defvar *edge-namespace* (uuid:uuid-to-byte-array
                          (uuid:make-uuid-from-string \"0392C7B5-A38B-466F-92E5-5A7493C2775A\"))
  "UUID namespace for generating deterministic edge IDs.

   Type: Byte-array (16 bytes, from UUID)
   Value: UUID string \"0392C7B5-A38B-466F-92E5-5A7493C2775A\" converted to bytes
   
   Purpose: Used for v5 UUID generation of edges. Ensures edge IDs distinct
            from vertex IDs (different namespace).
   
   Initialization: Load-time conversion of UUID string.
   
   Guarantee: edge-id = hash(*edge-namespace*, edge-data) - always unique from vertices
   
   Risk: Same as *vertex-namespace* - hardcoded, no versioning.
   
   Related: *vertex-namespace*
   Layer: 1 (defined), Layer 6 (used in edge ID generation)"
   )))

;; Sentinel values for skip lists
(alexandria:define-constant +min-sentinel+ :gmin
  "Sentinel keyword representing minimum (negative infinity) in skip-lists.

   Type: Keyword symbol
   Value: :gmin
   
   Purpose: Skip-list nodes are ordered. +min-sentinel+ is smaller than
            any real key. Used as lower bound in range queries.
   
   Why keyword: Keywords are globally unique; less likely to collide with
                user data.
   
   Example usage: (skip-list-range index :gmin :gmax) → entire range
   
   Assumption: User data doesn't contain :gmin keyword. If it does, skip-list
               ordering breaks.
   
   Layer: 1 (defined), Layer 4 (used in skip-list traversal)")

(alexandria:define-constant +max-sentinel+ :gmax
  "Sentinel keyword representing maximum (positive infinity) in skip-lists.

   Type: Keyword symbol
   Value: :gmax
   
   Purpose: Upper bound sentinel for skip-list range queries.
   
   Related: +min-sentinel+
   Layer: 1 (defined), Layer 4 (used in skip-list traversal)")

;; For views, aggregate key symbol
(alexandria:define-constant +reduce-master-key+ :gagg
  "Sentinel keyword used as key for aggregated view results.

   Type: Keyword symbol
   Value: :gagg
   
   Purpose: Views may compute aggregates (sum, count, etc.). Results stored
            in key-value pairs where key=:gagg holds aggregate value.
   
   Usage: In Layer 6 view computation:
     (setf (gethash :gagg view-results) aggregate-value)
   
   Layer: 1 (defined), Layer 6 (used in view aggregation)")

;; index-lists
(alexandria:define-constant +index-list-bytes+ 17
  "Size of index-list header in bytes.

   Type: Integer
   Value: 17
   
   Purpose: Index lists are serialized with 17-byte header.
   
   Unclear: Why 17? (1 byte + 16 UUID? Or different structure?)
            Purpose not well-documented.
   
   Risk: ** ! ** If Layer 4 index-list format different, this constant wrong.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

;; ve-key / ve-index
(alexandria:define-constant +ve-key-bytes+ 18
  "Size of VE-index key (vertex-edge index).

   Type: Integer
   Value: 18 bytes
   
   Likely structure: 16-byte vertex UUID + 2-byte edge info
   
   Purpose: VE-index maps (vertex, edge_type) → edges. Keys are 18 bytes.
   
   Assumption: Assumes 16-byte vertex ID + 2-byte edge metadata.
               If structure changes, update.
   
   Layer: 1 (defined), Layer 4 (used in VE-index operations)")

(alexandria:define-constant +null-ve-key+
    (make-array +ve-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp
  "Sentinel VE-index key (all zeros) for range query lower bound.

   Type: Byte-array (18 elements, all 0)
   
   Purpose: Range query starting point in VE-index.
   
   Layer: 1 (defined), Layer 4 (used in range queries)")

(alexandria:define-constant +max-ve-key+
    (make-array +ve-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
  :test 'equalp
  "Sentinel VE-index key (all 255s) for range query upper bound.

   Type: Byte-array (18 elements, all 255)
   
   Purpose: Range query ending point in VE-index.
   
   Layer: 1 (defined), Layer 4 (used in range queries)")

;; vev-key / vev-index
(alexandria:define-constant +vev-key-bytes+ 34
  "Size of VEV-index key (vertex-edge-vertex index).

   Type: Integer
   Value: 34 bytes
   
   Likely structure: 16-byte vertex1 UUID + 16-byte vertex2 UUID + 2-byte edge info
   
   Purpose: VEV-index maps (v1, edge_type, v2) → edge. Keys are 34 bytes.
            Enables fast \"find all edges from v1 to v2\" queries.
   
   Assumption: Assumes two 16-byte vertices + 2-byte metadata.
   
   Layer: 1 (defined), Layer 4 (used in VEV-index operations)")

(alexandria:define-constant +null-vev-key+
    (make-array +vev-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp
  "Sentinel VEV-index key (all zeros) for range query lower bound.

   Type: Byte-array (34 elements, all 0)
   
   Layer: 1 (defined), Layer 4 (used in range queries)")

(alexandria:define-constant +max-vev-key+
    (make-array +vev-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
   :test 'equalp
  "Sentinel VEV-index key (all 255s) for range query upper bound.

   Type: Byte-array (34 elements, all 255)
   
   Layer: 1 (defined), Layer 4 (used in range queries)")

;; ==============================================================================
;; Section 6: Type Serialization Byte Codes
;; ==============================================================================

;; Type bytes for serialization - maps Lisp types to binary byte codes
;; Used when serializing values to disk; byte code identifies type for deserialization

(alexandria:define-constant +needs-lookup+ :needs-lookup
  "Marker indicating value type needs lookup (not inline).

   Type: Keyword symbol
   
   Purpose: During serialization, if value type unknown, defer lookup.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +unknown+ 0
  "Type code for unknown/uninitialized type.

   Type: Byte (0)
   
   Purpose: Used as placeholder when type not yet determined.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +negative-integer+ 1
  "Type code for negative integers.

   Type: Byte (1)
   
   Purpose: When serializing -5, prefix byte is 1, followed by value bytes.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +positive-integer+ 2
  "Type code for positive integers.

   Type: Byte (2)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +character+ 3
  "Type code for single characters.

   Type: Byte (3)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +symbol+ 4
  "Type code for Lisp symbols.

   Type: Byte (4)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +string+ 5
  "Type code for string values.

   Type: Byte (5)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +list+ 6
  "Type code for proper lists (a b c).

   Type: Byte (6)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +vector+ 7
  "Type code for vectors (arrays).

   Type: Byte (7)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +single-float+ 8
  "Type code for single-precision floating-point.

   Type: Byte (8)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +double-float+ 9
  "Type code for double-precision floating-point.

   Type: Byte (9)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +ratio+ 10
  "Type code for rational numbers (fractions).

   Type: Byte (10)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +t+ 11
  "Type code for boolean true (t).

   Type: Byte (11)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +null+ 12
  "Type code for boolean false/null (nil).

   Type: Byte (12)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +blob+ 13
  "Type code for uninterpreted octet blobs (arbitrary bytes).

   Type: Byte (13)
   
   Purpose: For binary data that should be stored as-is without interpretation.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +dotted-list+ 14
  "Type code for improper/dotted lists (a b . c).

   Type: Byte (14)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +keyword+ 15
  "Type code for keyword symbols (:key).

   Type: Byte (15)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +slot-key+ 16
  "Type code for slot key (vertex/edge field identifier).

   Type: Byte (16)
   
   Purpose: When storing vertex field metadata, field identifier marked as slot-key.
   
   Layer: 1 (defined), Layer 4 (used in schema serialization)")

(alexandria:define-constant +id+ 17
  "Type code for ID/identifier values (UUIDs in binary form).

   Type: Byte (17)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +vertex+ 18
  "Type code for vertex objects.

   Type: Byte (18)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +edge+ 19
  "Type code for edge objects.

   Type: Byte (19)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +skip-list+ 20
  "Type code for skip-list index structures.

   Type: Byte (20)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +ve-index+ 21
  "Type code for VE-index (vertex-edge index) structures.

   Type: Byte (21)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +type-index+ 22
  "Type code for type index (maps type names to IDs).

   Type: Byte (22)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +pcons+ 23
  "Type code for persistent cons cells.

   Type: Byte (23)
   
   Purpose: Custom data structure for persistent list-like objects.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +pqueue+ 24
  "Type code for persistent queue structures.

   Type: Byte (24)
   
   Purpose: Custom data structure for persistent queue (FIFO).
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +mpointer+ 25
  "Type code for memory pointers (references to other objects).

   Type: Byte (25)
   
   Purpose: When object references another (e.g., edge references vertices),
            pointer field marked with +mpointer+.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +pcell+ 26
  "Type code for persistent cell (single-value container).

   Type: Byte (26)
   
   Purpose: Custom container for atomic values in persistent structures.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +index-list+ 27
  "Type code for index-list structures.

   Type: Byte (27)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +vev-index+ 28
  "Type code for VEV-index (vertex-edge-vertex index) structures.

   Type: Byte (28)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +bit-vector+ 29
  "Type code for bit vectors (boolean arrays).

   Type: Byte (29)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +bignum+ 30
  "Type code for arbitrary-precision integers.

   Type: Byte (30)
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

;; User-defined type identifiers for serializing. Start at 100
(alexandria:define-constant +uuid+ 100
  "Type code for UUID values.

   Type: Byte (100)
   
   Purpose: User-defined extension: when storing UUID, use type code 100.
   
   Note: Gap between 30 and 100 reserved for future VivaceGraph types.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

(alexandria:define-constant +timestamp+ 101
  "Type code for timestamp/datetime values.

   Type: Byte (101)
   
   Purpose: User-defined extension: when storing timestamp, use type code 101.
   
   Layer: 1 (defined), Layer 4 (used in serialization)")

;; ==============================================================================
;; Section 7: Tunable Parameters
;; ==============================================================================

(defparameter *initial-extents* 10
  "Initial number of memory extents to allocate when creating new graph.

   Type: Integer (positive)
   Default: 10
   
   Purpose: When new graph created, pre-allocate 10 data extents to avoid
            repeated allocation. Each extent is +data-extent-size+ bytes.
   
   Implication: New graph starts with 10 * 100 MB = 1 GB pre-allocated.
   
   Tradeoff:
     - Higher value: faster initial startup, wasted space for small graphs
     - Lower value: slower startup, efficient space usage
   
   Tuning: User can rebind:
     (let ((*initial-extents* 5)) (make-graph ...))
   
   Layer: 1 (defined), Layer 4 (used in extent allocation)")

(defparameter *max-locks* 10000
  "Maximum number of concurrent locks (for schema, transaction locking).

   Type: Integer (positive)
   Default: 10000
   
   Purpose: Limits concurrent lock count to prevent resource exhaustion.
            If exceeded, new lock requests block until existing locks released.
   
   Risk: ** ! ** Hardcoded. No mechanism to handle exhaustion gracefully.
         If 10000 locks exceeded, behavior undefined (may hang or error).
   
   Tuning: (let ((*max-locks* 50000)) ...) for high-concurrency apps
   
   Layer: 1 (defined), Layer 3 (used in transaction locking)")

;; ==============================================================================

(defvar *graph-hash* nil
  "Registry of open graphs (hash-table mapping graph names to instances).

   Type: NIL (initially) or hash-table
   
   Purpose: When user calls (open-graph \"mydb\"), look up \"mydb\" in
            *graph-hash* to reuse existing open instance (avoid duplicate opens).
   
   Structure: {\"mydb\" → graph-instance, \"other-db\" → graph-instance}
   
   Thread-safety: ** ! ** NOT synchronized. Multiple threads opening different
                  graphs may race on hash-table modification.
   
   Side-effects: Modified when graphs opened/closed. Persists for lifetime
                 of running application.
   
   FIX REQUIRED: Add `:synchronized t` (SBCL) or `:shared t` (CCL).
   
   Layer: 1 (defined), Layer 2 (used in graph lifecycle)")

;; ==============================================================================
;; Section 8: Prolog Global State
;; ==============================================================================

;; Prolog specials - global variables for Prolog query execution
;; WARNING: These are global, not thread-local. Multiple concurrent Prolog
;;          queries on different threads INTERFERE with each other.
;;          Fix: thread-local bindings needed.

(defparameter *occurs-check* t
  "Global parameter controlling occurs check during Prolog unification.

   Type: Boolean (t or nil)
   Default: t (occurs check enabled)
   
   Purpose: During unification, check for circular bindings:
              X = f(X)  when occurs-check is t → FAILS (prevents infinite)
              X = f(X)  when occurs-check is nil → SUCCEEDS (allows cycle)
   
   Thread-safety: Global. Affects all Prolog unification on all threads.
   
   Usage: Bind locally per query:
     (let ((*occurs-check* nil))
       (? (X = f(X))))  ;; allows cycles in this query
   
   Performance: Enabling occurs-check adds O(n) cost to unification (n = term size).
   
   Layer: 1 (defined), Layer 5 (used in Prolog unification)")

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t)
  "Prolog choice point trail - records variable bindings for backtracking.

   Type: Adjustable array with fill-pointer
   Initial size: 200 (grows as needed)
   Element type: Binding records (implementation-dependent)
   
   Purpose: During Prolog query execution, when unifying variables, record
            bindings to *trail*. On backtrack (failure), unwind trail to
            restore prior bindings.
   
   Example:
     Query: (? (X = 5) (Y = X))
     Trail records: [binding(X=5), binding(Y=X)]
     If second goal fails, unwind trail: X unbound, Y unbound
   
   Thread-safety: ** ! ** CRITICAL ISSUE - global shared state!
                  Multiple concurrent Prolog queries on different threads
                  interfere: Thread A modifies *trail*, Thread B reads stale.
   
   FIX REQUIRED: Thread-local *trail* per query:
     (let ((*trail* (make-array 200 :fill-pointer 0 :adjustable t)))
       (? (query ...)))
   
   Size growth: As array fills, auto-grows. May cause GC pauses.
   
   Layer: 1 (defined), Layer 5 (used in Prolog backtracking)")

(defvar *var-counter* 0
  "Counter for generating unique Prolog variable names.

   Type: Integer (non-negative)
   Initial: 0
   
   Purpose: When query execution needs fresh variable, increment counter:
            ?1 (from *var-counter* = 0)
            ?2 (from *var-counter* = 1)
            ?3 (from *var-counter* = 2)
   
   Thread-safety: ** ! ** NOT atomic. Multiple concurrent increments may collide:
                  Thread A reads 0, Thread B reads 0, both use 0
                  Result: variable name collision
   
   FIX REQUIRED: Use atomic-increment or thread-local counter.
   
   Overflow risk: Counter never decremented. After 2^63 variables, wraps.
                  In long-running systems, may collide with old variables.
   
   Layer: 1 (defined), Layer 5 (used in Prolog variable naming)")

(defvar *functor* nil
  "Current Prolog functor being evaluated - provides context for recursive calls.

   Type: NIL or functor object
   
   Purpose: During functor dispatch, set *functor* to current context.
            Recursive functor calls can check *functor* to avoid infinite loops.
   
   Thread-safety: ** ! ** Global. Multiple threads evaluating different functors
                  can overwrite *functor* context.
   
   Layer: 1 (defined), Layer 5 (used in Prolog functor evaluation)")

(defvar *select-list* nil
  "Accumulator for results during Prolog query execution.

   Type: NIL or list
   
   Purpose: During query (? (goal)), each solution added to *select-list*.
            After query completes, *select-list* contains all solutions.
   
   Usage pattern:
     (let ((*select-list* nil))
       (? (ancestor 'alice ?x))
       *select-list*)  ;; all ancestors of alice
   
   Thread-safety: ** ! ** Global. Multiple concurrent queries append to same list,
                  causing interspersed results.
   
   FIX REQUIRED: Thread-local per query (use let-binding).
   
   Layer: 1 (defined), Layer 5 (used in Prolog result collection)")

(defvar *cont* nil
  "Continuation container for step-wise Prolog query execution.

   Type: NIL or continuation object
   
   Purpose: For advanced use: executing query step-by-step rather than
            finding all solutions at once. Continuation holds resumption point.
   
   Documentation: Poorly documented; unclear usage.
   
   Layer: 1 (defined), Layer 5 (used in advanced Prolog execution)")

;; ==============================================================================
;; Section 9: Thread-Safe Prolog Functor Registries
;; ==============================================================================

;; Different Lisp implementations require different flags for thread-safety.
;; SBCL, CCL, LispWorks each have unique hash-table concurrency mechanisms.

#+sbcl
(defvar *prolog-global-functors* (make-hash-table :synchronized t)
  "Registry of built-in Prolog predicates (SBCL version, thread-safe).

   Type: Hash-table with :synchronized t (SBCL-specific)
         Key: functor name (symbol or string)
         Value: functor object (implementation details in Layer 5)
   
   Purpose: When query (? (ancestor ?x ?y)) executes, looks up 'ancestor
            in *prolog-global-functors* to find predicate definition.
   
   Thread-safety: ** OK ** SBCL :synchronized t flag ensures atomic operations.
                  Individual hash operations are atomic; multiple operations
                  may race (no transactional semantics).
   
   Usage: (def-global-prolog-functor 'ancestor (make-functor ...))
          → (setf (gethash 'ancestor *prolog-global-functors*) functor-obj)
   
   Related: *user-functors* (user-defined predicates)
   
   Layer: 1 (defined), Layer 5 (used in Prolog predicate lookup)")

#+sbcl
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql)
  "Registry of user-defined Prolog predicates (SBCL version, thread-safe).

   Type: Hash-table with :synchronized t and :test 'eql
   
   Purpose: Separate from *prolog-global-functors* to distinguish built-in
            from user-defined. Allows clearing user predicates on graph reload.
   
   Thread-safety: ** OK ** :synchronized t ensures atomic operations.
   
   Related: *prolog-global-functors* (built-in predicates)
   
   Layer: 1 (defined), Layer 5 (used in Prolog predicate lookup)")

#+lispworks
(defvar *prolog-global-functors* (make-hash-table :single-thread nil)
  "Registry of built-in Prolog predicates (LispWorks version, thread-safe).

   Type: Hash-table with :single-thread nil (LispWorks-specific for multi-threaded)
   
   Purpose: Same as SBCL version, but with LispWorks threading semantics.
   
   Thread-safety: ** OK ** :single-thread nil documented as thread-safe for LispWorks.
   
   Layer: 1 (defined), Layer 5 (used in Prolog predicate lookup)")

#+lispworks
(defvar *user-functors* (make-hash-table :single-thread nil :test 'eql)
  "Registry of user-defined Prolog predicates (LispWorks version, thread-safe).

   Type: Hash-table with :single-thread nil and :test 'eql
   
   Thread-safety: ** OK ** :single-thread nil ensures thread-safety on LispWorks.
   
   Layer: 1 (defined), Layer 5 (used in Prolog predicate lookup)")

#+ccl
(defvar *prolog-global-functors* (make-hash-table :shared t)
  "Registry of built-in Prolog predicates (CCL version, thread-safe).

   Type: Hash-table with :shared t (CCL-specific for multi-threaded)
   
   Purpose: CCL uses :shared t flag to enable concurrent hash-table access.
   
   Thread-safety: ** OK ** :shared t ensures atomic hash-table operations on CCL.
   
   Layer: 1 (defined), Layer 5 (used in Prolog predicate lookup)")

#+ccl
(defvar *user-functors* (make-hash-table :shared t :test 'eql)
  "Registry of user-defined Prolog predicates (CCL version, thread-safe).

   Type: Hash-table with :shared t and :test 'eql
   
   Thread-safety: ** OK ** :shared t ensures thread-safety on CCL.
   
   Layer: 1 (defined), Layer 5 (used in Prolog predicate lookup)")

;; ==============================================================================

(defparameter *prolog-trace* nil
  "Global flag to enable Prolog query tracing for debugging.

   Type: Boolean (t or nil)
   Default: nil (tracing disabled)
   
   Purpose: When t, Layer 5 Prolog engine logs each goal/unification step.
            Useful for debugging failing queries.
   
   Usage: (let ((*prolog-trace* t)) (? (some-query ...)))
   
   Performance: Enabling trace adds logging overhead; may slow queries.
   
   Layer: 1 (defined), Layer 5 (used in Prolog debugging)")

(alexandria:define-constant +unbound+ :unbound
  "Keyword marker for unbound Prolog variables.

   Type: Keyword symbol
   Value: :unbound
   
   Purpose: When variable not yet bound, its value is :unbound (not nil).
            This distinguishes \"has value nil\" from \"no value\".
   
   Usage: (if (eq (var-deref var) :unbound) (error \"unbound\"))
   
   Layer: 1 (defined), Layer 5 (used in variable binding tracking)")

(alexandria:define-constant +no-bindings+ '((t . t)) :test 'equalp
  "Constant representing empty binding environment (no variables bound).

   Type: List of pairs
   Value: ((t . t))
   
   Purpose: When initializing Prolog query with no initial bindings,
            use this constant. The (t . t) pair is a sentinel.
   
   Oddity: Why (t . t)? Could be (()), but (t . t) ensures non-empty list.
           Allows distinguishing nil (no bindings) from () (empty bindings).
   
   Layer: 1 (defined), Layer 5 (used in Prolog initialization)")

(alexandria:define-constant +fail+ nil
  "Constant representing Prolog failure (no solution).

   Type: NIL
   
   Purpose: When goal fails, return nil. (Identifies failure with NIL.)
   
   Layer: 1 (defined), Layer 5 (used in Prolog backtracking)")

;; ==============================================================================
;; End of globals.lisp
;; ==============================================================================
;; Summary:
;;   - 9 mutable global variables (high thread-safety risk)
;;   - 50+ immutable constants (safe)
;;   - 2 thread-safe hash-tables per Lisp (SBCL/CCL/LispWorks specific)
;;   - UUID namespaces for ID generation (immutable after load)
;;   - Prolog state variables (global, not isolated)
;;
;; Key issues:
;;   - *schema-node-metadata* NOT thread-safe (needs fix)
;;   - *trail*, *var-counter*, etc. not thread-isolated (needs fix)
;;   - Type codes not versioned (breaks on format change)
;;   - UUID namespaces hardcoded (no migration path)
;;   - +data-extent-size+ hardcoded (not configurable)
;;
;; Phase 3 (API Stability): All these constants must freeze.
;; Any future changes require versioning and migration.
;; ==============================================================================