# VivaceGraph - Layer 1: Infrastructure & Base Utilities

## Table of Contents

1. [Overview](#overview)
2. [Purpose and Responsibilities](#purpose-and-responsibilities)
3. [Layer 1 Components](#layer-1-components)
4. [Detailed Files](#detailed-files)
5. [Key Global Constants](#key-global-constants)
6. [Load Order](#load-order)
7. [Design Patterns](#design-patterns)

## Overview

**Layer 1** is the **ROOT** upon which all of VivaceGraph is built. It includes:

- **Namespace configuration** (Lisp package)
- **Global variables** and system constants
- **Custom exception handling**
- **Utility functions** reusable throughout the code
- **CLOS extensions** (Meta-Object Protocol)
- **Unique ID generation** (UUIDs)
- **Random number generator** (Mersenne Twister)
- **Graph statistics** (performance metrics)
- **Base classes** (GRAPH, NODE)

### Key Features

- ✓ Fully **compatible with multiple Lisps** (SBCL, LispWorks, CCL)
- ✓ **No circular dependencies** (this layer does not import from other layers)
- ✓ **Thread-safe** (where appropriate)
- ✓ **Extensible** via CLOS and metaclasses

### Lines of Code

```
File                       Lines
──────────────────────────────────
package.lisp                 188
globals.lisp                 133
conditions.lisp               83
utilities.lisp               483
clos.lisp                     88
uuid.lisp                    121
random.lisp                  254
stats.lisp                    77
graph-class.lisp              84
node-class.lisp              174
──────────────────────────────────
TOTAL                      1,685 lines
```

## Purpose and Responsibilities

### Why does Layer 1 exist?

VivaceGraph needs a **solid foundation** before it can:
- Create persistent hash tables (Layer 4)
- Manage transactions (Layer 3)
- Synchronize memory access (Layer 2)
- Index graphs (Layer 5)
- Model data (Layer 6)

### Specific Responsibilities

| Responsibility | File | Reason |
|----------------|------|--------|
| Define package and exports | `package.lisp` | Create public namespace |
| Shared global variables | `globals.lisp` | System state |
| Custom exceptions | `conditions.lisp` | Robust error handling |
| Helpers and utilities | `utilities.lisp` | Reusable functions |
| CLOS metaclasses | `clos.lisp` | Extend class system |
| Unique identifiers | `uuid.lisp` | IDs for nodes/edges |
| Randomness | `random.lisp` | If needed (Mersenne Twister) |
| Graph metrics | `stats.lisp` | Performance and monitoring |
| GRAPH structure | `graph-class.lisp` | Root DB object |
| NODE structure | `node-class.lisp` | Base metaclass for data |

## Layer 1 Components

### Internal Dependency Map

```
LAYER 1 - INTERNAL DEPENDENCIES
================================

package.lisp
    ↓
globals.lisp
    ↓
conditions.lisp
    ↓
utilities.lisp
    ├─→ uuid.lisp
    ├─→ random.lisp
    └─→ clos.lisp
        ↓
    graph-class.lisp (defines graph-class metaclass)
        ↓
    node-class.lisp (defines node-class metaclass)
        ↓
    stats.lisp

NOTE: There are no circular dependencies.
Each file only depends on previous ones.
```

### Interface Structure

```
PUBLIC (exported in package.lisp):
├─ make-graph, open-graph, close-graph, lookup-graph
├─ execute-tx, with-transaction, commit, rollback
├─ def-node-type, def-vertex, def-edge
├─ lookup-vertex, lookup-edge, lookup-node-type-by-name
├─ vertex, edge, generic-edge, generic-vertex
├─ id, string-id, type-id, revision, deleted-p
├─ def-view, map-view, map-reduced-view, invoke-graph-view
├─ def-global-prolog-functor, compile-body, unify, ?, ?-, cut
├─ make-rw-lock, with-read-lock, with-write-lock (SBCL-specific)
└─ ... (147 symbols total)

PRIVATE (internal use):
├─ *graphs*, *graph*, *schema-node-metadata*
├─ +null-key+, +max-key+, +key-bytes+, +value-bytes+
├─ +vertex-namespace+, +edge-namespace+
├─ Serialization constants (+positive-integer+, etc.)
├─ *trail*, *var-counter*, *functor* (Prolog)
└─ ... (many more)
```

## Detailed Files

### 1. `package.lisp` (188 lines)

**Purpose:** Defines the `:graph-db` package and exports all public symbols.

**Content:**

```lisp
(defpackage #:graph-db
  (:use #:cl
        #:bordeaux-threads
        #:local-time
        #+ccl #:closer-mop      ; metaclasses in CCL
        #+lispworks #:clos      ; CLOS in LispWorks
        #+sbcl #:sb-mop         ; Meta-Object Protocol in SBCL
        #+sbcl #:sb-pcl)        ; Portable Common Loops
  ;; Import Lisp-specific symbols
  ;; (shadow imports for compatibility)
  
  (:export #:make-graph #:open-graph #:close-graph
           #:lookup-graph #:graph-stats
           ;; ... 147 more symbols
           ))
```

**Responsibilities:**

| Task | Detail |
|------|--------|
| Create namespace | All VivaceGraph symbols live in `:graph-db` |
| Use dependencies | Includes `:cl`, `:bordeaux-threads`, `:local-time` |
| Compatibility | Imports MOP according to the Lisp (SBCL, LispWorks, CCL) |
| Shadow imports | Replaces some `closer-mop` symbols in CCL |
| Export public API | 147 user-accessible symbols |

**Key Patterns:**

1. **Conditional Compilation** (`#+` directives)
   ```lisp
   #+sbcl #:sb-mop          ; Only loaded in SBCL
   #+lispworks #:clos       ; Only loaded in LispWorks
   ```

2. **Shadow Imports** (replacing symbols)
   ```lisp
   (:shadowing-import-from "CLOSER-MOP" "STANDARD-METHOD")
   ```
   This is necessary because different Lisps implement CLOS differently.

**How it is used:**

```lisp
(in-package :graph-db)  ; All files begin like this
(defun make-graph ...)  ; Now it is graph-db:make-graph
```

### 2. `globals.lisp` (133 lines)

**Purpose:** Defines ALL system global variables and constants.

**Sections:**

#### 2.1 Dynamic Global Variables

```lisp
(defvar *cache-enabled* t)                    ; Enable cache?
(defvar *graph* nil)                          ; Current graph in session
(defvar *schema-node-metadata* 
  (make-hash-table :test 'equal))             ; Type metadata
```

#### 2.2 Configuration Constants

```lisp
(defconstant +db-version+ 1)                  ; Format version
(defconstant +storage-version+ #x01)          ; Storage version
(defconstant +main-table-file+ "main.dat")    ; Main file
(defconstant +meta-file+ "meta.dat")          ; Metadata
(defconstant +data-file+ "data.dat")          ; Data
(defconstant +max-node-types+ 65536)          ; Maximum types
```

#### 2.3 Magic Bytes (for serialization)

```lisp
(defconstant +data-magic-byte+     #x17)      ; Data marker
(defconstant +lhash-magic-byte+    #x18)      ; Persistent hash marker
(defconstant +overflow-magic-byte+ #x19)      ; Overflow marker
(defconstant +config-magic-byte+   #x20)      ; Configuration marker
```

These **magic bytes** allow VivaceGraph to identify what type of data it is reading from disk.

#### 2.4 Key Constants (IDs)

```lisp
(defconstant +null-key+      ;; 16 zero bytes
  (make-array '(16) :element-type '(unsigned-byte 8) 
              :initial-element 0))
(defconstant +max-key+       ;; 16 bytes of 255
  (make-array '(16) :element-type '(unsigned-byte 8) 
              :initial-element 255))
(defconstant +key-bytes+ 16)
(defconstant +value-bytes+ 8)
```

**Why 16 bytes?** Standard UUIDs are 128 bits = 16 bytes. This enables globally unique identification.

#### 2.5 UUID Namespaces

```lisp
(defvar *vertex-namespace* 
  (uuid:uuid-to-byte-array
   (uuid:make-uuid-from-string "2140DCE1-3208-4354-8696-5DF3076D1CEB")))
(defvar *edge-namespace* 
  (uuid:uuid-to-byte-array
   (uuid:make-uuid-from-string "0392C7B5-A38B-466F-92E5-5A7493C2775A")))
```

These **identify the node type**:
- Vertex v5 UUIDs use the vertex namespace
- Edge v5 UUIDs use the edge namespace

This guarantees that a vertex UUID never collides with an edge UUID, **even if they have the same content**.

#### 2.6 Index Constants

```lisp
(defconstant +min-sentinel+ :gmin)   ; Minimum sentinel
(defconstant +max-sentinel+ :gmax)   ; Maximum sentinel
(defconstant +reduce-master-key+ :gagg) ; Aggregation key
```

**Sentinels** are special values used in skip lists to mark beginning/end.

#### 2.7 Specialized Index Sizes

```lisp
(defconstant +index-list-bytes+ 17)
(defconstant +ve-key-bytes+ 18)      ; Vertex→Edge
(defconstant +vev-key-bytes+ 34)     ; Vertex→Vertex
```

**Explanation:**
- `+ve-key-bytes+` = 16 (vertex) + 2 (edge type) = 18
- `+vev-key-bytes+` = 16 (vertex1) + 16 (vertex2) + 2 (type) = 34

#### 2.8 Serialization Types

```lisp
(defconstant +unknown+ 0)
(defconstant +negative-integer+ 1)
(defconstant +positive-integer+ 2)
(defconstant +character+ 3)
(defconstant +symbol+ 4)
(defconstant +string+ 5)
(defconstant +list+ 6)
(defconstant +vector+ 7)
(defconstant +single-float+ 8)
(defconstant +double-float+ 9)
;; ... more types ...
(defconstant +vertex+ 18)
(defconstant +edge+ 19)
(defconstant +skip-list+ 20)
(defconstant +ve-index+ 21)
(defconstant +uuid+ 100)
(defconstant +timestamp+ 101)
```

When VivaceGraph **serializes an object** to disk, it first writes the type (1 byte), then the content. When deserializing, it reads the type and knows how to interpret the bytes.

#### 2.9 Prolog Variables (Query Engine)

```lisp
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0)
(defvar *functor* nil)
(defvar *select-list* nil)
(defconstant +unbound+ :unbound)
(defconstant +no-bindings+ '((t . t)))
(defconstant +fail+ nil)
```

These variables **control the Prolog engine** (used for Prolog-style queries).

#### 2.10 Thread Safety (per Lisp)

```lisp
#+sbcl
(defvar *prolog-global-functors* 
  (make-hash-table :synchronized t))   ; Thread-safe in SBCL

#+lispworks
(defvar *prolog-global-functors* 
  (make-hash-table :single-thread nil)) ; Multi-thread in LispWorks

#+ccl
(defvar *prolog-global-functors* 
  (make-hash-table :shared t))         ; Shared in CCL
```

Each Lisp has its own way of creating thread-safe hash tables. `globals.lisp` handles this automatically.

### 3. `conditions.lisp` (83 lines)

**Purpose:** Defines VivaceGraph's custom exceptions.

**Defined Exceptions:**

| Exception | Cause | Usage |
|-----------|-------|-------|
| `slave-auth-error` | Failed authentication in replication | Slave replication |
| `transaction-error` | Error during transaction | ACID control |
| `serialization-error` | Failed to serialize object | Persistence |
| `deserialization-error` | Failed to deserialize | Persistence |
| `stale-revision-error` | Attempt to update old version | Concurrency control |
| `duplicate-key-error` | Duplicate key in unique index | Indexes |
| `nonexistent-key-error` | Key not found | Search |
| `node-already-deleted-error` | Node already marked as deleted | Node operations |
| `vertex-already-deleted-error` | Deleted vertex | Inherits from node-already-deleted-error |
| `edge-already-deleted-error` | Deleted edge | Inherits from node-already-deleted-error |
| `invalid-view-error` | View does not exist | Views |
| `view-lock-error` | Error locking view | View synchronization |

**Definition Example:**

```lisp
(define-condition serialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Serialization failed for ~a because of ~a."
                       instance reason)))))
```

**How it is used:**

```lisp
(handler-case
    (serialize-node node)
  (serialization-error (e)
    (format t "Error: ~A~%" e)))
```

**Exception Hierarchy:**

```
ERROR (standard Lisp)
├─ slave-auth-error
├─ transaction-error
├─ serialization-error
├─ deserialization-error
├─ stale-revision-error
├─ duplicate-key-error
├─ nonexistent-key-error
├─ invalid-view-error
├─ view-lock-error
└─ node-already-deleted-error
   ├─ vertex-already-deleted-error
   └─ edge-already-deleted-error
```

### 4. `utilities.lisp` (483 lines)

**Purpose:** Utility functions reusable throughout VivaceGraph.

**Sections:**

#### 4.1 Debug and Logging

```lisp
(defun dbg (fmt &rest args)
  "Debug output to console"
  (apply #'format t fmt args)
  (terpri))

(defun ignore-warning (condition)
  "Suppress warnings"
  (muffle-warning))
```

#### 4.2 Byte and Memory Operations

```lisp
(defun get-random-bytes (&optional (count 16))
  "Read random bytes from /dev/urandom"
  (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
    ...))

(defun free-memory ()
  "Report free memory (Lisp-specific)"
  #+sbcl (- (sb-kernel::dynamic-space-size) 
            (sb-kernel:dynamic-usage))
  #+ccl  (ccl::%freebytes))
```

#### 4.3 Time Conversion

```lisp
(defvar *unix-epoch-difference* 
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  "Convert Lisp time to Unix time"
  (- universal-time *unix-epoch-difference*))

(defun get-unix-time ()
  "Get current time in Unix seconds"
  (universal-to-unix-time (get-universal-time)))

(defun gettimeofday ()
  "Get time with microseconds (FFI)"
  #+sbcl (multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
           (+ sec (/ msec 1000000)))
  #+ccl  (ccl:rlet ((tv :timeval)) ...)
  #+lispworks (fli:with-dynamic-foreign-objects ...))
```

**Why:** VivaceGraph needs precise timestamps for transactions and logs.

#### 4.4 Functional Operations

```lisp
(defun find-all (item sequence &key test test-not)
  "Find all matching elements"
  ...)

(defun find-anywhere (item tree)
  "Search for item in nested tree"
  ...)

(defun unique-find-anywhere-if (predicate tree)
  "Find unique elements satisfying predicate"
  ...)

(defun flatten (x)
  "Flatten nested list"
  ...)
```

#### 4.5 Symbol Operations

```lisp
(defun new-interned-symbol (&rest args)
  "Create interned symbol from strings"
  (intern (format nil "~{~a~}" args)))

(defun length=1 (list)
  "Is this a list of exactly 1 element?"
  (and (consp list) (null (cdr list))))
```

#### 4.6 UUID and ID Generation

```lisp
(defun gen-id ()
  "Generate random v4 UUID"
  (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))

(defun read-uuid-from-string (string)
  "Parse UUID from string (e.g.: 6ba7b810-9dad-11d1-80b4-00c04fd430c8)"
  ...)

(defun read-id-array-from-string (string)
  "Convert UUID string to byte array"
  ...)
```

#### 4.7 Generic Comparison (Key for Indexes)

```lisp
(defgeneric less-than (x y)
  (:documentation "Generic < comparison")
  (:method ((x (eql +min-sentinel+)) y) nil)
  (:method ((x (eql +max-sentinel+)) y) t)
  (:method ((x symbol) (y symbol)) 
    (string< (symbol-name x) (symbol-name y)))
  (:method ((x number) (y number)) (< x y))
  ...)

(defgeneric greater-than (x y)
  (:documentation "Generic > comparison")
  ;; Similar to less-than
  ...)
```

**Why do we need this?** VivaceGraph indexes must compare values of different types:
- Numbers vs numbers
- Strings vs strings
- Symbols vs symbols
- **And also "apples vs oranges"** (cross-type comparison)

The pattern is:
```
Type A vs Type B: type A always "wins" vs type B
Example: string > symbol > number > null > t
```

#### 4.8 Byte Vector Comparison

```lisp
(defun key-vector< (v1 v2)
  "Compare byte vectors lexicographically"
  ...)

(defun key-vector<= (v1 v2)
  ...)

(defun key-vector> (v1 v2)
  ...)
```

#### 4.9 Locks and Synchronization

```lisp
(defmacro with-lock ((lock &key whostate timeout) &body body)
  "Portable macro for acquiring locks"
  #+ccl
  `(do-with-lock ,lock ,whostate ,timeout (lambda () ,@body))
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sbcl
  `(sb-thread:with-recursive-lock (,lock) ,@body))

(defun make-semaphore ()
  "Create semaphore (Lisp-specific)"
  #+sbcl (sb-thread:make-semaphore)
  #+lispworks (mp:make-semaphore)
  #+ccl (ccl:make-semaphore))

(defmacro with-read-lock ((lock) &body body)
  "Read locks (CCL only)"
  #+ccl `(ccl:with-read-lock (,lock) ,@body))

(defmacro with-write-lock ((lock) &body body)
  "Write locks (CCL only)"
  #+ccl `(ccl:with-write-lock (,lock) ,@body))
```

### 5. `clos.lisp` (88 lines)

**Purpose:** Extensions to Lisp's CLOS system to support persistence.

**Content:**

```lisp
(defvar *meta-slots*
  '(id %type-id %revision %deleted-p ... %data %bytes))

(defclass graph-class (standard-class)
  ;; Custom metaclass for graphs
  ())

(defmethod validate-superclass ((class graph-class) (super standard-class))
  "graph-class classes can inherit from normal classes"
  t)

(defclass graph-slot-definition (standard-slot-definition)
  ;; Custom slot definition
  ())

(defmethod slot-value-using-class :around 
    ((class graph-class) instance slot)
  ;; Intercept slot access
  ;; If meta-slot: normal access
  ;; If data-slot: look up in data alist
  ...)

(defmethod (setf slot-value-using-class) :around 
    ((class graph-class) instance slot)
  ;; Intercept slot assignment
  ;; Mark node as modified
  ...)
```

**Why is this necessary?**

VivaceGraph uses **flexible persistence**: nodes can have different schemas, and slots are stored as associations (key → value) instead of as structure fields.

**Example:**

```lisp
(def-vertex my-vertex
  ((name :type string :persistent t)
   (age :type integer :persistent t)
   (email :type string :persistent t)))

;; When you do:
(setf (slot-value v 'name) "Alice")

;; What happens internally:
;; 1. slot-value-using-class is invoked
;; 2. Looks up 'name in the alist (data v)
;; 3. Marks v as modified
;; 4. If there's a transaction: adds v to the update queue
;; 5. If not: saves v to disk
```

### 6. `uuid.lisp` (121 lines)

**Purpose:** Operations with UUIDs (Universally Unique Identifiers).

**Content:**

#### 6.1 Predicates and Conversions

```lisp
(defgeneric uuid? (thing)
  (:method ((thing uuid)) t)
  (:method (thing) nil))

(defgeneric uuid-eql (uuid1 uuid2)
  "Compare whether two UUIDs are equal"
  (:method ((uuid1 uuid) (uuid2 uuid))
    (equalp (uuid-to-byte-array uuid1) 
            (uuid-to-byte-array uuid2))))
```

#### 6.2 UUID ↔ Byte Array Conversion

```lisp
(defun uuid-to-byte-array (uuid &optional type-specifier)
  "Convert UUID to 16-byte array (or 18 with type-specifier)"
  ;; If type-specifier: returns [type-byte | len-byte | 16 bytes UUID]
  ;; Without type-specifier: returns [16 bytes UUID]
  ...)

(defun mmap-array-to-uuid (mfp offset)
  "Convert bytes in memory-mapped file to UUID"
  (make-instance 'uuid
    :time-low (mmap-array-to-bytes offset (+ 3 offset) mfp)
    :time-mid (mmap-array-to-bytes (+ 4 offset) (+ 5 offset) mfp)
    ...))
```

#### 6.3 FFI Operations with Memory-Mapped Files

```lisp
(defun set-byte (mfp offset byte)
  "Write byte at memory-mapped file position"
  (cffi:mem-aref mfp :unsigned-char offset) = byte)

(defun get-byte (mfp offset)
  "Read byte from memory-mapped file"
  (cffi:mem-aref mfp :unsigned-char offset))

(defmacro mmap-array-to-bytes (from to mfp)
  "Helper to convert bytes to integer"
  `(loop for i from ,from to ,to
     with res = 0
     do (setf (ldb (byte 8 (* 8 (- ,to i))) res) 
              (get-byte ,mfp i))
     finally (return res)))
```

**Why FFI (Foreign Function Interface)?** Because accessing memory-mapped files from Lisp requires CFFI for direct byte read/write.

---

### 7. `random.lisp` (254 lines)

**Purpose:** Random number generator (Mersenne Twister).

**Content:**

```lisp
;; Implementation of the Mersenne Twister algorithm
;; Pseudo-random generator by Matsumoto & Nishimura

(defconstant *mt-n* 624)
(defconstant *mt-m* 397)

(defstruct (mt-random-state ...)
  mti    ; index
  arr)   ; array of 624 numbers

(defun mt-make-random-state (&optional state)
  "Create new random generator"
  ...)

(defun mt-genrand ()
  "Generate next random number (32-bit)"
  ...)

(defun mt-random (n &optional state)
  "Generate random number 0 ≤ x < n"
  (if (integerp n)
      (mod (... use mt-genrand) n)  ; for integers
      (* (mt-genrand) ...) ; for floats
```

**Why Mersenne Twister?**
- Very long period (2^19937 - 1)
- Uniform distribution in high dimensionality
- Fast (bit-level operations only)
- Standard in simulations

**Note:** This file is quite independent. It could be used for any randomness purpose.

### 8. `stats.lisp` (77 lines)

**Purpose:** Collect and report graph statistics.

**Content:**

```lisp
(defun graph-writes-report (graph)
  "Report writes per second (last N seconds)"
  (let ((report nil))
    (maphash (lambda (time writes)
               (push (cons time writes) report))
             (write-stats graph))
    (sort report '< :key 'car)))

(defun graph-writes-report-last-minute (graph)
  "Average writes per second in the last minute"
  ...)

(defun graph-reads-report (graph)
  "Report reads per second"
  ...)

(defun graph-rw-report (&key (graph *graph*))
  "Combined read + write report"
  ...)

(defun graph-stats (&key (graph *graph*) detail-p)
  "Complete statistics report"
  (let ((report
          `((:free-memory . ,(free-memory))
            (:avg-writes-per-second . ,(graph-writes-report-last-minute graph))
            (:avg-reads-per-second . ,(graph-reads-report-last-minute graph))
            (:cache-size . ,(hash-table-count (cache graph)))
            (:vertex-count . ,(read-lhash-count (vertex-table graph)))
            (:edge-count . ,(read-lhash-count (edge-table graph)))
            (:buffer-pool . ,(dump-buffer-pool-stats)))))
    ...))

(defun record-graph-write ()
  "Record a write (called internally)"
  (incf (gethash (get-universal-time) 
                 (write-stats *graph*) 0)))

(defun record-graph-read ()
  "Record a read (called internally)"
  (incf (gethash (get-universal-time) 
                 (read-stats *graph*) 0)))
```

**Data Structure:**

```
write-stats (hash table per second):
  unix-time → number-of-writes
  
Example:
  1234567890 → 150
  1234567891 → 148
  1234567892 → 152

When you call graph-stats:
  ├─ free-memory: available bytes
  ├─ avg-writes-per-second: last minute average
  ├─ avg-reads-per-second: last minute average
  ├─ cache-size: cache entries
  ├─ vertex-count: total vertices
  ├─ edge-count: total edges
  └─ buffer-pool: buffer pool status
```

### 9. `graph-class.lisp` (84 lines)

**Purpose:** Defines the root `GRAPH` class and its metaclass.

**Content:**

```lisp
(defvar *graphs*
  #+sbcl (make-hash-table :test 'equal :synchronized t)
  #+lispworks (make-hash-table :test 'equal :single-thread nil)
  #+ccl (make-hash-table :test 'equal :shared t))

(defclass graph ()
  ((graph-name :accessor graph-name :initarg :graph-name)
   (graph-open-p :accessor graph-open-p :initform nil)
   (location :accessor location :initarg :location)
   (txn-log :accessor txn-log :initarg :txn-log)
   (txn-file :accessor txn-file :initarg :txn-file)
   (txn-lock :accessor txn-lock :initform (make-recursive-lock))
   (transaction-manager :accessor transaction-manager :initarg :transaction-manager)
   (replication-key :accessor replication-key :initarg :replication-key)
   (replication-port :accessor replication-port :initarg :replication-port)
   (vertex-table :accessor vertex-table :initarg :vertex-table)
   (edge-table :accessor edge-table :initarg :edge-table)
   (heap :accessor heap :initarg :heap)
   (indexes :accessor indexes :initarg :indexes)
   (schema :accessor schema :initarg :schema)
   (cache :accessor cache :initarg :cache)
   (ve-index-in :accessor ve-index-in :initarg :ve-index-in)
   (ve-index-out :accessor ve-index-out :initarg :ve-index-out)
   (vev-index :accessor vev-index :initarg :vev-index)
   (vertex-index :accessor vertex-index :initarg :vertex-index)
   (edge-index :accessor edge-index :initarg :edge-index)
   (views-lock :accessor views-lock :initform (make-recursive-lock))
   (views :accessor views :initarg :views)
   (write-stats :accessor write-stats :initform (...))
   (read-stats :accessor read-stats :initform (...))))

(defclass master-graph (graph)
  ((replication-mbox :accessor replication-mbox)
   (replication-listener :accessor replication-listener)
   (stop-replication-p :accessor stop-replication-p :initform nil)
   (slaves :accessor slaves :initform ())
   (slaves-lock :accessor slaves-lock :initform (make-recursive-lock))))

(defclass slave-graph (graph)
  ((master-host :accessor master-host)
   (slave-socket :accessor slave-socket)
   (stop-replication-p :accessor stop-replication-p :initform nil)
   (slave-thread :accessor slave-thread :initform nil)
   (master-txn-id :accessor master-txn-id)))
```

**Main Slots:**

| Slot | Type | Purpose |
|------|------|---------|
| `graph-name` | string | Unique graph name |
| `location` | path | Storage directory |
| `vertex-table` | linear-hash | Vertex table |
| `edge-table` | linear-hash | Edge table |
| `heap` | allocator | Persistent memory |
| `indexes` | alist | Custom indexes |
| `schema` | hash-table | Defined node types |
| `cache` | hash-table | RAM cache |
| `ve-index-in/out` | ve-index | Edge indexes (inbound/outbound) |
| `vev-index` | vev-index | Vertex-to-vertex connectivity indexes |
| `views` | hash-table | Views (map-reduce) |
| `write-stats` | hash-table | Write statistics |
| `read-stats` | hash-table | Read statistics |

**Class Hierarchy:**

```
GRAPH (base class)
├─ MASTER-GRAPH (for replication)
│  └ (can have multiple slaves)
└─ SLAVE-GRAPH (for replication)
   └ (connected to a master)
```

**Defined Generic Methods:**

```lisp
(defgeneric graph-p (thing)
  (:method ((graph graph)) graph)
  (:method (thing) nil))

(defgeneric master-graph-p (thing) ...)
(defgeneric slave-graph-p (thing) ...)
(defgeneric init-schema (graph))
(defgeneric update-schema (graph-or-name))
(defgeneric snapshot (graph &key))
(defgeneric scan-for-unindexed-nodes (graph))
(defgeneric start-replication (graph &key package))
(defgeneric stop-replication (graph))

(defun lookup-graph (name)
  "Look up graph by name in *graphs* hash table"
  (gethash name *graphs*))
```

### 10. `node-class.lisp` (174 lines)

**Purpose:** Defines the `NODE-CLASS` metaclass that extends CLOS for persistence.

**Content:**

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass node-class (standard-class) nil)

(defmethod validate-superclass ((class node-class) (super standard-class))
  "node-class classes can inherit from standard classes"
  t)

(defclass node-slot-definition (standard-slot-definition)
  ((persistent :accessor persistent-p :initarg :persistent :initform t)
   (indexed :accessor indexed-p :initarg :index :initform nil)
   (ephemeral :accessor ephemeral-p :initarg :ephemeral :initform nil)
   (meta :accessor meta-p :initarg :meta :initform nil)))
```

**Slot Attributes:**

| Attribute | Meaning | Persisted to Disk |
|-----------|---------|-------------------|
| `:persistent t` | Persistent slot | ✓ Yes |
| `:persistent nil` | Ephemeral slot | ✗ No |
| `:ephemeral t` | Memory-only slot | ✗ No |
| `:meta t` | Metadata slot | ✗ No (special access) |
| `:index t` | Create index on this slot | ✓ Yes (automatic) |

**Example:**

```lisp
(def-vertex person
  ((name :type string :persistent t)        ; Persisted
   (age :type integer :persistent t)        ; Persisted
   (cached-data :ephemeral t)               ; RAM only
   (id :meta t)                             ; Metadata
   (email :persistent t :index t)))         ; Persisted + Indexed
```

**Key Methods:**

```lisp
(defmethod data-slots ((instance node-class))
  "Returns list of managed slots (persistent + ephemeral)"
  (map 'list 'slot-definition-name
       (remove-if-not #'(lambda (i)
                          (or (persistent-p i) (ephemeral-p i)))
                      (class-slots instance))))

(defmethod meta-slot-names ((instance node-class))
  "Returns list of metadata slots (:meta t)"
  ...)

(defmethod persistent-slot-names ((instance node-class))
  "Returns list of persistent slots"
  ...)

(defmethod ephemeral-slot-names ((instance node-class))
  "Returns list of ephemeral slots"
  ...)

(defmethod find-all-subclasses ((class class))
  "Recursively finds all subclasses"
  ...)

(defmethod find-ancestor-classes ((class node-class))
  "Finds relevant parent classes (excluding STANDARD-OBJECT, T, etc.)"
  ...)

(defmethod find-graph-parent-classes ((class node-class))
  "Finds graph parent classes (excluding VERTEX, EDGE, PRIMITIVE-NODE)"
  ...)
```

**BASE NODE Class:**

```lisp
(defclass node ()
  ((id :accessor id :initform +null-key+ :initarg :id :meta t)
   (type-id :accessor type-id :initform 1 :initarg :type-id :meta t)
   (revision :accessor revision :initform 0 :initarg :revision :meta t)
   (%revision-table :accessor %revision-table :initform (make-hash-table) :meta t)
   (heap-written-p :accessor heap-written-p :initform nil :meta t)
   (type-idx-written-p :accessor type-idx-written-p :initform nil :meta t)
   (ve-written-p :accessor ve-written-p :initform nil :meta t)
   (vev-written-p :accessor vev-written-p :initform nil :meta t)
   (views-written-p :accessor views-written-p :initform nil :meta t)
   (written-p :accessor written-p :initform nil :meta t)
   (data-pointer :accessor data-pointer :initform 0 :meta t)
   (deleted-p :accessor deleted-p :initform nil :meta t)
   (data :accessor data :initform nil :meta t)
   (bytes :accessor bytes :initform :init :meta t))
  (:metaclass node-class))
```

**NODE Slots:**

| Slot | Purpose |
|------|---------|
| `id` | Unique identifier (16 bytes) |
| `type-id` | Node type (for polymorphism) |
| `revision` | Node version |
| `%revision-table` | Revision table for conflict detection |
| `heap-written-p` | Already written to heap? |
| `type-idx-written-p` | Already written to type-index? |
| `ve-written-p` | Already written to ve-index? |
| `vev-written-p` | Already written to vev-index? |
| `views-written-p` | Already written to views? |
| `written-p` | Already written in general? |
| `data-pointer` | Offset in data file |
| `deleted-p` | Marked as deleted? |
| `data` | Alist of custom (key . value) pairs |
| `bytes` | Byte buffer (for serialization) |

## Key Global Constants

### Data Sizes

```lisp
+key-bytes+          = 16    ; ID size (UUID)
+value-bytes+        = 8     ; Hash value size
+bucket-size+        = 24    ; Bucket size
+data-extent-size+   = 100MB ; Data block size

+index-list-bytes+   = 17    ; List index key size
+ve-key-bytes+       = 18    ; ve-index key size
+vev-key-bytes+      = 34    ; vev-index key size
```

### Magic Bytes (Type Identifiers)

```lisp
+data-magic-byte+     = 0x17  ; Data marker
+lhash-magic-byte+    = 0x18  ; Persistent hash marker
+overflow-magic-byte+ = 0x19  ; Overflow marker
+config-magic-byte+   = 0x20  ; Configuration marker
```

### Index Sentinels

```lisp
+min-sentinel+  = :gmin      ; Minimum value in skip lists
+max-sentinel+  = :gmax      ; Maximum value in skip lists
+reduce-master+ = :gagg      ; Aggregated key in views
```

### Serialization Types

```lisp
+unknown+     = 0
+integer+     = 1-2     ; negative/positive
+character+   = 3
+symbol+      = 4
+string+      = 5
+list+        = 6
+vector+      = 7
+float+       = 8-9     ; single/double
+uuid+        = 100
+timestamp+   = 101
; ... and many more
```

## Load Order

**CRITICAL:** The order in `graph-db.asd` is exact:

```
1. package.lisp .............. Create package
2. globals.lisp .............. Global variables and constants
3. conditions.lisp ........... Exceptions
4. utilities.lisp ............ Utility functions
5. clos.lisp ................. CLOS extensions
6. uuid.lisp ................. UUID operations
7. random.lisp ............... Mersenne Twister
8. stats.lisp ................ Statistics
9. graph-class.lisp .......... GRAPH class
10. node-class.lisp .......... NODE-CLASS metaclass
```

**Why this order?**

```
package.lisp FIRST
    ↓ (defines the :graph-db package)
globals.lisp
    ↓ (constants and vars used by everything)
conditions.lisp
    ↓ (exceptions for signal/handle)
utilities.lisp
    ↓ (basic functions reused below)
clos.lisp
    ↓ (extends CLOS)
uuid.lisp + random.lisp
    ↓ (specialized utilities)
stats.lisp
    ↓ (statistics)
graph-class.lisp + node-class.lisp
    ↓ (root classes everything depends on)
```

If you change the order, you will get **undefined symbol errors**.

## Design Patterns

### 1. Conditional Compilation (Feature Flags)

```lisp
#+sbcl     ; SBCL only
#+lispworks ; LispWorks only
#+ccl      ; Clozure CL only
#-sbcl     ; Except in SBCL
```

**Example:**

```lisp
(defun free-memory ()
  #+sbcl
  (- (sb-kernel::dynamic-space-size) (sb-kernel:dynamic-usage))
  #+ccl
  (ccl::%freebytes)
  #+lispworks
  (mp:get-heap-size))
```

### 2. Thread-Safe Variables

```lisp
#+sbcl
(defvar *hash* (make-hash-table :synchronized t))

#+lispworks
(defvar *hash* (make-hash-table :single-thread nil))

#+ccl
(defvar *hash* (make-hash-table :shared t))
```

### 3. Custom Metaclasses

```lisp
(defclass my-class ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2))
  (:metaclass node-class))  ; Use custom metaclass
```

Metaclasses allow **intercepting** instance creation, slot access, etc.

### 4. Multi-Method Generic Methods

```lisp
(defgeneric less-than (x y)
  (:method ((x number) (y number)) (< x y))
  (:method ((x string) (y string)) (string< x y))
  (:method ((x symbol) (y symbol)) 
    (string< (symbol-name x) (symbol-name y)))
  (:method ((x (eql +min-sentinel+)) y) nil)
  (:method ((x (eql +max-sentinel+)) y) t))
```

The correct method is chosen **based on runtime types**.


## Summary

**Layer 1** provides:

1. ✓ **Package and namespace** (`package.lisp`)
2. ✓ **Global state** (`globals.lisp`)
3. ✓ **Error handling** (`conditions.lisp`)
4. ✓ **Utilities** (`utilities.lisp`)
5. ✓ **CLOS extensions** (`clos.lisp`)
6. ✓ **UUIDs and IDs** (`uuid.lisp`)
7. ✓ **Randomness** (`random.lisp`)
8. ✓ **Statistics** (`stats.lisp`)
9. ✓ **Root GRAPH class** (`graph-class.lisp`)
10. ✓ **NODE-CLASS metaclass** (`node-class.lisp`)

**Total:** ~1,685 lines of fundamental code that **does NOT depend on anything internal**.

In the following layers, everything will depend on these foundations.

*VivaceGraph Layer 1 Documentation*
*March 2026*
