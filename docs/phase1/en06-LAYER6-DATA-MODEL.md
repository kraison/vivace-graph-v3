# VivaceGraph - Layer 6: Data Model

## Table of Contents

1. [Overview](#overview)
2. [Purpose and Responsibilities](#purpose-and-responsibilities)
3. [Layer 6 Components](#layer-6-components)
4. [Detailed Files](#detailed-files)
5. [Class Hierarchy](#class-hierarchy)
6. [Dynamic Type System](#dynamic-type-system)
7. [CRUD Operations](#crud-operations)
8. [Node Serialization](#node-serialization)
9. [Load Order](#load-order)

## Overview

**Layer 6** defines the **concrete data structures** that VivaceGraph manipulates:

- **Primitive nodes:** Base, flags, serialization
- **Vertices:** Nodes without connectivity
- **Edges:** Directed connections with weight
- **Type system:** Dynamic class extension
- **CRUD operations:** Create, Read, Update, Delete

### Key Features

- ✓ **CLOS hierarchy:** Vertex/Edge inherit from Node
- ✓ **Dynamic types:** Define new types at runtime
- ✓ **Persistent/ephemeral slots:** Fine-grained persistence control
- ✓ **Automatic serialization:** Objects ↔ bytes
- ✓ **Lazy loading:** Data deserialized on demand
- ✓ **Versioning:** Each node has a revision

### Lines of Code

```
File                       Lines
──────────────────────────────────
primitive-node.lisp         319
vertex.lisp                 194
edge.lisp                   427
schema.lisp                 411
──────────────────────────────────
TOTAL                      1351 lines
```

## Purpose and Responsibilities

### Why does Layer 6 exist?

VivaceGraph needs to:
1. **Represent nodes and edges** as CLOS objects
2. **Automatic persistence** without manual code
3. **Type extension** without recompilation
4. **Fast access** via hash tables
5. **Versioning and concurrency** with revisions

### Specific Responsibilities

| Responsibility | File | Reason |
|----------------|------|--------|
| Primitive base node | `primitive-node.lisp` | Serialization, flags, base CRUD |
| Vertices | `vertex.lisp` | Nodes without outgoing edges |
| Edges | `edge.lisp` | Connections with from/to/weight |
| Dynamic schema | `schema.lisp` | Define new Vertex/Edge types |

## Layer 6 Components

### Dependency Diagram

```
LAYER 6 - INTERNAL DEPENDENCIES
================================

From LAYER 5 (ve-index, vev-index, type-index):
    ↓
primitive-node.lisp ........ Base Node class
    ├─ Depends: Layer 4 (allocator, serialize)
    ├─ Uses: CLOS, metaclasses
    └─ Defines: node, flags, base CRUD
         ↓
    vertex.lisp ............. Vertex class
         ├─ Depends: primitive-node
         └─ Defines: vertex, make-vertex, map-vertices
              ↓
    edge.lisp ............... Edge class
         ├─ Depends: primitive-node
         ├─ Uses: ve-index, vev-index (Layer 5)
         └─ Defines: edge, make-edge, outgoing/incoming
              ↓
    schema.lisp ............. Type system
         ├─ Depends: vertex, edge, prologc (Layer 5)
         └─ Defines: node-type, def-vertex, def-edge
```

## Detailed Files

### 1. `primitive-node.lisp` (319 lines) - **KEY FILE**

**Purpose:** Base `Node` class with serialization, flags, and CRUD operations.

#### 1.1 Node Header (Metadata)

```
NODE HEADER IN MEMORY (15 bytes):
═══════════════════════════════════════════════════════════

Byte 0:     FLAGS (1 byte, 7 flags)
            ├─ bit 0: deleted-p (deleted?)
            ├─ bit 1: written-p (written to disk?)
            ├─ bit 2: heap-written-p (data in heap?)
            ├─ bit 3: type-idx-written-p (in type-index?)
            ├─ bit 4: views-written-p (views updated?)
            ├─ bit 5: ve-written-p (in VE-index?) [edges only]
            └─ bit 6: vev-written-p (in VEV-index?) [edges only]

Bytes 1-2:  TYPE-ID (2 bytes, big-endian)
            ├─ 0 = Generic (vertex/edge)
            └─ 1-65535 = Custom type ID

Bytes 3-6:  REVISION (4 bytes, big-endian)
            └─ Version number (MVCC)

Bytes 7-14: DATA-POINTER (8 bytes, big-endian)
            └─ Heap address where data resides
```

#### 1.2 Node Structure

```lisp
(defclass node ()
  ((id :initarg :id 
       :type (simple-array (unsigned-byte 8) (16))
       :meta t)
   
   (%type-id :initarg :type-id
             :type (integer 0 65535)
             :meta t)
   
   (%revision :initarg :revision
              :type (integer 0 4294967295)
              :meta t)
   
   ;; FLAGS (metadata, not persistent)
   (%deleted-p :meta t)
   (%written-p :meta t)
   (%heap-written-p :meta t)
   (%type-idx-written-p :meta t)
   (%views-written-p :meta t)
   (%ve-written-p :meta t)        ; edges only
   (%vev-written-p :meta t)       ; edges only
   
   ;; DATA
   (%data-pointer :type word :meta t)  ; Heap address
   (%data :persistent t)                ; Alist or map
   (%bytes :ephemeral t))               ; Serialization cache
  (:metaclass node-class))
```

**Attributes:**
- `:persistent t` - Data stored on disk
- `:ephemeral t` - Data only in memory (not persisted)
- `:meta t` - Metadata (not versioned)

#### 1.3 Primitive Operations

```lisp
(defun serialize-node-head (mf n offset)
  "Serialize node header (15 bytes)"
  ;; 1. Flags (1 byte)
  (let ((flags (flags-as-int n)))
    (set-byte mf offset flags))
  ;; 2. Type-ID (2 bytes)
  (dotimes (i 2)
    (set-byte mf (incf offset) 
             (ldb (byte 8 (* i 8)) (type-id n))))
  ;; 3. Revision (4 bytes)
  (dotimes (i 4)
    (set-byte mf (incf offset)
             (ldb (byte 8 (* i 8)) (revision n))))
  ;; 4. Data-pointer (8 bytes)
  (dotimes (i 8)
    (set-byte mf (incf offset)
             (ldb (byte 8 (* i 8)) (data-pointer n))))
  offset)

(defun deserialize-node-head (mf offset)
  "Deserialize header"
  ;; Returns 7 values: flags (7 bool bytes), type-id, revision, pointer
  (let ((flags (get-byte mf offset)))
    (values
     (ldb-test (byte 1 0) flags)  ;; deleted-p
     (ldb-test (byte 1 1) flags)  ;; written-p
     (ldb-test (byte 1 2) flags)  ;; heap-written-p
     (ldb-test (byte 1 3) flags)  ;; type-idx-written-p
     (ldb-test (byte 1 4) flags)  ;; views-written-p
     (ldb-test (byte 1 5) flags)  ;; ve-written-p
     (ldb-test (byte 1 6) flags)  ;; vev-written-p
     ;; type-id (2 bytes)
     (let ((int 0))
       (dotimes (i 2)
         (setq int (dpb (get-byte mf (incf offset))
                        (byte 8 (* i 8)) int)))
       int)
     ;; revision (4 bytes)
     ...
     ;; data-pointer (8 bytes)
     ...
     offset)))

(defun lookup-node (table key graph)
  "Look up node by ID in hash table"
  ;; 1. Check cache
  (or (and *cache-enabled*
           (gethash key (cache graph)))
      ;; 2. Search in table
      (let ((node (lhash-get table key)))
        (when (node-p node)
          ;; 3. Store in cache
          (setf (gethash key (cache graph)) node)
          (record-graph-read)
          node))))

(defun save-node (node table &key (graph *graph*))
  "Save node to disk"
  ;; 1. Serialize data if present
  (when (plusp (data-pointer node))
    (if (data node)
        (setf (bytes node) (serialize (data node)))
        (maybe-init-node-data node :graph graph))
    ;; 2. Allocate in heap
    (let ((addr (allocate (heap graph) (length (bytes node)))))
      (dotimes (i (length (bytes node)))
        (set-byte (heap graph)
                 (+ i addr) (aref (bytes node) i)))
      (setf (data-pointer node) addr)))
  
  ;; 3. Update in hash table
  (with-locked-hash-key (table (id node))
    (lhash-put table (id node) node)
    ;; 4. Cache
    (setf (gethash (id node) (cache graph)) node)))
```

### 2. `vertex.lisp` (194 lines)

**Purpose:** `Vertex` class - node without implicit outgoing edges.

#### 2.1 Vertex Class

```lisp
(defclass vertex (node)
  ()
  (:metaclass node-class))
```

Vertex inherits EVERYTHING from Node, with no additional fields.

#### 2.2 Operations

```lisp
(defun make-vertex (type-id data &key id deleted-p revision 
                               retry-p (graph *graph*))
  "Create new vertex"
  ;; 1. Resolve type
  (let ((type-meta 
         (or (eq type-id :generic)
             (lookup-node-type-by-name type-id :vertex :graph graph))))
    
    ;; 2. Serialize data
    (let ((bytes (when data (serialize data))))
      
      ;; 3. Create instance
      (let ((v (%make-vertex 
                :id (or id (gen-vertex-id))
                :type-id (if (eq type-meta :generic) 0 (node-type-id type-meta))
                :revision (or revision 0)
                :deleted-p deleted-p
                :bytes bytes
                :data data)))
        
        ;; 4. Change class if custom type
        (when (and type-meta (not (eq type-meta :generic)))
          (change-class v (node-type-name type-meta)))
        
        ;; 5. Save to disk
        (handler-case
            (create-node v graph)
          (duplicate-key-error (c)
            ;; Retry with new ID if requested
            (if retry-p
                (make-vertex type-id data 
                           :id (gen-vertex-id)
                           :revision revision :graph graph)
                (error c))))
        v))))

(defmethod lookup-vertex ((id array) &key (graph *graph*))
  "Look up vertex by ID"
  (lookup-object id (vertex-table graph) *transaction* graph))

(defmethod lookup-vertex ((id string) &key (graph *graph*))
  "Look up vertex by string UUID"
  (lookup-vertex (read-id-array-from-string id) :graph graph))

(defmethod delete-vertex ((vertex vertex) &key (graph *graph*))
  "Logically delete vertex"
  (when (deleted-p vertex)
    (error 'vertex-already-deleted-error :node vertex))
  (delete-node vertex graph))

(defun map-vertices (fn graph &key collect-p vertex-type 
                               include-deleted-p 
                               (include-subclasses-p t))
  "Iterate over vertices (potentially filtered)"
  (let ((result nil))
    (cond
      ;; Filter by type
      ((and vertex-type include-subclasses-p)
       (let ((vertex-class (find-class vertex-type)))
         (let ((all-classes 
                (nconc (list vertex-type)
                      (find-all-subclass-names vertex-class))))
           (dolist (class all-classes)
             (let ((type-meta (lookup-node-type-by-name class :vertex)))
               (when type-meta
                 (let ((index-list 
                       (get-type-index-list (vertex-index graph)
                                           (node-type-id type-meta))))
                   (map-index-list 
                    (lambda (id)
                      (let ((vertex (lookup-vertex id :graph graph)))
                        (when (and (written-p vertex)
                                  (or include-deleted-p
                                      (not (deleted-p vertex))))
                          (if collect-p
                              (push (funcall fn vertex) result)
                              (funcall fn vertex)))))
                    index-list))))))))
      
      ;; No filter: iterate all
      (t
       (map-lhash 
        (lambda (pair)
          (let ((vertex (cdr pair)))
            (when (and (written-p vertex)
                      (or include-deleted-p
                          (not (deleted-p vertex))))
              (setf (id vertex) (car pair))
              (if collect-p
                  (push (funcall fn vertex) result)
                  (funcall fn vertex)))))
        (vertex-table graph))))
    
    (when collect-p (nreverse result))))
```

### 3. `edge.lisp` (427 lines) - **IMPORTANT FILE**

**Purpose:** `Edge` class - directed edges with from/to/weight.

#### 3.1 Edge Structure

```lisp
(defclass edge (node)
  ((from :accessor from 
         :initform +null-key+
         :initarg :from
         :type (simple-array (unsigned-byte 8) (16))
         :persistent nil :ephemeral nil :meta t)
   
   (to :accessor to 
       :initform +null-key+
       :initarg :to
       :type (simple-array (unsigned-byte 8) (16))
       :persistent nil :ephemeral nil :meta t)
   
   (weight :accessor weight 
           :initform 1.0 
           :initarg :weight 
           :type float
           :persistent nil :ephemeral nil :meta t))
  (:metaclass node-class))
```

**Fields:**
- `from` - Source vertex UUID (16 bytes)
- `to` - Destination vertex UUID (16 bytes)
- `weight` - Edge weight (64-bit IEEE float)

#### 3.2 Edge Header Layout (35 bytes)

```
EDGE HEADER:
═══════════════════════════════════════════════════════════

Bytes 0-14:     Node header (15 bytes)
                ├─ Flags, type-id, revision, data-pointer
                
Bytes 15-30:    FROM UUID (16 bytes)
                ├─ Source vertex identifier
                
Bytes 31-46:    TO UUID (16 bytes)
                ├─ Destination vertex identifier
                
Bytes 47-54:    WEIGHT (8 bytes, IEEE 754 double)
                ├─ Edge weight
```

#### 3.3 Edge Operations

```lisp
(defun serialize-edge-head (mf e offset)
  "Serialize edge header (35 bytes)"
  ;; 1. Node header (15 bytes)
  (setq offset (serialize-node-head mf e offset))
  
  ;; 2. FROM UUID (16 bytes)
  (dotimes (i 16)
    (set-byte mf (incf offset) (aref (from e) i)))
  
  ;; 3. TO UUID (16 bytes)
  (dotimes (i 16)
    (set-byte mf (incf offset) (aref (to e) i)))
  
  ;; 4. WEIGHT (8 bytes, IEEE 754)
  (let ((int (ieee-floats:encode-float64 (weight e))))
    (dotimes (i 8)
      (set-byte mf (incf offset) (ldb (byte 8 0) int))
      (setq int (ash int -8))))
  offset)

(defun make-edge (from to type-id &key data id deleted-p 
                                    revision (graph *graph*))
  "Create new directed edge from → to"
  ;; 1. Resolve type
  (let ((type-meta (lookup-node-type-by-name type-id :edge :graph graph)))
    (when (or (null from) (null to))
      (error "from and to are required"))
    
    ;; 2. Serialize data
    (let ((bytes (when data (serialize data))))
      
      ;; 3. Create edge
      (let ((e (%make-edge
                :id (or id (gen-edge-id))
                :from (if (node-p from) (id from) from)
                :to (if (node-p to) (id to) to)
                :type-id (if type-meta (node-type-id type-meta) 0)
                :revision (or revision 0)
                :deleted-p deleted-p
                :bytes bytes
                :data data)))
        
        ;; 4. Save
        (create-node e graph)
        
        ;; 5. Index
        (add-to-ve-index e graph)
        (add-to-vev-index e graph)
        (add-to-type-index e graph)
        
        e))))

(defmethod outgoing-edges ((v vertex) &key (graph *graph*) 
                                          edge-type)
  "Get all outgoing edges of vertex"
  (map-edges 'identity graph 
            :vertex v 
            :edge-type edge-type
            :direction :out
            :collect-p t))

(defmethod incoming-edges ((v vertex) &key (graph *graph*) 
                                          edge-type)
  "Get all incoming edges"
  (map-edges 'identity graph
            :vertex v
            :edge-type edge-type
            :direction :in
            :collect-p t))

(defmethod map-edges (fn graph &key collect-p edge-type vertex 
                                   direction 
                                   (include-deleted-p nil))
  "Iterate over edges with multiple filters"
  (let ((result nil))
    (cond
      ;; Filter: edge-type + direction
      ((and edge-type vertex direction)
       (let* ((type-meta (lookup-node-type-by-name edge-type :edge))
              (ve-key (make-ve-key 
                      :id (id vertex) 
                      :type-id (node-type-id type-meta)))
              (index-list 
               (if (eq direction :out)
                   (lookup-ve-out-index-list ve-key graph)
                   (lookup-ve-in-index-list ve-key graph))))
         (when index-list
           (map-index-list
            (lambda (edge-id)
              (let ((edge (lookup-edge edge-id :graph graph)))
                (when (and (written-p edge)
                          (or include-deleted-p (active-edge-p edge)))
                  (if collect-p
                      (push (funcall fn edge) result)
                      (funcall fn edge)))))
            index-list))))
      
      ;; Filter: edge-type only
      ((and edge-type (not vertex))
       (let ((type-meta (lookup-node-type-by-name edge-type :edge)))
         (let ((index-list 
               (get-type-index-list (edge-index graph)
                                   (node-type-id type-meta))))
           (map-index-list 
            (lambda (id)
              (let ((edge (lookup-edge id :graph graph)))
                (when (and (written-p edge)
                          (or include-deleted-p (active-edge-p edge)))
                  (if collect-p
                      (push (funcall fn edge) result)
                      (funcall fn edge)))))
            index-list))))
      
      ;; No filters: all
      (t
       (map-lhash
        (lambda (pair)
          (let ((edge (cdr pair)))
            (when (and (written-p edge)
                      (or include-deleted-p (active-edge-p edge)))
              (setf (id edge) (car pair))
              (if collect-p
                  (push (funcall fn edge) result)
                  (funcall fn edge)))))
        (edge-table graph))))
    
    (when collect-p (nreverse result))))
```

### 4. `schema.lisp` (411 lines) - **TYPE SYSTEM**

**Purpose:** Dynamic type system - define new classes at runtime.

#### 4.1 Schema Structure

```lisp
(defstruct schema
  (lock (make-recursive-lock))
  
  ;; Type table: { :vertex → { type-id → node-type, ... }, 
  ;;               :edge → { type-id → node-type, ... } }
  (type-table (make-hash-table :test 'eql :synchronized t))
  
  ;; Per-class locks for concurrent updates
  (class-locks (make-hash-table :test 'eql :synchronized t))
  
  ;; Counters for generating new IDs
  (next-edge-id 1 :type (unsigned-byte 16))
  (next-vertex-id 1 :type (unsigned-byte 16)))

(defstruct node-type
  name                    ; Class name (symbol)
  parent-type             ; Parent class
  id                      ; Type ID (0-65535)
  graph-name              ; Graph where it's defined
  slots                   ; Slot specs
  package                 ; Package for the class
  constructor)            ; Function to create instances
```

#### 4.2 Definition Macros

```lisp
(defmacro def-vertex (name parent-types slot-specs graph-name)
  "Define new vertex type"
  `(def-node-type ,name (,@parent-types vertex) 
                  ,slot-specs ,graph-name))

(defmacro def-edge (name parent-types slot-specs graph-name)
  "Define new edge type"
  `(def-node-type ,name (,@parent-types edge) 
                  ,slot-specs ,graph-name))

(defmacro def-node-type (name parent-types slot-specs graph-name)
  "Main macro for defining types"
  `(let ((meta (make-node-type 
               :name ',name
               :parent-type ',(first parent-types)
               :id (get-next-type-id (schema (lookup-graph ',graph-name))
                                    ',(if (find 'edge parent-types)
                                          :edge :vertex))
               :graph-name ',graph-name
               :slots ',slot-specs)))
     
     ;; 1. Define CLOS class
     (defclass ,name (,@parent-types)
       ,(mapcar (lambda (spec)
                  (let ((name (car spec)))
                    `(,name :initarg ,(intern (symbol-name name) :keyword)
                           :accessor ,name
                           :allocation :instance)))
               slot-specs)
       (:metaclass node-class))
     
     ;; 2. Register in schema
     (setf (gethash (node-type-id meta)
                   (gethash ,(if (find 'edge parent-types) :edge :vertex)
                           (schema-type-table (schema (lookup-graph ',graph-name)))))
          meta)
     
     ;; 3. Automatically generate Prolog predicates
     ;; For queries like: ?- user(Name, Email).
     (def-global-prolog-functor ,(intern (format nil "~A/~A" name 
                                                 (length slot-specs)))
       ,params
       ...)
     
     meta))
```

#### 4.3 Definition Example

```lisp
;; Define "User" vertex type
(def-vertex user (user)
  ((name :persistent t :type string)
   (email :persistent t :type string)
   (age :persistent t :type integer))
  my-graph)

;; This generates:
;; - CLOS class: user
;; - Type ID: (e.g.) 1
;; - Prolog predicate: user/3 for queries
;; - Slots: name, email, age (persistent)

;; Define "follows" edge type
(def-edge follows (follows)
  ((strength :persistent t :type float))
  my-graph)

;; Create instances:
(let ((alice (make-vertex 'user 
                         '((:name "Alice") (:email "alice@example.com")))))
  (let ((bob (make-vertex 'user 
                         '((:name "Bob") (:email "bob@example.com")))))
    (let ((edge (make-edge alice bob 'follows 
                          :data '((:strength 0.9)))))
      edge)))
```

#### 4.4 Schema Operations

```lisp
(defmethod init-schema ((graph graph))
  "Initialize empty schema for graph"
  (let ((schema (make-schema)))
    (setf (schema graph) schema)
    
    ;; Create type tables for vertex and edge
    (setf (gethash :edge (schema-type-table schema))
          (make-hash-table :test 'eql :synchronized t))
    (setf (gethash :vertex (schema-type-table schema))
          (make-hash-table :test 'eql :synchronized t))
    
    ;; Create base locks
    (setf (gethash 'edge (schema-class-locks schema))
          (make-rw-lock))
    (setf (gethash 'vertex (schema-class-locks schema))
          (make-rw-lock))
    
    schema))

(defmethod save-schema ((schema schema) (graph graph))
  "Persist schema to disk"
  (with-recursive-lock-held ((schema-lock schema))
    (let ((schema-file (format nil "~A/schema.dat" (location graph))))
      ;; Save with cl-store (Lisp serialization)
      (cl-store:store schema schema-file))))

(defmethod schema-digest ((schema schema))
  "Get MD5 hash of schema (for replication)"
  (map nil 
       (lambda (octet)
         (format t "~(~2,'0X~)'" octet))
       (md5:md5sum-string (schema-string-representation schema)
                         :external-format :utf8)))

(defun list-vertex-types (&optional (graph *graph*))
  "Get IDs of all vertex types"
  (nconc (list 0)  ; Generic vertex type
        (loop for key being the hash-keys
              in (gethash :vertex (schema-type-table (schema graph)))
              if (numberp key) collecting key)))

(defun list-edge-types (&optional (graph *graph*))
  "Get IDs of all edge types"
  (nconc (list 0)  ; Generic edge type
        (loop for key being the hash-keys
              in (gethash :edge (schema-type-table (schema graph)))
              if (numberp key) collecting key)))
```

## Class Hierarchy

```
CLOS INHERITANCE
═════════════════════════════════════════════════════════════

Object (Lisp)
    │
    ├─ Node (node-class metaclass)
    │   │   [Base class for all nodes]
    │   │   Slots: id, type-id, revision, deleted-p, 
    │   │          written-p, data-pointer, data, bytes
    │   │
    │   ├─ Vertex
    │   │   [No additional fields]
    │   │   Example: generic vertex
    │   │
    │   │   ├─ User [custom type]
    │   │   │   [name, email, age]
    │   │   │
    │   │   ├─ Post [custom type]
    │   │   │   [title, content, published-at]
    │   │   │
    │   │   └─ ...other types...
    │   │
    │   └─ Edge
    │       [from, to, weight]
    │       Example: generic edge
    │
    │       ├─ Follows [custom type]
    │       │   [strength]
    │       │
    │       ├─ Likes [custom type]
    │       │   [rating]
    │       │
    │       └─ ...other types...
    │
    └─ ... other CLOS classes ...
```

## Dynamic Type System

### Lifecycle

```
1. DEFINITION (at load time or runtime):
   ─────────────────────────────────────────────
   (def-vertex user (user)
     ((name :persistent t)
      (email :persistent t)))
   
   → Creates node-type structure
   → Registers in schema
   → Defines CLOS class
   → Generates Prolog predicates

2. LOOKUP (when creating instance):
   ─────────────────────────────────────
   (make-vertex 'user data)
   
   → lookup-node-type-by-name 'user
   → Gets node-type meta
   → change-class v user
   → Proceeds as user instance

3. QUERY (Prolog queries):
   ──────────────────────────
   ?- user(Name, Email)
   
   → Searches for user/2 predicate
   → Iterates over type-index
   → Unifies with variables
```

### Slot Fields

```
(def-vertex person ()
  ((name :persistent t :type string)
   (age :persistent t :type integer)
   (temp-cache :ephemeral t)
   (readonly :readonly t)))

:persistent t     → Store on disk
:persistent nil   → Memory only, not persisted
:ephemeral t      → RAM only, never serialize
:readonly t       → Does not allow setf
:type T           → Validation (opt)
```

## CRUD Operations

### CREATE

```lisp
;; Create generic vertex
(let ((v (make-vertex :generic '((:name "Alice")))))
  v)

;; Create typed vertex
(let ((u (make-vertex 'user '((:name "Bob") (:email "bob@ex.com")))))
  u)

;; Create edge
(let ((e (make-edge v1 v2 'follows :data '((:weight 0.8)))))
  e)
```

### READ

```lisp
;; Lookup vertex by ID
(lookup-vertex vertex-id :graph graph)

;; Lookup edge by ID
(lookup-edge edge-id :graph graph)

;; Slot access
(slot-value user 'name)
(node-slot-value user :email)

;; Lazy data loading
(maybe-init-node-data node :graph graph)
```

### UPDATE

```lisp
;; Modify slot
(setf (slot-value user 'name) "Charlie")

;; Save changes
(save-node user (vertex-table graph) :graph graph)

;; Inside transaction: automatic changes
(with-transaction (graph)
  (setf (slot-value user 'name) "Dave"))
```

### DELETE

```lisp
;; Logical deletion
(delete-vertex v :graph graph)
(delete-edge e :graph graph)

;; Check if deleted
(deleted-p node)

;; Physical deletion (compaction)
(compact-vertices graph)
(compact-edges graph)
```

## Node Serialization

### Process

```
SAVING NODE
═════════════════════════════════════════════════════════════

1. Serialize data (alist → bytes)
   ────────────────────────────────
   data = (:name "Alice" :age 30)
         ↓
   bytes = [type-tag, length, bytes...]
   
2. Allocate in heap
   ────────────────
   addr = allocate(heap, length(bytes))
   
3. Write to heap
   ───────────────
   for i in 0..length(bytes):
     set-byte(heap, addr+i, bytes[i])
   
4. Update data-pointer
   ───────────────────────
   node.data-pointer = addr
   
5. Serialize header
   ──────────────────
   header = [flags, type-id, revision, addr]
   write-to-table(node.id, header)
   
6. Cache
   ──────
   cache[node.id] = node


LOADING NODE
═════════════════════════════════════════════════════════════

1. Check cache
   ────────────
   node = cache.get(id)
   if found: return node
   
2. Deserialize header
   ───────────────────
   (flags, type-id, revision, ptr) = read-header(table, id)
   
3. Create instance
   ───────────────
   node = new Node(id, type-id, revision, ptr)
   
4. Lazy load data
   ────────────
   upon access:
     bytes = read-bytes(heap, ptr)
     data = deserialize(bytes)
     node.data = data
     node.bytes = bytes (cache)
   
5. Cache
   ──────
   cache[id] = node
```

## Load Order

```
LOAD ORDER - LAYER 6
═══════════════════════════════════════════════════════════

From LAYER 5 (ve-index, vev-index, type-index):
    ↓
primitive-node.lisp ........ Base Node class
    ├─ Depends: Layer 4 (allocator, serialize)
    ├─ Uses: CLOS, metaclasses
    └─ Defines: node, flags, base CRUD
         ↓
    vertex.lisp ............. Vertex class
         ├─ Depends: primitive-node
         └─ Defines: vertex, make-vertex, map-vertices
              ↓
    edge.lisp ............... Edge class
         ├─ Depends: primitive-node
         ├─ Uses: ve-index, vev-index (Layer 5)
         └─ Defines: edge, make-edge, outgoing/incoming
              ↓
    schema.lisp ............. Type system
         ├─ Depends: vertex, edge, prologc (Layer 5)
         └─ Defines: node-type, def-vertex, def-edge
```

## Summary

**Layer 6** provides:

1. ✓ **Primitive Node (319 lines)** - Base class, serialization, CRUD
2. ✓ **Vertex (194 lines)** - Edgeless nodes
3. ✓ **Edge (427 lines)** - Directed edges from→to→weight
4. ✓ **Schema (411 lines)** - Dynamic type system

**Total:** ~1,351 lines providing:
- CLOS representation of graph data
- Automatic serialization
- Extensible type system
- Complete CRUD operations
- Lazy data loading
- Automatic indexing
- Object caching

In the next layer (7), this infrastructure is exposed through high-level APIs.

*VivaceGraph Layer 6 Documentation*
*March 2026*
