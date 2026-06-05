# VivaceGraph - Layer 4: Data Structures

## Table of Contents

1. [Overview](#overview)
2. [Purpose and Responsibilities](#purpose-and-responsibilities)
3. [Layer 4 Components](#layer-4-components)
4. [Detailed Files](#detailed-files)
5. [Skip Lists - Probabilistic Structure](#skip-lists---probabilistic-structure)
6. [Linear Hashing - Dynamic Hash Table](#linear-hashing---dynamic-hash-table)
7. [Memory and Allocation](#memory-and-allocation)
8. [Serialization](#serialization)
9. [Load Order](#load-order)

## Overview

**Layer 4** provides **efficient data structures** that VivaceGraph needs for:

- **Skip Lists:** Ordered indexes with O(log n) search
- **Linear Hash:** Dynamically growing hash table
- **Allocator:** Memory manager with bins and fragmentation
- **Buffer Pool:** Reusable object pool
- **Serialization:** Object-to/from-bytes conversion
- **Indexes:** Wrappers over skip lists for different data types

### Key Features

- ✓ **Skip Lists:** O(log n) search without rebalancing
- ✓ **Linear Hashing:** O(1) average, grows dynamically
- ✓ **Allocator:** Efficient fragmentation, multiple bins
- ✓ **Buffer Pool:** Object reuse to avoid GC
- ✓ **Serialization:** Type-agnostic, extensible
- ✓ **Persistence:** All structures in mapped memory

### Lines of Code

```
File                       Lines
──────────────────────────────────
skip-list.lisp               888
skip-list-cursors.lisp       122
linear-hash.lisp             764
allocator.lisp               334
buffer-pool.lisp             424
serialize.lisp               470
node-id.lisp                  54
index.lisp                    56
index-list.lisp              192
index-vector.lisp            197
──────────────────────────────────
TOTAL                       3501 lines
```

## Purpose and Responsibilities

### Why does Layer 4 exist?

VivaceGraph needs data structures that:
1. **Work in persistent memory** (not just RAM)
2. **Grow dynamically** (without full rehashing)
3. **Are efficient** (logarithmic, not linear)
4. **Support indexes** (for fast searches)

### Specific Responsibilities

| Responsibility | File | Reason |
|----------------|------|--------|
| Ordered indexes | `skip-list.lisp` | Logarithmic binary search |
| Skip-list iterators | `skip-list-cursors.lisp` | Traverse skip lists |
| Dynamic hash table | `linear-hash.lisp` | Hash with auto-growth |
| Memory management | `allocator.lisp` | Bins, free-lists |
| Object reuse | `buffer-pool.lisp` | Pool to avoid memory fragmentation |
| Type conversion | `serialize.lisp` | Objects ↔ bytes |
| ID generation | `node-id.lisp` | UUID v5 SHA1 |
| Index wrappers | `index.lisp` | Interface over skip-lists |
| In-memory indexes | `index-list.lisp` | Persistent linked lists |
| Index vectors | `index-vector.lisp` | Persistent arrays |

## Layer 4 Components

### Dependency Diagram

```
LAYER 4 - INTERNAL DEPENDENCIES
================================

From LAYER 3 (allocator, buffer-pool):
    ↓
skip-list.lisp ............... Skip lists
    ├─ Depends: allocator, buffer-pool
    ├─ Uses: random-level (Mersenne Twister)
    └─ Defines: skip-node, skip-list
         ↓
    skip-list-cursors.lisp ... Iterators
         ├─ Depends: skip-list.lisp
         └─ Defines: cursor, range-cursor, value-cursor
              ↓
    linear-hash.lisp ......... Dynamic hash table
         ├─ Depends: allocator, buffer-pool
         └─ Defines: lhash, buckets, overflow
              ↓
    allocator.lisp ........... Memory management
         ├─ Depends: LAYER 2 (mmap, pmem)
         └─ Defines: memory, free-list, bins
              ↓
    buffer-pool.lisp ......... Object pool
         ├─ Depends: allocator
         └─ Defines: buffer-pool, recycling
              ↓
    serialize.lisp ........... Type-agnostic serialization
         ├─ Depends: buffer-pool
         └─ Defines: type-tags, encoding
              ↓
    node-id.lisp ............. ID generation
         ├─ Depends: random, uuid, sha1 (ironclad)
         └─ Defines: gen-vertex-id, gen-edge-id
              ↓
    index.lisp ............... Index interface
         ├─ Depends: skip-list
         └─ Defines: index wrapper
              ↓
    index-list.lisp .......... Persistent lists
         ├─ Depends: allocator, pcons (Layer 2)
         └─ Defines: index-list, linked-lists
              ↓
    index-vector.lisp ........ Persistent vectors
         ├─ Depends: allocator
         └─ Defines: index-vector, persistent arrays
```

## Detailed Files

### 1. `skip-list.lisp` (888 lines) - **KEY FILE**

**Purpose:** Implement **skip lists** - probabilistic structure for ordered logarithmic search.

**What is a Skip List?**

```
NORMAL LIST (linear search O(n)):
┌─┬─┬─┬─┬─┬─┬─┬─┬─┐
│1│2│3│4│5│6│7│8│9│
└─┴─┴─┴─┴─┴─┴─┴─┴─┘
 ↑ traverse until found

SKIP LIST (O(log n) search):
Level 3: ┌─────────────────────────┐
         │1                        9│
Level 2: │1     │3     │5     │8  9│
Level 1: │1│2 │3│4│5│6│7│8  9│
Level 0: │1│2│3│4│5│6│7│8│9│
         └─┴─┴─┴─┴─┴─┴─┴─┴─┘

Search for 7:
  Level 3: 1 < 7, jump to 9
  Level 2: 1 < 7, try 3, 3 < 7, try 5, 5 < 7, try 8, 8 > 7
           go back to 5
  Level 1: 5 < 7, try 6, 6 < 7, try 7, FOUND
  
Comparisons: ~3 (instead of 7)
```

#### 1.1 Constants and Structures

```lisp
(alexandria:define-constant +max-level+ 64
  "Maximum skip-list level (enough for 2^64 elements)")

(alexandria:define-constant +skip-list-header-size+ 25
  "Bytes for: type, count, head, tail pointers")

;; NODE STRUCTURE:
;; ├─ Size (8 bytes) - total node size
;; ├─ Level (1 byte) - current level of this node
;; ├─ Flags (1 byte) - marked, fully-linked, locked
;; ├─ Pointers (level * 8 bytes) - forward pointers
;; ├─ Key (variable) - serialized key
;; └─ Value (variable) - serialized value
```

#### 1.2 Skip-Node Structure

```lisp
(defstruct (skip-node
             (:conc-name %sn-)
             (:predicate skip-node-p))
  (addr 0 :type (unsigned-byte 64))      ; Heap address
  (size 0 :type (unsigned-byte 64))      ; Size in bytes
  (level 0 :type (unsigned-byte 8))      ; Current level
  key                                     ; Key (in RAM, cached)
  skey                                    ; Serialized key
  value                                   ; Value (in RAM)
  svalue                                  ; Serialized value
  pointers                                ; Forward pointer array
  flags                                   ; Flags byte
  (head-p nil)                            ; Is head?
  (tail-p nil))                           ; Is tail?
```

**Memory Layout (disk):**

```
Heap address: 0x1000
┌──────────────────────────────────────────────────┐
│  Size (8)  │ Level (1) │ Flags (1) │ Pointers... │
├────────────┼───────────┼───────────┼─────────────┤
│ 0x1000-07  │ 0x1008    │ 0x1009    │ 0x100A-... │
│ (total)    │           │           │ (level*8) │
└──────────────────────────────────────────────────┴─────────────┐
                                                    │ Key (var)  │
                                                    │ Value (var)│
                                                    └────────────┘
```

#### 1.3 Skip-List Structure

```lisp
(defstruct (skip-list
             (:conc-name %sl-))
  (length 0 :type (unsigned-byte 64))       ; Number of nodes
  (max-level +max-level+ :type fixnum)      ; Maximum level
  (head nil)                                ; Sentinel node (head)
  (tail nil)                                ; Sentinel node (tail)
  (heap nil)                                ; Memory allocator
  (mmap nil)                                ; Memory-mapped file
  
  ;; Comparison/equality functions
  (key-equal #'equal)                       ; Keys equal?
  (key-comparison #'<)                      ; Key1 < Key2?
  
  ;; Serialization functions
  (key-serializer #'serialize)              ; Key → bytes
  (key-deserializer #'deserialize)          ; bytes → Key
  (value-serializer #'serialize)            ; Value → bytes
  (value-deserializer #'deserialize)        ; bytes → Value
  
  ;; Sentinels
  (head-key nil)                            ; Minimum key (-∞)
  (head-value nil)                          ; Dummy value
  (tail-key nil)                            ; Maximum key (+∞)
  (tail-value nil)                          ; Dummy value
  
  ;; Configuration
  (duplicates-allowed-p t)                  ; Allow duplicates?
  
  ;; Cache and locks
  (node-cache (make-hash-table))            ; Cache addr → skip-node
  (lock (make-rw-lock)))                    ; RW lock per list
```

#### 1.4 Main Operations

```lisp
(defun make-skip-list (&key heap key-equal key-comparison ...)
  "Create new skip-list"
  (let ((sl (%make-skip-list ...)))
    (let ((head (make-head sl ...))
          (tail (make-tail sl head ...)))
      sl)))

(defun add-to-skip-list (skip-list key value)
  "Add key→value, O(log n)"
  ;; 1. Find position
  (find-in-skip-list sl key)
  ;; 2. Create node with random level
  (let ((level (random-level)))
    (make-skip-node sl key value level))
  ;; 3. Insert at all levels
  (loop for i from 0 below level
        do (update-pointer pred i new-node))
  ;; 4. Mark fully-linked
  (set-node-fully-linked sl node)
  ...)

(defun find-in-skip-list (skip-list key &optional preds succs)
  "Search for key, O(log n)"
  ;; Returns: (node, level-found, preds, succs)
  (with-read-lock ((skip-list-lock skip-list))
    (let ((pred (%sl-head skip-list))
          (level-found -1))
      ;; Descend from maximum level
      (loop for level from (%sl-max-level skip-list) downto 0
            do
            ;; At each level, advance while less than
            (loop for node = (read-skip-node sl 
                                             (aref pointers pred level))
                  while (funcall (%sl-key-comparison sl)
                                (%sn-key node) key)
                  do (setq pred node))
            ;; Record predecessors/successors
            (setf (aref preds level) pred
                  (aref succs level) next-node)
            ;; Found?
            (when (funcall (%sl-key-equal sl)
                          (%sn-key next-node) key)
              (setq level-found level)))
      (values found-node level-found preds succs))))

(defun remove-from-skip-list (skip-list key)
  "Remove key, O(log n)"
  (with-write-lock ((skip-list-lock skip-list))
    (let ((node (find-in-skip-list sl key)))
      (when node
        ;; Mark as deleted
        (mark-node sl node)
        ;; Remove from all levels
        (loop for i from 0 below (%sn-level node)
              do (set-node-pointer sl pred i successor))
        ;; Free memory
        (free (sl-heap sl) (sn-addr node))
        t))))
```

**Random Levels:**

```lisp
(defun random-level (&optional (max-level +max-level+))
  "Random level following Pugh's pattern"
  ;; L1: 50%, L2: 25%, L3: 12.5%, etc.
  (do ((level 1 (1+ level)))
      ((or (= level max-level)
           (= (random 4) 3))  ; 1 in 4 probability
       level)))
```

**Complexity:**

```
Operation      Worst Case   Average     Space
────────────────────────────────────────────────
Search         O(n)         O(log n)    O(1)
Insertion      O(n)         O(log n)    O(log n)
Deletion       O(n)         O(log n)    O(1)
```

### 2. `skip-list-cursors.lisp` (122 lines)

**Purpose:** Iterators for traversing skip lists.

**Content:**

```lisp
(defclass skip-list-cursor (cursor)
  ((node :initarg :node :accessor skip-list-cursor-node)
   (skip-list :initarg :skip-list :accessor skip-list)))

(defmethod cursor-next ((slc skip-list-cursor) &optional eoc)
  "Get next node"
  (with-slots (node) slc
    (if (tail-p node)
        eoc  ; End of collection
        (let ((result node))
          (setq node (node-forward (skip-list slc) node))
          result))))

(defclass skip-list-value-cursor (skip-list-cursor)
  ;; Cursor that returns values only
  ())

(defmethod cursor-next :around ((slc skip-list-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (%sn-value result))))

(defclass skip-list-key-cursor (skip-list-cursor)
  ;; Cursor that returns keys only
  ())

(defclass skip-list-range-cursor (skip-list-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod cursor-next :around ((slc skip-list-range-cursor) &optional eoc)
  "Cursor limited to range [start, end)"
  (with-slots (node end) slc
    (if (funcall (%sl-key-comparison sl)
                (%sn-key node) end)
        (call-next-method)
        eoc)))
```

**Usage Patterns:**

```lisp
;; Iterate over all
(let ((cursor (make-cursor sl)))
  (do ((node (cursor-next cursor) (cursor-next cursor)))
      ((null node))
    (process node)))

;; Iterate values
(let ((cursor (make-values-cursor sl)))
  (do ((value (cursor-next cursor) (cursor-next cursor)))
      ((null value))
    (process-value value)))

;; Range [start, end)
(let ((cursor (make-range-cursor sl "alice" "bob")))
  (do ((key (cursor-next cursor) (cursor-next cursor)))
      ((null key))
    (process-key key)))
```

### 3. `linear-hash.lisp` (764 lines)

**Purpose:** Dynamic hash table that grows without full rehashing.

**What is Linear Hashing?**

```
NORMAL HASH TABLE:
  Size = 16 buckets
  ┌──┬──┬──┬──┬──┬──┬──┬──┐
  │  │  │ X│ X│  │ X│  │  │
  └──┴──┴──┴──┴──┴──┴──┴──┘
  
  Insert more: O(n) for full rehashing

LINEAR HASHING:
  Size = 4 buckets (base-buckets)
  Level = 0
  Split pointer = 0
  
  Insert → overflow?
    ├─ Yes: Expand (split) bucket[0]
    │        bucket[0] → bucket[0], bucket[4]
    │        Level++
    │        Split++
    └─ No: Normal insert
  
  Advantage: Incremental, no full rehash
```

#### 3.1 LHash Structure

```lisp
(defstruct (lhash
             (:conc-name %lhash-))
  (test 'uuid-array-equal)        ; Equality function
  (base-buckets 4)                ; Initial buckets
  (level 0)                        ; Current level
  (key-bytes +key-bytes+)         ; Key bytes
  (value-bytes +value-bytes+)     ; Value bytes
  (bucket-size +bucket-size+)     ; Items per bucket
  (next-split 0)                  ; Next to split
  (next-overflow-pointer 1)       ; Next overflow
  (count 0)                        ; Total items
  
  count-lock                       ; Lock for count
  split-lock                       ; Lock for splits
  overflow-lock                    ; Lock for overflow
  
  table                            ; Hash table (buckets)
  overflow                         ; Overflow buckets
  location                         ; Mmap file
  lock-vector                      ; Per-bucket locks
  
  key-serializer                   ; Key → bytes
  key-deserializer                 ; bytes → Key
  value-serializer                 ; Value → bytes
  value-deserializer)              ; bytes → Value
```

#### 3.2 Operations

```lisp
(defun lhash-get (lhash key)
  "Search in table, O(1) average"
  (let ((bucket-index (hash-key-to-bucket lhash key)))
    (let ((bucket (get-bucket lhash bucket-index)))
      (loop for entry in bucket
            when (funcall (%lhash-test lhash) key entry)
            return entry))))

(defun lhash-put (lhash key value)
  "Insert, with resize if necessary"
  (with-write-lock ((lhash-lock lhash))
    ;; 1. Find bucket
    (let ((bucket-index (hash-key-to-bucket lhash key)))
    
    ;; 2. Check overflow
    (if (> (bucket-size lhash) threshold)
        ;; Expand
        (split-bucket lhash)
        ;; Normal insert
        (insert-in-bucket lhash bucket-index key value))))

(defun split-bucket (lhash)
  "Split bucket[next-split] into bucket[next-split] + new-bucket"
  ;; 1. Create new bucket
  (let ((new-bucket (create-bucket lhash)))
    ;; 2. Redistribute entries from bucket[next-split]
    (loop for entry in (get-bucket lhash (%lhash-next-split lhash))
          do
          (let ((new-index (hash-key-to-bucket lhash (car entry))))
            (if (= new-index (%lhash-next-split lhash))
                (add-to-bucket lhash (%lhash-next-split lhash) entry)
                (add-to-bucket lhash new-bucket entry))))
    ;; 3. Update state
    (incf (%lhash-next-split lhash))
    (when (>= (%lhash-next-split lhash) (1- (expt 2 (%lhash-level lhash))))
      (setf (%lhash-next-split lhash) 0)
      (incf (%lhash-level lhash)))))
```

**Advantages vs Traditional:**

```
TRADITIONAL HASHING (full resize):
  Size: 4 → 8
  ┌─┬─┬─┬─┐    Rehash    ┌─┬─┬─┬─┬─┬─┬─┬─┐
  │X│X│X│X│ ─────────→  │ │ │ │ │ │ │ │ │
  └─┴─┴─┴─┘   O(n)       └─┴─┴─┴─┴─┴─┴─┴─┘
  
LINEAR HASHING (incremental):
  Size: 4, split 0       resize 1       resize 2
  ┌─┬─┬─┬─┐              ┌─┬─┬─┬─┬─┐   ┌─┬─┬─┬─┬─┬─┐
  │X│X│X│X│ ─O(1)→      │X│X│X│X│ │ → │X│X│X│X│ │ │
  └─┴─┴─┴─┘              └─┴─┴─┴─┴─┘   └─┴─┴─┴─┴─┴─┘
  
ADVANTAGE: Incremental, no O(n) pause
```

### 4. `allocator.lisp` (334 lines)

**Purpose:** Persistent memory manager with bins and free-lists.

**Content:**

```lisp
(defstruct (memory
             (:print-function ...))
  location                         ; File location
  size                             ; Total size
  mmap                             ; Memory-mapped file
  
  (free-list                       ; Free list by size
   (make-hash-table :synchronized t))
  
  (pointer 0)                      ; Current pointer
  (lock (make-rw-lock))            ; RW lock
  extent-size                      ; Extent size
  data-offset                      ; Data offset
  
  (bin-locks                       ; Per-bin locks
   (map-into (make-array 128) 'make-recursive-lock))
  
  (cache-lock (make-rw-lock))      ; Cache lock
  (cache                           ; Cache addr → object
   (make-hash-table :weakness :value)))
```

**Bins (Size Buckets):**

```
Bins for different sizes:
  Bin 0:   8-byte allocations
  Bin 1:   16-byte allocations
  Bin 2:   32-byte allocations
  ...
  Bin 127: 16384+ byte allocations

Allocation:
  allocate(memory, 50 bytes)
    ↓
  50 fits in Bin 3 (64 bytes)
    ↓
  Is there a free one in Bin 3?
    ├─ Yes: reuse
    └─ No: allocate new
```

**Operations:**

```lisp
(defun allocate (memory size)
  "Allocate memory, O(1) if free available"
  (with-write-lock ((memory-lock memory))
    ;; 1. Find bin for size
    (let ((bin (size-to-bin size)))
    ;; 2. Is there a free one in bin?
    (if (free-list-for-bin memory bin)
        ;; Reuse
        (let ((addr (pop (free-list-for-bin memory bin))))
          (initialize-allocation memory addr size)
          addr)
        ;; Allocate new
        (let ((addr (memory-pointer memory)))
          (incf (memory-pointer memory) size)
          (initialize-allocation memory addr size)
          addr))))

(defun free (memory address)
  "Free memory, return to free-list"
  (with-write-lock ((memory-lock memory))
    (let ((size (get-allocation-size memory address)))
      (let ((bin (size-to-bin size)))
        (push address (free-list-for-bin memory bin))))))

(defun grow-memory (memory length)
  "Extend mmap file"
  (let ((num-extents (ceiling (/ length (memory-extent-size memory)))))
    (extend-mapped-file (memory-mmap memory)
                        (* num-extents (memory-extent-size memory)))))
```

### 5. `buffer-pool.lisp` (424 lines)

**Purpose:** Reusable object pool to avoid fragmentation.

**Content:**

```lisp
(defvar *buffer-pool*
  "Global pool of reusable buffers"
  (make-hash-table :test 'eq :synchronized t))

(defvar *buffer-pool-stats*
  "Pool statistics")

(defvar *buffer-pool-low-water-mark* 1000
  "Minimum buffers before refresh")

(defvar *free-memory-low-water-mark* 10485760
  "Minimum free memory (10MB) before forced GC")

(defstruct (buffer-pool-stats
             (:conc-name bps-))
  (buffer-8 0)        ; 8-byte buffers
  (buffer-16 0)       ; 16-byte buffers
  (buffer-18 0)       ; 18-byte buffers
  (pcons 0)           ; Persistent cons cells
  (vertex 0)          ; Vertex objects
  (edge 0)            ; Edge objects
  (skip-node 0))      ; Skip-list nodes
```

**Operations:**

```lisp
(defun get-buffer (size)
  "Get buffer of size, from pool if possible"
  #+sbcl (sb-ext:atomic-pop (first (gethash size *buffer-pool*)))
  #+lispworks (sys:atomic-pop (car (gethash size *buffer-pool*)))
  ;; If none available: create one
  (or existing (make-byte-vector size)))

(defun return-buffer (buffer)
  "Return buffer to pool"
  (let ((size (length buffer)))
    #+sbcl (sb-ext:atomic-push buffer (first (gethash size *buffer-pool*)))
    #+lispworks (sys:atomic-push buffer (car (gethash size *buffer-pool*)))))

(defun monitor-buffer-pool ()
  "Monitor thread that keeps pool full"
  (loop
    (sleep 5)  ; Check every 5 seconds
    (let ((free-memory (free-memory)))
      (if (<= free-memory *free-memory-low-water-mark*)
          ;; Force GC if memory low
          #+sbcl (sb-ext:gc :full t)
          #+ccl (gc)
          #+lispworks (hcl:gc-generation 2)
          ;; Refresh buffers if pool low
          (dolist (buffer-type '(8 16 18 24 34))
            (when (< (pool-count buffer-type) *buffer-pool-low-water-mark*)
              (dotimes (i 1000)
                (return-buffer (make-byte-vector buffer-type)))))))))
```

### 6. `serialize.lisp` (470 lines)

**Purpose:** Type-agnostic object serialization.

**Content:**

```lisp
(defgeneric serialize (object)
  (:documentation "Convert object to byte-array"))

(defgeneric deserialize (bytes)
  (:documentation "Convert byte-array to object"))

;; Type tags (type identifiers):
(defconstant +null+ 0)
(defconstant +t+ 1)
(defconstant +uuid+ 2)
(defconstant +positive-integer+ 3)
(defconstant +negative-integer+ 4)
(defconstant +single-float+ 5)
(defconstant +double-float+ 6)
(defconstant +character+ 7)
(defconstant +bit-vector+ 8)
(defconstant +timestamp+ 9)
(defconstant +string+ 10)
(defconstant +list+ 11)
(defconstant +vector+ 12)
(defconstant +vertex+ 13)
(defconstant +edge+ 14)
(defconstant +mpointer+ 15)
; ...
```

**Serialization Layout:**

```
Fixed-size types (1 byte type + 1 byte size + data):
┌──────┬───────┬────────────────────┐
│Type  │Size   │ Data               │
│+uuid │2      │16 bytes UUID       │
└──────┴───────┴────────────────────┘

Variable-size types (1 byte type + N bytes length + data):
┌──────┬──┬──────┬────────────────────┐
│Type  │N │Length│ Data               │
│+string│1 │25    │"hello world..."    │
└──────┴──┴──────┴────────────────────┘
```

**Operations:**

```lisp
(defmethod serialize ((obj null))
  #(0))  ; +null+ = 0

(defmethod serialize ((obj (eql t)))
  #(1))  ; +t+ = 1

(defmethod serialize ((uuid array))
  ;; UUID: +uuid+ (1) + length (1) + data (16)
  (let ((v (make-byte-vector 18)))
    (setf (aref v 0) +uuid+)
    (setf (aref v 1) 16)
    (dotimes (i 16)
      (setf (aref v (+ 2 i)) (aref uuid i)))
    v))

(defmethod serialize ((str string))
  ;; String: +string+ (1) + length-bytes (var) + data
  (let* ((utf8 (babel:string-to-octets str :encoding :utf-8))
         (len-bytes (encode-length (length utf8)))
         (v (make-byte-vector (+ 1 (length len-bytes) (length utf8)))))
    (setf (aref v 0) +string+)
    (dotimes (i (length len-bytes))
      (setf (aref v (+ 1 i)) (aref len-bytes i)))
    (dotimes (i (length utf8))
      (setf (aref v (+ 1 (length len-bytes) i)) (aref utf8 i)))
    v))

(defmethod deserialize ((bytes array))
  ;; Inspect type tag
  (let ((type-byte (aref bytes 0)))
    (case type-byte
      (#.+null+ nil)
      (#.+t+ t)
      (#.+uuid+ (subseq bytes 2 18))
      (#.+string+
       (let ((len-bytes-count (aref bytes 1)))
         (let ((len (decode-length (subseq bytes 2 (+ 2 len-bytes-count))))
               (start (+ 2 len-bytes-count)))
           (babel:octets-to-string (subseq bytes start (+ start len))
                                  :encoding :utf-8))))
      ...)))
```

**Extensibility:**

```lisp
;; User can define new types:
(defmethod serialize ((my-obj my-custom-class))
  (let ((v (make-byte-vector ...)))
    (setf (aref v 0) +my-custom-type+)  ; New tag
    ;; Serialize fields
    v))

(defmethod deserialize-my-custom ((bytes array) offset)
  ;; Deserialize
  (make-instance 'my-custom-class ...))
```

### 7. `node-id.lisp` (54 lines)

**Purpose:** Unique identifier generation (UUID v5).

**Content:**

```lisp
(defun generate-uuid-name ()
  "Generate name for UUID v5 using time + random"
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (sec msec)
      #+sbcl (sb-ext:get-time-of-day)
      #-sbcl (osicat-posix:gettimeofday)
    (let* ((total-bytes 40)
           (vec (make-array total-bytes :element-type '(unsigned-byte 8))))
      ;; Write seconds
      (let ((n-bytes (ceiling (integer-length sec) 8)))
        (dotimes (i n-bytes)
          (setf (aref vec i) (ldb (byte 8 (* i 8)) sec))))
      ;; Write milliseconds
      (let ((n-bytes (ceiling (integer-length msec) 8)))
        (dotimes (i n-bytes)
          (setf (aref vec (+ offset i)) (ldb (byte 8 (* i 8)) msec))))
      ;; Random bytes
      (loop for i from offset below total-bytes
            do (setf (aref vec i) (random 256)))
      vec)))

(defun gen-v5-uuid (namespace)
  "Generate UUID v5 (SHA1 name-based)"
  (let ((name (generate-uuid-name))
        (digester (ironclad:make-digest :sha1)))
    ;; Hash: SHA1(namespace || name)
    (ironclad:update-digest digester namespace)
    (ironclad:update-digest digester name)
    (let ((hash (ironclad:produce-digest digester)))
      ;; UUID v5 format (RFC 4122)
      (let ((id (subseq hash 0 16)))
        ;; Set version (5) and variant (RFC)
        (let ((time-high (dpb #b0101 (byte 4 12) ...)))
          ;; ... update version bits
          id))))

(defun gen-vertex-id ()
  "Generate UUID for vertex"
  (gen-v5-uuid *vertex-namespace*))

(defun gen-edge-id ()
  "Generate UUID for edge"
  (gen-v5-uuid *edge-namespace*))
```

**Properties:**

```
- Deterministic: same input → same UUID
- Unique: P(collision) < 2^-128
- Distributed: uniformly spaced
- Non-sequential: impossible to predict next
```

### 8. `index.lisp` (56 lines)

**Purpose:** Wrapper over skip-lists to create typed indexes.

**Content:**

```lisp
(defstruct index
  skip-list              ; Underlying skip-list
  key-type              ; :string, :number, :custom
  order                 ; :ascending, :descending
  unique-p              ; Unique keys?
  heap                  ; Memory allocator
  addr)                 ; Heap address

(defun make-string-index (heap &key (order :ascending) unique-p)
  "Create string index"
  (let ((pointer (allocate heap 20))
        (skip-list (make-skip-list 
                    :heap heap
                    :key-equal 'string=
                    :key-comparison 
                    (if (eql order :ascending) 'string< 'string>)
                    :head-key (string (code-char 0))
                    :tail-key (string (code-char 65535))
                    :duplicates-allowed-p (null unique-p))))
    (let ((index (make-index :skip-list skip-list
                             :key-type :string
                             :order order
                             :unique-p unique-p
                             :heap heap
                             :addr pointer)))
      ;; Serialize metadata
      (set-byte (memory-mmap heap) pointer +db-version+)
      (set-byte (memory-mmap heap) (incf pointer) +string-index+)
      (set-byte (memory-mmap heap) (incf pointer)
               (if (eql order :ascending) 
                   +index-ascending+ +index-descending+))
      (set-byte (memory-mmap heap) (incf pointer)
               (if unique-p +index-unique+ +index-not-unique+))
      index)))

(defun make-number-index (heap &key ...)
  "Create numeric index")

(defun make-custom-index (heap key-type ...)
  "Create custom index")
```

### 9. `index-list.lisp` (192 lines)

**Purpose:** Persistent linked lists using pcons.

**Content:**

```lisp
(defstruct (index-list
             (:constructor %make-index-list))
  heap                  ; Memory allocator
  (cache ...)           ; Pcons cache
  head                  ; Head address
  (lock (make-rw-lock)) ; RW lock
  dirty-p)              ; Modified?

(defmethod deserialize-pcons ((il index-list) address &optional buffer)
  "Read pcons from heap"
  (let ((pcons (or buffer (get-pcons-buffer))))
    (setf (%pcons-car pcons) (get-bytes (index-list-heap il) address 16)
          (%pcons-cdr pcons) (deserialize-uint64 
                              (index-list-heap il) (+ 16 address))
          (%pcons-deleted-p pcons) (ldb-test (byte 1 0) 
                                             (get-byte ...)))
    pcons))

(defmethod map-index-list (fn (il index-list) &key collect-p)
  "Iterate over index-list"
  (let ((address (index-list-head il))
        (result nil))
    (loop until (zerop address)
          do
          (let ((pcons (deserialize-pcons il address)))
            (if collect-p
                (push (funcall fn (%pcons-car pcons)) result)
                (funcall fn (%pcons-car pcons)))
            (setq address (%pcons-cdr pcons))))
    (nreverse result)))

(defmethod index-list-push ((uuid array) (il index-list))
  "Add to front of index-list"
  (let ((pcons (get-pcons-buffer)))
    (setf (%pcons-car pcons) uuid
          (%pcons-cdr pcons) (index-list-head il)
          (%pcons-deleted-p pcons) nil)
    (let ((address (serialize-pcons pcons (index-list-heap il))))
      (setf (index-list-head il) address)
      (setf (index-list-dirty-p il) t))))
```

### 10. `index-vector.lisp` (197 lines)

**Purpose:** Persistent vectors (dynamic arrays).

**Content:**

```lisp
(defstruct (index-vector
             (:constructor %make-index-vector))
  (address 0)          ; Heap address
  (size 0)             ; Number of elements
  (vector #())         ; Array of UUIDs
  heap)                ; Memory allocator

(defun make-index-vector (heap key-vector)
  "Create persistent vector"
  (let* ((total-size (+ 8 (* 16 (length key-vector))))
         (address (allocate heap total-size))
         (index-vector (%make-index-vector 
                       :address address
                       :heap heap
                       :size (length key-vector)
                       :vector key-vector)))
    ;; Serialize size
    (serialize-uint64 (memory-mmap heap) 
                     (index-vector-size index-vector) address)
    ;; Serialize elements
    (incf address 8)
    (dotimes (i (index-vector-size index-vector))
      (dotimes (j 16)
        (set-byte (memory-mmap heap) (incf address)
                 (aref (aref key-vector i) j))))
    index-vector))

(defun get-index-vector (heap address)
  "Read persistent vector"
  (let* ((size (deserialize-uint64 (memory-mmap heap) address))
         (index-vector (%make-index-vector 
                       :address address :heap heap :size size
                       :vector (make-array size))))
    ;; Deserialize elements
    (incf address 8)
    (dotimes (i size)
      (let ((key (get-buffer 16)))
        (dotimes (j 16)
          (setf (aref key j) 
                (get-byte (memory-mmap heap) (incf address))))
        (setf (aref (index-vector-vector index-vector) i) key)))
    index-vector))

(defun index-vector-push-extend (index-vector key)
  "Add element, expanding if necessary"
  (let* ((heap (index-vector-heap index-vector))
         (new-size (1+ (index-vector-size index-vector)))
         (total-size (+ 8 (* 16 new-size)))
         (new-address (allocate heap total-size)))
    ;; Copy old elements
    (dotimes (i (index-vector-size index-vector))
      (dotimes (j 16)
        (set-byte (memory-mmap heap) (incf new-address)
                 (aref (aref old-vector i) j))))
    ;; Add new element
    (dotimes (j 16)
      (set-byte (memory-mmap heap) (incf new-address)
               (aref key j)))
    ;; Free old
    (free heap (index-vector-address index-vector))
    ;; Update
    (setf (index-vector-address index-vector) new-address
          (index-vector-size index-vector) new-size)))
```

## Skip Lists - Probabilistic Structure

### Advantages vs B-Trees

```
SKIP LISTS:
  ✓ Simpler to implement
  ✓ Comparable O(log n) performance
  ✓ No rebalancing required
  ✓ Good cache locality
  ✗ Extra memory usage (pointers)

B-TREES:
  ✓ Better cache locality
  ✓ Fewer pointers
  ✗ More complex
  ✗ Requires rebalancing
```

### Level Distribution

```
With +max-level+ = 64 and P = 1/4:

Level 0: 100%    ████████████████████████████████████████
Level 1:  25%    ██████████
Level 2:   6%    ██
Level 3:   1%    
Level 4:   0%    

Expected nodes per level:
Level 0: n
Level 1: n/4
Level 2: n/16
Level 3: n/64
...
Total overhead: n * (1 + 1/4 + 1/16 + ...) ≈ 1.33n
```

## Linear Hashing - Dynamic Hash Table

### Advantages vs Rehashing

```
TRADITIONAL REHASHING:
  Load factor > threshold
  → Rehash everything (O(n))
  → Observable pause
  
LINEAR HASHING:
  Load factor > threshold
  → Split bucket[next]
  → Rehash only ~ n/initial_size items
  → Incremental, no pause
```

## Memory and Allocation

### Bins and Free-Lists

```
Free-list structure:
  ┌─────────────────────────────────────┐
  │ Bin 0 (8B):  [addr1] → [addr2]     │
  │ Bin 1 (16B): [addr3] → [addr4]     │
  │ Bin 2 (32B): [addr5] → [addr6]     │
  │ ...                                 │
  │ Bin 127 (16KB+): [addr7] → nil     │
  └─────────────────────────────────────┘

Allocate(memory, 50 bytes):
  1. Find bin for 50B → Bin 3 (64B)
  2. Check Bin 3:
     ├─ Has free: pop and reuse
     └─ No free: allocate new
```

## Serialization

### Length Encoding

```
Variable-length encoding for economy:

Integer 5 (needs 1 byte):
  Encoded: [1, 5]  (1 byte for length, 5 is the value)

Integer 256 (needs 2 bytes):
  Encoded: [2, 0, 1]  (1 byte length=2, 2 bytes for value)

Integer 65536 (needs 3 bytes):
  Encoded: [3, 0, 0, 1]  (1 byte length=3, 3 bytes for value)
```

## Load Order

```
LOAD ORDER - LAYER 4
═══════════════════════════════════════════════════════════════

From LAYER 3:
    ↓
allocator.lisp ............. Memory management
    ├─ Depends: LAYER 2 (mmap, pmem)
    └─ Defines: memory, bins, free-lists
         ↓
    buffer-pool.lisp ........ Object pool
         ├─ Depends: allocator
         └─ Defines: buffer-pool, recycling
              ↓
    node-id.lisp ........... ID generation
         ├─ Depends: ironclad (SHA1)
         └─ Defines: gen-vertex-id, gen-edge-id
              ↓
    serialize.lisp ......... Serialization
         ├─ Depends: buffer-pool
         └─ Defines: type-tags, encode/decode
              ↓
    skip-list.lisp ......... Skip lists
         ├─ Depends: allocator, serialize
         └─ Defines: skip-node, skip-list, insert/search
              ↓
    skip-list-cursors.lisp . Iterators
         ├─ Depends: skip-list
         └─ Defines: cursor, range-cursor
              ↓
    linear-hash.lisp ....... Hash table
         ├─ Depends: allocator, serialize
         └─ Defines: lhash, buckets, splits
              ↓
    index.lisp ............. Index wrappers
         ├─ Depends: skip-list
         └─ Defines: index, string/number/custom
              ↓
    index-list.lisp ........ Persistent lists
         ├─ Depends: allocator, pcons (Layer 2)
         └─ Defines: index-list, linked-lists
              ↓
    index-vector.lisp ...... Persistent vectors
         ├─ Depends: allocator
         └─ Defines: index-vector, dynamic-arrays
```

## Summary

**Layer 4** provides:

1. ✓ **Skip Lists (888 lines)** - Ordered O(log n) indexes
2. ✓ **Skip List Cursors (122 lines)** - Iterators
3. ✓ **Linear Hashing (764 lines)** - Dynamic hash
4. ✓ **Allocator (334 lines)** - Memory management with bins
5. ✓ **Buffer Pool (424 lines)** - Reusable pool
6. ✓ **Serialization (470 lines)** - Type-agnostic conversion
7. ✓ **Node ID (54 lines)** - UUID generation
8. ✓ **Index (56 lines)** - Index wrappers
9. ✓ **Index List (192 lines)** - Persistent lists
10. ✓ **Index Vector (197 lines)** - Persistent vectors

**Total:** ~3,501 lines of code providing:
- Efficient data structures (O(log n), O(1))
- Disk persistence
- Dynamically managed memory
- Object pool to avoid GC
- Extensible serialization

In the following layers (5+), everything uses these structures as a base.

*VivaceGraph Layer 4 Documentation*
*March 2026*
