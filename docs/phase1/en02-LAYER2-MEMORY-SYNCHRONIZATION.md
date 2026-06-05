# VivaceGraph - Layer 2: Memory & Synchronization

## Table of Contents

1. [Overview](#overview)
2. [Purpose and Responsibilities](#purpose-and-responsibilities)
3. [Memory Architecture](#memory-architecture)
4. [Layer 2 Components](#layer-2-components)
5. [Detailed Files](#detailed-files)
6. [Synchronization and Concurrency](#synchronization-and-concurrency)
7. [Interoperability Between Lisps](#interoperability-between-lisps)
8. [Load Order](#load-order)

## Overview

**Layer 2** is the **second fundamental level** of VivaceGraph. It manages:

- **Persistent memory** in mapped files (memory-mapped files)
- **Persistent data structures** (pcons, pmem)
- **Access synchronization** (read-write locks)
- **Inter-process communication** (queues, mailboxes)
- **Generic iterators** (cursors)

### Key Features

- ✓ **Persistent memory:** Data survives crashes
- ✓ **Synchronization:** Thread-safe access and reader-writer locks
- ✓ **Serialization:** Conversion to/from bytes in memory-mapped files
- ✓ **Segfault handling:** Automatic retry on invalid access
- ✓ **Compatible with multiple OSes:** Linux and Darwin (macOS)

### Lines of Code

```
File                       Lines
──────────────────────────────────
pcons.lisp                   37
pmem.lisp                    78
pstruct.lisp                 50
mmap.lisp                   266
rw-lock.lisp                185
queue.lisp                   43
mailbox.lisp                 31
cursors.lisp                 12
──────────────────────────────────
TOTAL                       702 lines
```

### Location in the Architecture

```
                    LAYER 3
               (Persistence)
                     ↑
                     │
         ┌──────────────────────┐
         │   LAYER 2 (HERE)     │
         │ Memory & Sync        │
         └──────────────────────┘
                     ↑
                     │
                    LAYER 1
             (Infrastructure)
```

## Purpose and Responsibilities

### Why does Layer 2 exist?

VivaceGraph needs to store data **persistently** on disk, but access it at **RAM speed**. This requires:

1. **File-to-memory mapping** (memory-mapped files)
2. **Custom memory manager** (persistent stack/heap)
3. **Access synchronization** (locks for concurrency)
4. **Failure handling** (segfaults, retries)

### Specific Responsibilities

| Responsibility | File | Reason |
|----------------|------|--------|
| Cons-like persistent structures | `pcons.lisp` | Base for persistent lists |
| Persistent memory model | `pmem.lisp` | Manage stack/heap on disk |
| Custom persistent structures | `pstruct.lisp` | Define persistent data types |
| Memory-mapped files | `mmap.lisp` | Disk access as memory |
| Reader-writer synchronization | `rw-lock.lisp` | Multiple readers, one writer |
| Thread-safe queues | `queue.lisp` | Asynchronous communication |
| Mailboxes | `mailbox.lisp` | IPC (Inter-Process Communication) |
| Cursors/Iterators | `cursors.lisp` | Generic traversal interface |

## Memory Architecture

### General Model

```
HARD DISK (Persistent)
├─ File main.dat (persistent hash table)
├─ File meta.dat (metadata)
├─ File data.dat (object data)
└─ File ...

         ↓ mmap() ↓ (memory-mapped file)

RAM MEMORY (Fast Cache)
├─ Region 1: STACK (grows upward)
│  ├─ Local variables
│  ├─ Function frames
│  └─ "Hot" data
│
├─ Region 2: HEAP (grows downward)
│  ├─ Persistent objects
│  ├─ Dynamic structures
│  └─ "Cold" data
│
└─ Control Points
   ├─ Stack Pointer (SPP): where the top of the stack is
   └─ Heap Pointer (HPP): where the top of the heap is

         ↓ Normal Access ↓

LISP APPLICATION
├─ Reads/writes to memory as normal
├─ Changes propagate to disk automatically
└─ Crash recovery via journal/log
```

### Persistent Stack vs Heap

```
STACK (Ascending)         HEAP (Descending)
┌──────────────┐          ┌──────────────┐
│ Top (SPP) ◄──┤          │ Top (HPP) ►──┤
├──────────────┤          ├──────────────┤
│ Variable 3   │          │ Object 3     │
├──────────────┤          ├──────────────┤
│ Variable 2   │          │ Object 2     │
├──────────────┤          ├──────────────┤
│ Variable 1   │          │ Object 1     │
├──────────────┤          ├──────────────┤
│ Base         │          │ Base         │
└──────────────┘          └──────────────┘

Stack grows →    Heap grows ←
(upward)         (downward)

Collision?
If SPP >= HPP: OUT OF MEMORY
```

## Layer 2 Components

### Internal Dependency Diagram

```
LAYER 2 - INTERNAL DEPENDENCIES
================================

(From LAYER 1)
    ↓
pcons.lisp ................... Persistent cons cells
    ↓
pmem.lisp .................... Memory model
    ├─→ pcons.lisp
    └─→ (Layer 1)
         ↓
    pstruct.lisp ............. Persistent structures
         ↓
    mmap.lisp ................ Memory-mapped files
         ├─→ pmem.lisp
         ├─→ (Layer 1: utilities, globals)
         └─→ (FFI: CFFI, osicat)
              ↓
    rw-lock.lisp ............. Reader-writer locks
         ├─→ queue.lisp
         └─→ (Bordeaux-threads)
              ↓
    queue.lisp ............... Queues
         ↓
    mailbox.lisp ............. Mailboxes
         ├─→ queue.lisp
         ├─→ rw-lock.lisp
         └─→ (Trivial-timeout)
              ↓
    cursors.lisp ............. Cursor interface
         └─→ (CLOS only)
```

## Detailed Files

### 1. `pcons.lisp` (37 lines)

**Purpose:** Define **persistent cons cells** - persistent Lisp cells that can be stored on disk.

**Content:**

```lisp
(defstruct (pcons
             (:constructor %make-pcons)
             (:conc-name %pcons-)
             (:print-function
              (lambda (c s d)
                (format s "#P(~A ~A (deleted-p ~A))"
                        (%pcons-car c) (%pcons-cdr c) 
                        (%pcons-deleted-p c)))))
  car           ; Head (type: byte array = ID)
  cdr           ; Tail (type: uint64 = heap offset)
  deleted-p)    ; Marked as deleted?
```

**Memory Structure:**

```
PCONS (25 bytes total):
┌─────────────────────────────┐
│ CAR (16 bytes)              │  Object UUID
├─────────────────────────────┤
│ CDR (8 bytes)               │  Heap offset
├─────────────────────────────┤
│ DELETED-P (1 byte)          │  Deletion flag
└─────────────────────────────┘
```

**Methods:**

```lisp
(defmethod serialize-pcons ((pcons pcons) heap)
  "Serialize pcons to persistent heap"
  (let ((address (allocate heap 25)))
    ;; Write car (16 bytes)
    (dotimes (i 16)
      (set-byte heap (+ i address) (aref car i)))
    ;; Write cdr (8 bytes)
    (serialize-uint64 heap cdr (+ 16 address))
    ;; Write deleted-p (1 byte)
    (let ((flags 0))
      (when deleted-p
        (setq flags (dpb 1 (byte 1 0) flags)))
      (set-byte heap (+ 24 address) flags))
    address))

(defgeneric deserialize-pcons (index-list address &optional buffer)
  "Deserialize pcons from heap")

(defmethod mark-pcons-deleted ((pcons pcons) heap address)
  "Mark pcons as deleted"
  (setf (%pcons-deleted-p pcons) t)
  (let ((flags (dpb 1 (byte 1 0) 0)))
    (set-byte heap (+ 24 address) flags)))
```

**Use Cases:**

- Persistent lists in indexes
- Chains of linked objects
- Skip list structures

**Note:** This is a very low-level pattern. Most code does not use pcons directly.

### 2. `pmem.lisp` (78 lines)

**Purpose:** Define the **Persistent Memory Model** - persistent memory manager with ascending stack and descending heap.

**Content:**

#### 2.1 PMEM Structure

```lisp
(defstruct (pmem
             (:conc-name %pmem-)
             (:constructor %make-pmem))
  memory          ; Memory-mapped file backing
  size            ; Total size (e.g.: 16MB)
  offset          ; Offset in file
  stack-pointer   ; Stack pointer (grows ↑)
  heap-pointer    ; Heap pointer (grows ↓)
  lock            ; Recursive lock for synchronization
  cache)          ; Cache hash table (weak references)
```

#### 2.2 Constants

```lisp
(defconstant +pmem-magic-byte+ #x1A)         ; Magic byte
(defconstant +stack-pointer-offset+ 1)       ; SPP offset
(defconstant +heap-pointer-offset+ 5)        ; HPP offset
(defconstant +stack-pointer-start-offset+ 9) ; Initial SPP
```

#### 2.3 PMEM Creation

```lisp
(defun make-pmem (memory &key (size (expt 2 24)))
  "Create new persistent memory model"
  (when (> size (expt 2 32))
    (error "Cannot create pmem greater than 2^32 bytes"))
  
  (let ((offset (allocate memory size)))
    ;; Write magic byte
    (set-byte memory offset +pmem-magic-byte+)
    
    ;; Create structure
    (let ((pmem (%make-pmem 
                 :memory memory
                 :size size
                 :offset offset
                 :stack-pointer (+ offset +stack-pointer-offset+)
                 :heap-pointer (+ offset +heap-pointer-offset+)
                 :lock (make-recursive-lock)
                 :cache (make-hash-table :weakness :value))))
      
      ;; Initialize pointers
      (setf (stack-pointer pmem) +stack-pointer-start-offset+)
      (setf (heap-pointer pmem) (+ offset size))
      
      pmem)))
```

#### 2.4 Memory Allocation

```lisp
(defmethod stack-allocate ((pmem pmem) (size integer))
  "Allocate from STACK (upward)"
  (with-recursive-lock-held ((%pmem-lock pmem))
    (let ((address (stack-pointer pmem)))
      (if (>= address (heap-pointer pmem))
          (error "Out of memory: stack reached heap")
          (progn
            (incf (stack-pointer pmem) size)
            address)))))

(defmethod heap-allocate ((pmem pmem) (size integer))
  "Allocate from HEAP (downward)"
  (with-recursive-lock-held ((%pmem-lock pmem))
    (let ((address (- (heap-pointer pmem) size)))
      (if (<= address (stack-pointer pmem))
          (error "Out of memory: heap reached stack")
          (progn
            (setf (heap-pointer pmem) address)
            address)))))
```

**Allocation Flow:**

```
Request: stack-allocate 100 bytes
    ↓
SPP + 100 >= HPP?
    ├─ Yes: ERROR "Out of memory"
    └─ No: SPP_new = SPP + 100, return SPP_old

Request: heap-allocate 50 bytes
    ↓
HPP - 50 <= SPP?
    ├─ Yes: ERROR "Out of memory"
    └─ No: HPP_new = HPP - 50, return HPP_new
```

**Thread Safety:**

```lisp
(with-recursive-lock-held ((%pmem-lock pmem))
  ;; Code here executes with mutual exclusion
  (incf (stack-pointer pmem) size))
```

### 3. `pstruct.lisp` (50 lines)

**Purpose:** Macro DSL for defining **persistent structures** - custom data types with validation and indexing.

**Content (Incomplete in the code):**

```lisp
(defmacro def-doc (name attribs slots)
  ;; Macro for defining persistent documents
  )

;; Provided example:
(def-doc customer ()
  ((name :type :string :validator 'valid-name-p :indexed-p t)
   (city :type :string :validator 'valid-city-p :indexed-p nil)
   (diseases :type :list :indexed-p nil :private-p t)))
```

**What it generates:**

```lisp
(defun create-customer (&key name city diseases id type revision deleted-p addr bytes)
  "Create new customer instance"
  (let ((%%id (or id (gen-id)))
        (%%type (or type 1))
        (%%revision (or revision 0))
        (%%deleted-p deleted-p)
        (%%addr addr)
        (%%bytes (or bytes :init))
        (%name name)
        (%city city)
        (%diseases diseases))
    
    ;; Closure acting as object
    (lambda (msg &optional arg)
      (case msg
        (:type %%type)
        (:id %%id)
        (:rev %%revision)
        (:deleted-p %%deleted-p)
        (:name %name)
        (:city %city)
        (:diseases %diseases)
        (:delete! (setq %%deleted-p t))
        (:set-name! (setq %name arg))
        (:set-city! (setq %city arg))
        (:set-diseases! (setq %diseases arg))
        (:as-json ...)
        (:save nil)
        (:bytes %%bytes)))))

(defun lookup-customer (id)
  "Look up customer by ID"
  (let* ((bytes (deserialize-customer (lookup-pointer id)))
         (type (deserialize-doc-type bytes)))
    (create-customer 
      :id (deserialize-doc-id bytes)
      :type type
      ...)))
```

**Features:**

- `:type` - Data type (string, integer, list, etc.)
- `:validator` - Validation function
- `:indexed-p` - Create automatic index?
- `:private-p` - Private field?

**Note:** This file is incomplete in VivaceGraph. It is a prototype for a persistent structure DSL that could be improved.

### 4. `mmap.lisp` (266 lines) - **KEY FILE**

**Purpose:** Handle **memory-mapped files** - map files into physical memory accessible as RAM.

**Content:**

#### 4.1 Mapped File Structure

```lisp
(defstruct (mapped-file
             (:conc-name m-)
             (:predicate mapped-file-p))
  path      ; File path
  pointer   ; CFFI pointer to mapped region
  fd)       ; File descriptor
```

**Custom Types:**

```lisp
(cffi:defctype size :unsigned-int)
(deftype uint32 () '(integer 0 4294967295))
(deftype uint40 () '(integer 0 1099511627775))
(deftype uint64 () '(integer 0 18446744073709551615))
(deftype word () '(unsigned-byte 64))  ; LispWorks
```

#### 4.2 Creating Mapped Files

```lisp
(defun mmap-file (file &key (create-p t) (size (* 4096 25600)))
  "Map file into memory"
  
  ;; If it doesn't exist and create-p, create file
  (when (and (not create-p) (not (probe-file file)))
    (error "File ~A does not exist" file))
  
  ;; Open file (create if necessary)
  (let* ((fd (osicat-posix:open
              file
              (if create-p
                  (logior osicat-posix:O-CREAT osicat-posix:O-RDWR)
                  osicat-posix:O-RDWR))))
    
    ;; If new, expand file to desired size
    (when create-p
      (osicat-posix:lseek fd (1- size) osicat-posix:seek-set)
      (cffi:foreign-funcall "write" :int fd :pointer ... size 1))
    
    ;; Map into memory
    (let* ((pointer (osicat-posix:mmap
                     (cffi:null-pointer)
                     size
                     (logior osicat-posix:prot-read 
                             osicat-posix:prot-write)
                     osicat-posix:map-shared
                     fd
                     0)))
      
      (make-mapped-file :path (truename file)
                       :fd fd
                       :pointer pointer))))
```

**What does mmap() do?**

```
Before (without mmap):
  Application → read() → Kernel → Disk → Buffer → Application
                                (slow, 3+ context switches)

With mmap:
  Application → [Mapped Memory] → Kernel ↔ Disk
                     (fast, direct access)
                     (the kernel handles automatic
                      synchronization)
```

#### 4.3 Segmentation Fault Handling

```lisp
(defmethod set-byte :around (mf offset byte)
  "Wrap set-byte with segfault handling"
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (log:error "SEGV in ~A, retrying..." mf)
      (set-byte mf offset byte))  ; Automatic retry
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (log:error "SEGV in ~A, retrying..." mf)
      (set-byte mf offset byte))))
```

**Why?** Memory-mapped files can fault if:
- The file is truncated
- The disk temporarily fails
- The region is expanded

VivaceGraph **automatically retries** in these cases.

#### 4.4 Read/Write Operations

```lisp
(defmethod set-byte ((mapped-file mapped-file) offset byte)
  "Write 1 byte at position"
  (declare (type word offset))
  (declare (type (integer 0 255) byte))
  (setf (cffi:mem-aref (m-pointer mapped-file) 
                       :unsigned-char offset) 
        byte))

(defmethod get-byte ((mapped-file mapped-file) offset)
  "Read 1 byte from position"
  (declare (type word offset))
  (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset))

(defmethod get-bytes ((mapped-file mapped-file) offset length)
  "Read N bytes from position"
  (let ((vec (make-byte-vector length)))
    (dotimes (i length)
      (setf (aref vec i) (get-byte mapped-file (+ i offset))))
    vec))

(defmethod set-bytes ((mapped-file mapped-file) vec offset length)
  "Write N bytes at position"
  (dotimes (i length)
    (set-byte mapped-file (+ i offset) (aref vec i)))
  vec)
```

#### 4.5 Disk Synchronization

```lisp
(defmethod sync-region ((mapped-file mapped-file) 
                       &key addr length
                       (sync osicat-posix:ms-sync))
  "Force region synchronization with disk"
  (osicat-posix:msync 
   (or addr (m-pointer mapped-file))
   (or length (mapped-file-length mapped-file))
   sync))

(defmethod munmap-file ((mapped-file mapped-file) &key (save-p nil) ...)
  "Unmap file from memory (with optional sync)"
  (when save-p
    (osicat-posix:msync (m-pointer mapped-file) ...))
  (osicat-posix:munmap (m-pointer mapped-file) ...)
  (osicat-posix:close (m-fd mapped-file))
  nil)
```

#### 4.6 Extending Mapped Files (Linux vs Darwin)

```lisp
#+linux
(defmethod extend-mapped-file ((mapped-file mapped-file) (length integer))
  "Extend mapped file using mremap() [LINUX]"
  (let ((ptr (osicat-posix:mremap 
              (m-pointer mapped-file)
              (mapped-file-length mapped-file)
              (+ length (mapped-file-length mapped-file))
              osicat-posix:MREMAP-MAYMOVE)))
    (setf (m-pointer mapped-file) ptr)
    mapped-file))

#+darwin
(defmethod extend-mapped-file ((mapped-file mapped-file) (length integer))
  "Extend mapped file by re-mapping [DARWIN/macOS]"
  (let ((len (mapped-file-length mapped-file)))
    (munmap-file mapped-file)
    ;; Re-map larger region
    (setf (m-pointer mapped-file)
          (osicat-posix:mmap ...))
    mapped-file))
```

**Why different?** Linux has `mremap()` which is more efficient. macOS does not, so you have to unmap and remap.

#### 4.7 Integer Serialization

```lisp
;; Serialize/deserialize integers in little-endian

(defmethod serialize-uint64 ((mf mapped-file) int offset)
  "Write uint64 in 8 bytes (little-endian)"
  (dotimes (i 8)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset)))

(defmethod deserialize-uint64 ((mf mapped-file) offset)
  "Read uint64 from 8 bytes"
  (let ((int 0))
    (dotimes (i 8)
      (setq int (dpb (get-byte mf (+ i offset)) 
                     (byte 8 (* i 8)) int)))
    int))

;; Same for uint32 (4 bytes) and uint40 (5 bytes)
```

**Note on Endianness:**

```
Little-endian (Intel x86, ARM):
  uint64 = 0x0102030405060708
  Bytes in memory: [08] [07] [06] [05] [04] [03] [02] [01]
  
Correct reading:
  byte[0] = 08 (bits 0-7)
  byte[1] = 07 (bits 8-15)
  byte[2] = 06 (bits 16-23)
  ...
```

### 5. `rw-lock.lisp` (185 lines) - **ADVANCED SYNCHRONIZATION**

**Purpose:** Implement **Read-Write Locks** - synchronization that allows multiple readers but only one writer.

**Content:**

#### 5.1 RW-Lock Structure

```lisp
(defstruct (rw-lock
             (:print-function print-rw-lock)
             (:predicate rw-lock-p))
  (lock (make-recursive-lock))        ; Internal mutex
  (readers 0 :type integer)           ; Reader count
  (semaphore (make-semaphore))        ; Semaphore for writers
  (writer-queue (make-empty-queue))   ; Writer queue
  (writer nil)                        ; Current writer
  (waitqueue (make-waitqueue)))       ; Condition variable
```

#### 5.2 Access Policy

```
RULES:
════════════════════════════════════════════════════════

1. MULTIPLE READERS:
   Reader 1 ──┐
   Reader 2 ──├─ CONCURRENT ACCESS
   Reader 3 ──┤
              └─ (simultaneously)

2. SINGLE WRITER:
   If Writer is active:
   Other Writers wait in queue
   New Readers CANNOT enter

3. MUTUAL EXCLUSION:
   Never: Reader + Writer simultaneously
   Never: Writer + Writer simultaneously

4. PRIORITY:
   Writers > Readers (to avoid starvation)
```

#### 5.3 Read Operations

```lisp
(defun acquire-read-lock (rw-lock &key (max-tries 1000))
  "Acquire read lock"
  (loop
    (with-recursive-lock-held ((lock-lock rw-lock))
      (if (lock-writer rw-lock)
          ;; There's a writer: wait for it to finish
          (condition-wait (lock-waitqueue rw-lock) 
                         (lock-lock rw-lock))
          ;; No writer: increment reader count
          (progn
            (incf (lock-readers rw-lock))
            (return-from acquire-read-lock rw-lock))))))

(defun release-read-lock (rw-lock)
  "Release read lock"
  (with-recursive-lock-held ((lock-lock rw-lock))
    (assert (not (eql 0 (lock-readers rw-lock))))
    (decf (lock-readers rw-lock))
    ;; If last reader: wake up writers
    (when (eql 0 (lock-readers rw-lock))
      (when (lock-writer rw-lock)
        (signal-semaphore (lock-semaphore rw-lock))))))

(defmacro with-read-lock ((rw-lock) &body body)
  `(unwind-protect
     (progn
       (acquire-read-lock ,rw-lock)
       ,@body)
     (release-read-lock ,rw-lock)))
```

**Read Flow:**

```
Thread A: acquire-read-lock
    ↓
Is there a writer?
    ├─ Yes: wait (condition-wait)
    └─ No: readers++, return
    
... perform read ...

Thread A: release-read-lock
    ↓
readers--
readers == 0 and there's a writer?
    ├─ Yes: signal-semaphore (wake writer)
    └─ No: nothing
```

#### 5.4 Write Operations

```lisp
(defun acquire-write-lock (rw-lock &key reading-p wait-p)
  "Acquire write lock"
  (with-recursive-lock-held ((lock-lock rw-lock))
    ;; If recursively owned: done
    (cond ((and (next-in-queue-p rw-lock (current-thread))
                (eq (lock-writer rw-lock) (current-thread)))
           (return-from acquire-write-lock rw-lock))
          ;; Wait or fail
          (wait-p
            (enqueue (lock-writer-queue rw-lock) (current-thread)))
          (t
            (if (lock-unused-p rw-lock)
                (progn
                  (enqueue (lock-writer-queue rw-lock) 
                          (current-thread))
                  (setf (lock-writer rw-lock) (current-thread))
                  (return-from acquire-write-lock rw-lock))
                (return-from acquire-write-lock nil)))))
  
  ;; Loop waiting for turn
  (loop
    (if (eq (lock-writer rw-lock) (current-thread))
        (return-from acquire-write-lock rw-lock)
        (let ((internal-wait-p nil))
          (with-recursive-lock-held ((lock-lock rw-lock))
            (if (and (null (lock-writer rw-lock))
                    (next-in-queue-p rw-lock (current-thread)))
                (progn
                  (setf (lock-writer rw-lock) (current-thread))
                  ;; If reader: release read
                  (when reading-p
                    (decf (lock-readers rw-lock)))
                  ;; If readers still present: wait
                  (unless (eql 0 (lock-readers rw-lock))
                    (setf internal-wait-p t)))
                (condition-wait (lock-waitqueue rw-lock) 
                               (lock-lock rw-lock))))
          ;; Wait for readers to finish
          (when internal-wait-p
            (wait-on-semaphore (lock-semaphore rw-lock)))))))

(defun release-write-lock (rw-lock &key reading-p)
  "Release write lock"
  (with-recursive-lock-held ((lock-lock rw-lock))
    (if (next-in-queue-p rw-lock (current-thread))
        (dequeue (lock-writer-queue rw-lock))
        (error "Cannot release lock I don't own!"))
    (if (next-in-queue-p rw-lock (current-thread))
        nil  ; Recursion detected
        (progn
          (setf (lock-writer rw-lock) nil)
          ;; If reader returning to read:
          (when reading-p
            (incf (lock-readers rw-lock)))
          ;; Wake other writers and readers
          (condition-broadcast (lock-waitqueue rw-lock))))))

(defmacro with-write-lock ((rw-lock &key reading-p) &body body)
  `(unwind-protect
     (progn
       (acquire-write-lock ,rw-lock :reading-p ,reading-p)
       ,@body)
     (release-write-lock ,rw-lock :reading-p ,reading-p)))
```

**Write Flow:**

```
Thread A: acquire-write-lock
    ↓
Am I the current writer?
    ├─ Yes: return (recursive ownership)
    └─ No: enqueue in writer-queue
    
Are there readers or a writer?
    ├─ Yes: wait-on-semaphore
    └─ No: setf writer = self, return
    
... perform write ...

Thread A: release-write-lock
    ↓
Next in queue = me?
    ├─ Yes: nothing (recursion)
    └─ No: dequeue, writer = nil
    
Wake waiting threads (writers + readers)
```

**Mode Change (Reader → Writer):**

```lisp
;; Convert read lock to write lock:
(with-read-lock (lock)
  ;; Reading...
  (with-write-lock (lock :reading-p t)
    ;; Safe conversion:
    ;; 1. Decrements readers
    ;; 2. Waits for other readers to finish
    ;; 3. Acquires write lock
    ;; Writing...
    ))  ; Auto-releases
```

### 6. `queue.lisp` (43 lines)

**Purpose:** Implement **thread-safe queues** for asynchronous communication.

**Content:**

```lisp
(defstruct (queue
             (:print-function
              (lambda (q stream depth)
                (format stream "<QUEUE: ~a>" (queue-elements q)))))
  (key #'identity)          ; Sort function
  (last nil)                ; Last cell (for fast append)
  (elements nil))           ; Elements

(defun make-empty-queue ()
  (make-queue))

(defun empty-queue-p (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  "View the first element (without removing)"
  (elt (queue-elements q) 0))

(defun dequeue (q)
  "Remove the first element"
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue-front (q &rest items)
  "Add items to the FRONT"
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-elements q) (nconc items (queue-elements q))
               (queue-last q) (last (queue-elements q))))
        (t (setf (queue-elements q) (nconc items (queue-elements q))))))

(defun enqueue (q &rest items)
  "Add items to the END"
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))
```

**Operations:**

```
empty-queue-p: O(1) - check length
queue-front:   O(1) - direct access
dequeue:       O(n) - pop is O(n) on lists
enqueue:       O(1) - amortized (using queue-last)
enqueue-front: O(1) - amortized
queue-length:  O(n) - traverse the list
```

**Usage in rw-lock.lisp:**

```lisp
(lock-writer-queue rw-lock)  ; Queue of waiting writers
(enqueue queue thread)       ; Add to end
(dequeue queue)              ; Remove from front
(next-in-queue-p lock thread) ; Is it next?
```

### 7. `mailbox.lisp` (31 lines)

**Purpose:** **Mailboxes** - message mailboxes for inter-thread and inter-process communication.

**Content:**

```lisp
(defstruct (mailbox
             (:conc-name mb-)
             (:constructor %make-mailbox))
  (lock (make-lock))        ; Protect access
  (queue (make-queue)))     ; Message queue

(defun make-mailbox ()
  "Create new mailbox"
  #+sbcl (sb-concurrency:make-mailbox)
  #+lispworks (mp:make-mailbox)
  #+ccl (%make-mailbox))

(defun send-message (mailbox message)
  "Send message to mailbox"
  #+sbcl (sb-concurrency:send-message mailbox message)
  #+lispworks (mp:mailbox-send mailbox message)
  #+ccl (with-lock ((mb-lock mailbox))
          (enqueue (mb-queue mailbox) message)))

(defun receive-message (mailbox &key (timeout 1))
  "Receive message from mailbox (with timeout)"
  #+sbcl (sb-concurrency:receive-message mailbox :timeout timeout)
  #+lispworks (mp:mailbox-read mailbox nil timeout)
  #+ccl (with-lock ((mb-lock mailbox) :timeout timeout)
          (let ((result nil))
            (handler-case
                (trivial-timeout:with-timeout (timeout)
                  (loop until result do
                    (setq result (dequeue (mb-queue mailbox)))))
              (com.metabang.trivial-timeout:timeout-error (c)
                (declare (ignore c))))
            result)))
```

**Communication Model:**

```
Thread A                    Thread B
    │                           │
    ├─ send-message("hello")   │
    │─────────────────────────→│
    │                           ├─ receive-message()
    │                           ├─ "hello" received
    │                           │
    │                      send-message("ack")
    │←─────────────────────────┤
    ├─ receive-message()
    ├─ "ack" received
    │
```

**Use Cases:**

- **Replication:** Master ↔ Slave
- **Async queries:** Client ↔ Server
- **Background tasks:** Worker threads

### 8. `cursors.lisp` (12 lines)

**Purpose:** Generic interface for **iterators/cursors**.

**Content:**

```lisp
(in-package :graph-db)

(defclass cursor ()
  ;; Base class for iterators
  ())

(defgeneric cursor-next (cursor &optional eoc)
  (:documentation "Get next element"))

(defgeneric cursor-prev (cursor &optional eoc)
  (:documentation "Get previous element"))

(defgeneric make-cursor (index &key cursor-class &allow-other-keys)
  (:documentation "Create cursor on index"))

(defgeneric make-values-cursor (index &key &allow-other-keys)
  (:documentation "Values-only cursor"))

(defgeneric make-keys-cursor (index &key &allow-other-keys)
  (:documentation "Keys-only cursor"))

(defgeneric make-range-cursor (index start end &key &allow-other-keys)
  (:documentation "Cursor on range [start, end)"))
```

**Usage Pattern:**

```lisp
;; Iterate over an index
(let ((cursor (make-cursor my-index)))
  (loop for item = (cursor-next cursor :eoc :done)
        until (eq item :done)
        do (process item)))

;; Specific range
(let ((cursor (make-range-cursor my-index start-key end-key)))
  (loop for key = (cursor-next cursor)
        while key
        do ...))
```

## Synchronization and Concurrency

### Concurrency Model

```
LAYER 2 - THREAD SAFETY
═══════════════════════════════════════════════════

1. PMEM (Persistent Memory):
   ├─ Each pmem has its own lock (recursive)
   ├─ stack-allocate: locks pmem
   └─ heap-allocate: locks pmem

2. MMAP (Memory-Mapped Files):
   ├─ Atomic byte-level operations
   ├─ set-byte: safe
   ├─ sync-region: thread-safe
   └─ Segfault handling: retries

3. RW-LOCK:
   ├─ Readers: CONCURRENT
   ├─ Writers: EXCLUSIVE
   └─ Reader→Writer transition: SAFE

4. QUEUE:
   ├─ Basic operations: O(1) amortized
   ├─ Not thread-safe on its own
   └─ Wrap with lock for multiple threads

5. MAILBOX:
   ├─ send-message: THREAD-SAFE
   ├─ receive-message: THREAD-SAFE + TIMEOUT
   └─ Different implementations per Lisp
```

### Deadlock Prevention

```
AVOIDING DEADLOCKS:
════════════════════════════════════════

1. LOCK ORDERING:
   Always acquire locks in the SAME order
   
   Safe:
   with-lock (lock1)
     with-lock (lock2)
       ...
   
   DANGER (deadlock possible):
   Thread A: with-lock (lock1) then lock2
   Thread B: with-lock (lock2) then lock1
             ↑ they can wait for each other

2. TIMEOUT:
   receive-message (mailbox :timeout 5)
   ↑ does not wait forever
   
   with-lock (lock :timeout 1)
   ↑ fail if not acquired in 1s

3. UNWIND-PROTECT:
   (unwind-protect
     (acquire-lock)
     (release-lock))
   ↑ guarantees release even if error
```

## Interoperability Between Lisps

### Main Differences

| Feature | SBCL | LispWorks | CCL |
|---|---|---|---|
| **Threads** | sb-thread | mp | ccl |
| **Mutex** | sb-thread:mutex | mp:lock | ccl:lock |
| **Semaphore** | sb-thread:semaphore | mp:semaphore | manual |
| **Condition Var** | sb-thread:waitqueue | mp:condition-variable | ccl:condition-variable |
| **Read-Write Lock** | custom (Layer 2) | custom (Layer 2) | native (ccl:rw-lock) |

### Compatibility Strategy

```lisp
;; Use conditional compilation directives
#+sbcl
(defun my-lock-function ()
  (sb-thread:with-recursive-lock (lock)
    ...))

#+lispworks
(defun my-lock-function ()
  (mp:with-lock (lock)
    ...))

#+ccl
(defun my-lock-function ()
  (ccl:with-lock-grabbed (lock)
    ...))
```

**Example in mmap.lisp:**

```lisp
;; Memory-mapped files: different on Linux vs Darwin
#+linux
(defmethod extend-mapped-file ...)  ; uses mremap()

#+darwin
(defmethod extend-mapped-file ...)  ; re-maps
```

## Load Order

**CRITICAL:** Respect the exact order.

```
LOAD ORDER - LAYER 2
═══════════════════════════════════════════════════

From LAYER 1:
    ↓
pcons.lisp ...................... Persistent cons
    ↓
pmem.lisp ....................... Memory model
    ↓
pstruct.lisp .................... Structures (less used)
    ↓
mmap.lisp ....................... Memory-mapped files
    ├─ Depends: LAYER 1 (utilities, globals)
    ├─ Depends: CFFI, osicat (FFI)
    └─ Defines: set-byte, get-byte, serialize-uint*
         ↓
    rw-lock.lisp ................ RW Locks
         ├─ Depends: queue.lisp
         ├─ Depends: bordeaux-threads
         └─ Uses: condition-wait, signal-semaphore
              ↓
    queue.lisp .................. Queues
         ├─ Depends: LAYER 1
         └─ Uses: enqueue, dequeue, queue-front
              ↓
    mailbox.lisp ................ Mailboxes
         ├─ Depends: queue.lisp, rw-lock.lisp
         ├─ Depends: sb-concurrency (SBCL)
         ├─ Depends: mp (LispWorks)
         ├─ Depends: trivial-timeout
         └─ Provides: send/receive-message
              ↓
    cursors.lisp ................ Cursor interface
         └─ Depends: CLOS only
```

**In graph-db.asd:**

```lisp
(:file "pcons" :depends-on ("mmap"))
(:file "pmem" :depends-on ("mmap"))
(:file "pstruct" :depends-on ("conditions" "buffer-pool"))
(:file "mmap" :depends-on ("rw-lock"))
(:file "rw-lock" :depends-on ("queue"))
(:file "queue" :depends-on ("utilities"))
(:file "mailbox" :depends-on ("queue"))
(:file "cursors" :depends-on ("package"))
```

## Summary

**Layer 2** provides:

1. ✓ **Persistent structures** (pcons, pmem)
2. ✓ **Memory-mapped files** (fast disk access)
3. ✓ **Advanced synchronization** (rw-locks)
4. ✓ **Thread-safe queues** (for communication)
5. ✓ **Mailboxes** (for IPC)
6. ✓ **Generic interface** (cursors/iterators)

**Total:** ~702 lines of code that handles:
- Disk persistence
- Thread-safe concurrent access
- Crash recovery
- Multi-Lisp compatibility

In the following layers (3+), all persistent memory usage and synchronization depends on **Layer 2**.

*VivaceGraph Layer 2 Documentation*
*March 2026*
