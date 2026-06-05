# VivaceGraph - Layer 3: Persistence & Transactions

## Table of Contents

1. [Overview](#overview)
2. [Purpose and Responsibilities](#purpose-and-responsibilities)
3. [ACID Model](#acid-model)
4. [Layer 3 Components](#layer-3-components)
5. [Detailed Files](#detailed-files)
6. [Transaction Flow](#transaction-flow)
7. [Master-Slave Replication](#master-slave-replication)
8. [Crash Recovery](#crash-recovery)
9. [Load Order](#load-order)


## Overview

**Layer 3** is the **third and most crucial level** of VivaceGraph. It provides:

- **ACID Transactions** - Atomicity, Consistency, Isolation, Durability guarantees
- **Disk persistence** - Snapshots and transaction logs
- **Master-slave replication** - Redundancy and read scalability
- **Crash recovery** - Transaction replay from logs
- **Garbage collection** - Unreferenced memory cleanup

### Key Features

- ✓ **ACID-compliant:** Strong transaction guarantees
- ✓ **Optimistic Locking:** Minimizes locks, maximizes concurrency
- ✓ **Conflict Detection:** Detects and resolves conflicts
- ✓ **Replication Logs:** Exhaustive change logging
- ✓ **Crash Recovery:** Automatic failure recovery
- ✓ **Garbage Collection:** Automatic memory release

### Lines of Code

```
File                             Lines
────────────────────────────────────────
transactions.lisp                1402
transaction-restore.lisp            71
transaction-streaming.lisp         484
transaction-log-streaming.lisp     112
txn-log.lisp                        51
backup.lisp                         80
replication.lisp                     3
gc.lisp                            133
────────────────────────────────────────
TOTAL                            2336 lines
```

## Purpose and Responsibilities

### Why does Layer 3 exist?

VivaceGraph needs to **guarantee** that data:
1. Is written **atomically** (all or nothing)
2. Remains **consistent** (validation)
3. Is **isolated** (no interference between transactions)
4. Is **durable** (survives crashes)

Additionally, it needs:
- **Horizontal scalability:** Replication to slaves
- **Recovery:** Ability to restore from failures
- **Cleanup:** Free unreferenced memory

### Specific Responsibilities

| Responsibility | File | Reason |
|----------------|------|--------|
| Transaction control | `transactions.lisp` | Manage read/write sets, validation |
| Snapshot recovery | `transaction-restore.lisp` | Recreate state from backup |
| Streaming replication | `transaction-streaming.lisp` | Send changes to slaves |
| Log streaming | `transaction-log-streaming.lisp` | Incremental recovery |
| Transaction log | `txn-log.lisp` | Manage snapshots and replay |
| Backup/Snapshots | `backup.lisp` | Create point-in-time backups |
| Replication (stub) | `replication.lisp` | Replication placeholder |
| Garbage collection | `gc.lisp` | Clean unreferenced memory |

## ACID Model

### Atomicity

```
Guarantee: A transaction occurs COMPLETELY or NOT AT ALL
═══════════════════════════════════════════════════════════════

BEFORE:
  Graph:
    Vertex A: value=10
    Vertex B: value=20

TRANSACTION:
  update A.value = 15
  update B.value = 25
  
DURING (if it fails midway):
  ↑ Nothing changes (automatic rollback)
  
AFTER (if it completes):
  Vertex A: value=15
  Vertex B: value=25
  ↑ Both or neither (NEVER just one)
```

**Implementation:**

```lisp
(defmacro with-transaction ((transaction-manager) &body body)
  ;; Executes body inside a transaction
  ;; If error: automatic rollback
  ;; If success: automatic commit
  ...)

;; Example:
(with-transaction ((transaction-manager graph))
  (update-node node1 :value 15)
  (update-node node2 :value 25)
  ;; Either both apply or neither
)
```

### Consistency

```
Guarantee: Data always satisfies constraints
═══════════════════════════════════════════════════════════════

Possible Constraints:
  - Foreign key: Edge.from/to always point to existing vertices
  - Type: Vertex.age is always an integer ≥ 0
  - Custom: sum of all values ≤ 100
  
VivaceGraph Validates:
  ✓ Revisions (no race conditions)
  ✓ Type checking
  ✓ User-defined validators (if they exist)
  
If validation fails:
  → validation-conflict exception
  → Transaction retries (up to N times)
```

### Isolation

```
Guarantee: Concurrent transactions do not interfere
═══════════════════════════════════════════════════════════════

DANGEROUS SCENARIO:

Thread A: Reads Vertex X (value=10)
            ↓
Thread B:   Modifies Vertex X (value=20)
            Commit
            ↓
Thread A:   Uses old value (value=10) ← INCONSISTENCY!

VivaceGraph Uses OPTIMISTIC LOCKING:

Thread A: Reads Vertex X (value=10, revision=5)
         Modifies locally
         Attempts commit
         Validates: revision still 5?
            ├─ Yes: SUCCESS
            └─ No: CONFLICT, retry

Thread B: (in parallel)
         Modifies Vertex X
         Validates: revision changed → CONFLICT
         Retries
         
GUARANTEE: If both read the same, only ONE can write
```

### Durability

```
Guarantee: Once committed, it is permanent
═══════════════════════════════════════════════════════════════

COMMIT SEQUENCE:

1. Write changes to TRANSACTION LOG (on disk)
2. Write changes to REPLICATION LOG (send to slaves)
3. Apply changes to memory-mapped file
4. Sync file region (msync)
5. Confirm commit to client

If crash BEFORE step 1:
  → Transaction is reverted (it's in RAM)
  
If crash AFTER step 1:
  → Transaction is replayed in recovery
  
PROPERTY: Once commit returns, data is PERMANENT
```

## Layer 3 Components

### Dependency Diagram

```
LAYER 3 - INTERNAL DEPENDENCIES
================================

From LAYER 2:
    ↓
transactions.lisp ........... Txn control, object-sets
    ├─ Depends: LAYER 2 (locks, memory)
    ├─ Defines: transaction, transaction-manager
    └─ Uses: read-set, write-set, validation
         ↓
    transaction-restore.lisp . Restore from snapshot
         ├─ Depends: transactions.lisp
         └─ Creates: restore-transaction
              ↓
    transaction-streaming.lisp Txn replication
         ├─ Depends: transactions.lisp
         ├─ Depends: usocket (networking)
         └─ Protocols: master-slave
              ↓
    transaction-log-streaming.lisp Log streaming
         ├─ Depends: transaction-streaming.lisp
         └─ Reads: replication-*.log
              ↓
    txn-log.lisp ............. Log management
         ├─ Depends: backup.lisp
         └─ Implements: snapshot, replay
              ↓
    backup.lisp .............. Create snapshots
         ├─ Depends: transactions.lisp
         └─ Serializes: vertices/edges to disk
              ↓
    replication.lisp ......... (current stub)
         └─ Placeholder
              ↓
    gc.lisp .................. Garbage collection
         ├─ Depends: all (maps allocations)
         └─ Frees: unreferenced memory
```

## Detailed Files

### 1. `transactions.lisp` (1402 lines) - **KEY FILE**

**Purpose:** Core of the transaction system - full ACID control.

**Content:**

#### 1.1 Global Variables

```lisp
(defvar *transaction* nil)  ; Current transaction
(defvar *end-of-transaction-action* '%commit)  ; End action (commit/rollback)

(defparameter *maximum-transaction-attempts* 8
  "Number of retries before using exclusive lock")

(defparameter *add-to-indexes-unless-present-p* nil
  "Verify uniqueness when recovering?")
```

#### 1.2 Object Sets

An **object-set** tracks which objects were read/written.

```lisp
(defgeneric make-object-set (initial-contents)
  "Create object set")

(defgeneric object-set-count (set)
  "How many objects?")

(defgeneric add-to-object-set (object set)
  "Add object to set")

(defgeneric object-set-member-p (object set)
  "Is it a member?")

(defgeneric object-sets-intersect-p (set1 set2)
  "Is there overlap between two sets?")

(defclass object-set ()
  ((table :initform (make-id-table)
          :reader table)))
```

**Internal Usage:**

```lisp
;; Each transaction maintains:
(read-set txn)      ; ← Which vertices/edges were read
(write-set txn)     ; ← What was modified

;; Validation:
(if (object-sets-intersect-p 
     (read-set txn)
     (write-set other-committed-txn))
    (error 'validation-conflict)
    (commit txn))
```

#### 1.3 Transaction Exceptions

```lisp
(define-condition validation-conflict (error)
  ((transaction :initarg :transaction 
                :reader validation-conflict-transaction))
  (:report "The read/write sets conflict with another txn"))

(define-condition no-transaction-in-progress (error)
  ()
  (:report "No active transaction"))

(define-condition modifying-non-copy (error)
  ((node :initarg :node :reader modifying-non-copy-node))
  (:report "Modifying without copying first"))
```

**Validation Example:**

```
Thread A: read-set = {Vertex1}
          write-set = {Vertex1}
          attempts commit
          
Thread B: (already committed)
          write-set = {Vertex1}
          
Validation in Thread A:
  {Vertex1} (read) ∩ {Vertex1} (write B) ?
  → YES intersects → CONFLICT
  → Retry transaction
```

#### 1.4 Transaction Class

```lisp
(defclass tx ()
  ((sequence-number      ; Local ID in transaction-manager
    :accessor sequence-number)
   (state                ; :active, :committed, :aborted
    :accessor state)
   (start-tx-id          ; Txn ID when it started
    :accessor start-tx-id)
   (finish-tx-id         ; Txn ID when it finished
    :accessor finish-tx-id)
   (transaction-id       ; ID assigned at commit
    :accessor transaction-id)
   (read-set             ; Objects read
    :accessor read-set
    :initform (make-object-set nil))
   (write-set            ; Objects modified
    :accessor write-set
    :initform (make-object-set nil))
   (delete-set           ; Objects deleted
    :accessor delete-set
    :initform (make-object-set nil))
   (update-queue         ; Update queue
    :accessor update-queue
    :initform (make-empty-queue))
   (lock                 ; RW-lock for validation
    :accessor transaction-lock
    :initform (make-rw-lock))
   (transaction-manager  ; Reference to manager
    :accessor transaction-manager)
   (graph                ; Reference to graph
    :accessor graph)
   (graph-cache          ; Graph cache
    :accessor graph-cache)))
```

#### 1.5 Transaction Manager

```lisp
(defclass transaction-manager ()
  ((sequence-number          ; Local txn counter
    :accessor sequence-number :initform 0)
   (tx-id-counter            ; Global txn counter
    :accessor tx-id-counter)
   (transactions             ; Hash of active txns
    :reader transactions
    :initform (make-hash-table))
   (lock                     ; Manager lock
    :reader lock)
   (replication-log-file     ; Replication file
    :accessor replication-log-file)
   (replication-log          ; Open replication stream
    :accessor replication-log)
   (graph                    ; Reference to graph
    :reader graph)))
```

**Main Methods:**

```lisp
(defmethod create-transaction (transaction-manager)
  "Create new transaction"
  (with-recursive-lock-held ((lock transaction-manager))
    (let ((sequence-number (next-sequence-number transaction-manager))
          (graph (graph transaction-manager))
          (cache (cache graph)))
      (let ((tx (make-instance 'tx
                               :sequence-number sequence-number
                               :start-tx-id (tx-id-counter transaction-manager)
                               :transaction-manager transaction-manager
                               :graph graph
                               :graph-cache cache)))
        (add-transaction tx transaction-manager)
        (setf (state tx) :active)
        tx))))

(defmethod call-with-transaction (fun transaction-manager)
  "Execute function within a transaction"
  (let ((completed nil)
        (attempt-count 0))
    (loop
      ;; If too many attempts: use exclusive lock
      (when (<= *maximum-transaction-attempts* attempt-count)
        (with-transaction-manager-lock (transaction-manager)
          (return (call-transaction-fun))))
      
      ;; Try without exclusive lock
      (handler-case
          (return (call-transaction-fun))
        (validation-conflict ()
          (incf attempt-count))))))

(defmethod overlapping-transactions (transaction transaction-manager)
  "Find txns that could conflict"
  (let ((start (start-tx-id transaction))
        (finish (finish-tx-id transaction))
        (result '()))
    (do-committed-transactions (tx transaction-manager)
      (when (<= start (transaction-id tx) finish)
        (push tx result)))
    result))
```

**Convenience Macros:**

```lisp
(defmacro with-transaction ((transaction-manager) &body body)
  "Execute body within transaction"
  `(call-with-transaction (lambda () ,@body)
                         ,transaction-manager))

(defmacro do-transactions ((transaction transaction-manager) &body body)
  "Iterate over all txns (active + committed)")

(defmacro do-committed-transactions ((tx tm) &body body)
  "Iterate only over committed ones (have transaction-id)")

(defmacro do-active-transactions ((tx tm) &body body)
  "Iterate only over active ones (state = :active)")
```

#### 1.6 Copy-on-Write Interceptors

```lisp
(defmethod lookup-node :around (graph node-id)
  "Automatically do copy-on-write if modified"
  (let ((node (call-next-method)))
    (when *transaction*
      (add-to-object-set node (read-set *transaction*)))
    node))

(defmethod update-node :around (node graph)
  "Track write and validate transaction"
  (when *transaction*
    (add-to-object-set node (write-set *transaction*)))
  (call-next-method))

(defmethod delete-node :around (node graph)
  "Track deletion in transaction"
  (ensure-transaction ((transaction-manager graph))
    (call-next-method)))
```

**Read Flow:**

```
lookup-node("Vertex1")
    ↓
Is there an active transaction?
    ├─ Yes: add-to-object-set(Vertex1, read-set)
    │       ↓ (we track that it was read)
    └─ No: nothing
    
Returns: Vertex1
```

**Write Flow:**

```
update-node(Vertex1, :value 15)
    ↓
Is there an active transaction?
    ├─ Yes: add-to-object-set(Vertex1, write-set)
    │       ↓ (we track that it was written)
    └─ No: nothing
    
Modifies in RAM (changes local to the txn)
    ↓
Returns: ok
```

#### 1.7 Validation and Commit

```lisp
;; PSEUDOCODE of the commit flow:

(defgeneric apply-transaction (transaction graph)
  ;; 1. Validate (are there conflicts?)
  (unless (validate-transaction transaction graph)
    (signal 'validation-conflict))
  
  ;; 2. Assign transaction-id
  (assign-transaction-id transaction 
                        (transaction-manager graph))
  
  ;; 3. Write write-set to disk
  (dolist (node (object-set-list (write-set transaction)))
    (save-node node))
  
  ;; 4. Record in transaction log
  (log-transaction transaction graph)
  
  ;; 5. Update indexes
  (update-indexes transaction graph)
  
  ;; 6. Sync disk
  (sync-region (mmap graph))
  
  ;; 7. Mark as committed
  (setf (state transaction) :committed))
```

### 2. `transaction-restore.lisp` (71 lines)

**Purpose:** Restore graph state from **snapshots** (point-in-time backups).

**Content:**

```lisp
(defvar *restore-objects-per-transaction* 10
  "Objects to restore per transaction (to avoid overloading)")

(defun restore-sharp-paren-reader (stream subcharacter arg)
  "Custom reader for byte-arrays in snapshots"
  (let ((contents (read-delimited-list #\\) stream)))
    (coerce contents '(simple-array (unsigned-byte 8) (*)))))

(defparameter *restore-readtable*
  "Readtable with support for timestamps and byte-arrays"
  (let ((*readtable* (copy-readtable)))
    (local-time:enable-read-macros)
    (set-dispatch-macro-character #\\# #\\( 'restore-sharp-paren-reader)
    *readtable*))

(defun read-n-sexps (stream n)
  "Read N s-expressions from stream"
  (let ((sexps '()))
    (dotimes (i n (nreverse sexps))
      (let ((sexp (read stream nil stream)))
        (when (eq sexp stream)
          (return (values (nreverse sexps) :eof)))
        (push sexp sexps)))))

(defmacro do-snapshot-sexps ((var file &optional (count 10)) &body body)
  "Iterate over s-expressions in snapshot"
  `(call-for-snapshot-sexps (lambda (,var) ,@body)
                            ,file
                            ,count))

(defun recreate-graph (graph snapshot-file &key package-name)
  "Restore graph from snapshot"
  (let ((*package* (find-package package-name))
        (*readtable* *restore-readtable*)
        (*graph* graph)
        (count 0)
        (tx-id (load-highest-transaction-id graph)))
    
    ;; Read snapshot in batches
    (do-snapshot-sexps (plists snapshot-file *restore-objects-per-transaction*)
      
      ;; Create restore transaction
      (let ((*transaction* 
             (make-instance 'restore-transaction
                           :transaction-id (incf tx-id))))
        
        ;; Process each plist in the batch
        (dolist (plist plists)
          (incf count)
          (when (zerop (mod count 1000))
            (log:info "Restored ~A nodes" count))
          
          ;; Interpret plist by type
          (ecase (car plist)
            (:v (apply 'make-vertex (rest plist)))  ; Vertex
            (:e (apply 'make-edge (rest plist)))    ; Edge
            (:last-txn-id)                          ; Metadata
            (otherwise (log:error "Unknown: ~S" plist))))
        
        ;; Apply this batch's transaction
        (apply-transaction *transaction* graph)))
    
    ;; Save highest transaction ID
    (persist-highest-transaction-id (incf tx-id) graph)
    
    (values graph :count count)))
```

**Restoration Flow:**

```
snapshot-file.txt:
  (:v user id1 revision1 :name "Alice" :age 30)
  (:v user id2 revision1 :name "Bob" :age 25)
  (:e friend id3 revision0 :from id1 :to id2 :weight 1.0)
  (:v user id4 revision1 :name "Charlie" :age 35)
  ...
  
recreate-graph(graph, snapshot-file)
    ↓
Read 10 plists at a time
    ↓
Create restore-transaction
    ↓
make-vertex for each (:v ...)
make-edge for each (:e ...)
    ↓
apply-transaction (apply batch)
    ↓
Next batch of 10
    ↓
End of file
```

**Use Case:**

```lisp
;; Recover graph from crash:
(let ((graph (make-graph :name "mydb" :location "~/graphdb")))
  (recreate-graph graph "~/graphdb/txn-log/snap-1234567890.123456"
                  :package-name :my-package)
  ;; Graph is now in snapshot state
  )
```

### 3. `transaction-streaming.lisp` (484 lines)

**Purpose:** Streaming transaction replication to slaves (real-time redundancy).

**Content:**

#### 3.1 Replication Protocol

```lisp
(defvar *replication-protocol-version* 2)

(deftype broken-pipe-error () '(satisfies broken-pipe-error-p))

(deftype unfancy-plist-element ()
  '(or number string keyword boolean))

(alexandria:define-constant +replication-buffer-size+ 4096)
```

#### 3.2 Packet Read/Write

```lisp
(defun simple-socket-read (socket buffer length &key (eof-error-p t))
  "Read LENGTH bytes from socket"
  (let ((bytes-read (read-sequence buffer 
                                   (usocket:socket-stream socket)
                                   :end length)))
    (cond ((zerop bytes-read)
           (if eof-error-p
               (error 'eof-socket-error)
               nil))
          ((< bytes-read length)
           (error 'short-socket-read-error))
          ((= bytes-read length)
           bytes-read))))

(defun read-packet (socket &key (eof-error-p t))
  "Read a complete packet from socket"
  (let* ((buffer (make-byte-vector +replication-buffer-size+)))
    ;; Read header (8 size bytes)
    (simple-socket-read socket buffer 8 :eof-error-p eof-error-p)
    (let* ((packet-size (deserialize-uint64 buffer 0))
           (packet (make-byte-vector packet-size)))
      ;; Read rest of packet
      (replace packet buffer :end2 8)
      (multiple-value-bind (whole-buffers remainder)
          (truncate (- packet-size 8) +replication-buffer-size+)
        (dotimes (i whole-buffers)
          (simple-socket-read socket buffer +replication-buffer-size+)
          (replace packet buffer :start1 (+ 8 (* i +replication-buffer-size+))))
        (when (plusp remainder)
          (simple-socket-read socket buffer remainder)))
      packet)))

(defun write-packet (packet socket &key (start 0) end)
  "Write packet to socket"
  (write-sequence packet (usocket:socket-stream socket)
                  :start start :end end)
  (force-output (usocket:socket-stream socket)))
```

#### 3.3 Plist Packets (Authentication and Metadata)

```lisp
(defun serialize-plist-packet (plist)
  "Serialize plist to transport packet"
  ;; Header: [8 bytes size][1 byte flags][1 byte type]
  ;; Payload: encoded plist
  (let* ((payload (serialize-packet-plist plist))
         (size (+ (length payload) 10))  ; 8+1+1
         (packet (make-byte-vector size)))
    (serialize-uint64 packet size 0)    ; Size
    (setf (aref packet 8) 0)             ; Flags
    (setf (aref packet 9) (char-code #\p))  ; Type: 'p' for plist
    (replace packet payload :start1 10)
    packet))

(defun deserialize-plist-packet (packet)
  "Deserialize plist from packet"
  (check-packet-type packet (char-code #\p))
  (deserialize-packet-plist packet :offset 10))
```

**Authentication Protocol:**

```
Master ←→ Slave connection:

Master sends:
  {:protocol-version 2
   :graph-name "mydb"
   :schema-digest "abc123..."}

Slave responds:
  {:protocol-version 2
   :key "slave-key-secret"}

If version/schema don't match:
  → Connection rejected
  → Slave must sync from snapshot first
```

#### 3.4 Master-Slave Communication

```lisp
;; Replication session structure:

(defclass replication-session ()
  ((socket :accessor socket)
   (remote-address :accessor remote-address)
   (highest-transaction-id :accessor highest-transaction-id)
   (graph :accessor graph)
   ...))

;; On master: when txn commits
;; → Write to replication-log
;; → Send to all slaves
;; → Wait for slave ACK (if synchronous)
;; → Return success to client
```

### 4. `transaction-log-streaming.lisp` (112 lines)

**Purpose:** Incremental recovery - stream transaction logs for synchronization.

**Content:**

```lisp
;; Replication log files are named like:
;; "replication-<hex-txn-id>.log"
;; Example: "replication-0000000000000010.log"
;;          (starts with txn #16)

(defun all-replication-logs (graph)
  "List all replication logs"
  (directory (merge-pathnames "replication-*.log"
                             (persistent-transaction-directory graph))))

(defun parse-replication-log-name (replication-log)
  "Extract initial txn-id from file name"
  ;; "replication-00000010.log" → #x10 = 16
  (let ((name (pathname-name replication-log)))
    (when (search "replication-" name)
      (let ((start (length "replication-")))
        (parse-integer name :start start :radix 16)))))

(defun replication-log-ranges (replication-logs)
  "Calculate txn ranges for each log"
  ;; Example:
  ;; log1.log starts at txn 10, log2.log at txn 20
  ;; → log1 covers [10, 20)
  ;; → log2 covers [20, ∞)
  (let* ((starts (mapcar 'minimum-transaction-id replication-logs))
         (ends (append (mapcar '1- (rest starts))
                       (list most-positive-fixnum))))
    (mapcar 'cons starts ends)))

(defun applicable-replication-logs (transaction-id graph)
  "Find logs starting from transaction-id"
  ;; Example: if I want txns from 15 and I have:
  ;; log1 [10, 20), log2 [20, 30), log3 [30, ∞)
  ;; → Returns [log1, log2, log3] (all applicable)
  (let* ((logs (all-replication-logs graph))
         (ranges (replication-log-ranges logs))
         (target (cons transaction-id most-positive-fixnum)))
    (loop for log in logs
         for range in ranges
         when (ranges-overlap-p range target)
         collect log)))

(defun stream-replication-log (socket replication-log minimum-transaction-id)
  "Stream packets from log, filtering by minimum txn-id"
  (with-open-file (stream replication-log 
                         :element-type '(unsigned-byte 8))
    ;; Find first packet with txn-id >= minimum
    (loop
      (let ((packet (read-stream-packet stream)))
        (unless packet
          (return))
        (let* ((tx-header (deserialize-tx-header-vector packet)))
          (when (<= minimum-transaction-id (transaction-id tx-header))
            ;; Found: stream everything else
            (write-packet packet socket)
            (return))))
    ;; Stream rest of file without filters
    (stream-all-packets stream socket)))
```

**Catch-Up Flow:**

```
Slave connects to Master:
  Says: "I have txns up to #19, give me the rest"

Master searches applicable-replication-logs(20):
  Logs [0-100), [100-200), [200-∞)
  → Returns all 3

Master streams:
  1. First 100 packets from log 1 (from txn 20)
  2. All 100 packets from log 2
  3. All packets from log 3 (until end of file)

Slave receives and applies packets in order
  → Now synchronized with Master
```

### 5. `txn-log.lisp` (51 lines)

**Purpose:** Snapshot management and transaction replay.

**Content:**

```lisp
(defmethod snapshot ((graph graph) 
                    &key include-deleted-p
                         (check-data-integrity-p t))
  "Create graph snapshot"
  (with-recursive-lock-held ((txn-lock graph))
    
    ;; Verify integrity before snapshot
    (when check-data-integrity-p
      (let ((problems (check-data-integrity graph 
                                            :include-deleted-p include-deleted-p)))
        (if problems
            (return-from snapshot 
              (values :data-integrity-issues problems)))))
    
    ;; Create snapshot name
    (multiple-value-bind (sec msec) (gettimeofday)
      (let ((snap-file (format nil "~A/txn-log/snap-~D.~6,'0D"
                               (location graph) sec msec)))
        ;; Perform backup
        (backup graph snap-file :include-deleted-p include-deleted-p)))))

(defun find-newest-snapshot (dir)
  "Find most recent snapshot in directory"
  (let ((file (first (sort
                      (remove-if-not 
                       (lambda (file)
                         (cl-ppcre:scan "^snap-"
                                       (file-namestring file)))
                       (cl-fad:list-directory dir))
                      '> :key 'file-write-date))))
    (when file
      (values file (file-write-date file)))))

(defmethod replay ((graph graph) txn-dir package-name &key (check-integrity-p t))
  "Replay transactions from logs"
  ;; 1. Find most recent snapshot
  (let ((snapshot (find-newest-snapshot txn-dir)))
    (when snapshot
      (recreate-graph graph snapshot :package-name package-name)))
  
  ;; 2. Regenerate views
  (log:debug "Generating graph views.")
  (map nil
       (lambda (pair)
         (destructuring-bind (class-name . view-name) pair
           (regenerate-view graph class-name view-name)))
       (all-views graph))
  
  ;; 3. Validate integrity
  (log:debug "Checking data integrity.")
  (if check-integrity-p
      (check-data-integrity graph)
      graph))
```

**Recovery Scenario:**

```
Server crashes at 15:30
  ↓
Find most recent snapshot: snap-1609459800.000000
  (created 5 minutes ago)
  ↓
recreate-graph(graph, snapshot)
  → Restores state from 15:25
  ↓
replay(graph, txn-dir)
  → Reads transaction logs from 15:25 → 15:30
  → Re-applies all txns during those 5 minutes
  ↓
Graph now in EXACT state from 15:30
  (nothing lost!)
```

### 6. `backup.lisp` (80 lines)

**Purpose:** Create snapshots by serializing all vertices/edges.

**Content:**

```lisp
(defgeneric backup (object location &key include-deleted-p)
  (:documentation "Serialize object to file"))

(defmethod backup ((v vertex) (stream stream) &key include-deleted-p)
  "Serialize vertex as plist to stream"
  (declare (ignore include-deleted-p))
  (let ((plist
         `(:v
           ,(type-of v)
           ,(when (slot-boundp v 'data) (data v))
           :id ,(id v)
           :revision ,(revision v)
           :deleted-p ,(deleted-p v))))
    (let ((*print-pretty* nil))
      (format stream "~S~%" plist))))

(defmethod backup ((e edge) (stream stream) &key include-deleted-p)
  "Serialize edge as plist to stream"
  (let ((plist
         `(:e
           ,(type-of e)
           ,(from e)
           ,(to e)
           ,(weight e)
           ,(when (slot-boundp e 'data) (data e))
           :id ,(id e)
           :revision ,(revision e)
           :deleted-p ,(deleted-p e))))
    (let ((*print-pretty* nil))
      (format stream "~S~%" plist))))

(defmethod backup ((graph graph) location &key include-deleted-p)
  "Serialize entire graph to file"
  (ensure-directories-exist location)
  (let ((count 0))
    (with-open-file (out location :direction :output)
      ;; Backup all vertices
      (map-vertices (lambda (v)
                      (maybe-init-node-data v :graph graph)
                      (incf count)
                      (backup v out))
                    graph :include-deleted-p include-deleted-p)
      
      ;; Backup all edges
      (map-edges (lambda (e)
                   (maybe-init-node-data e :graph graph)
                   (incf count)
                   (backup e out))
                 graph :include-deleted-p include-deleted-p)
      
      (values count location))))

(defmethod check-data-integrity ((graph graph) &key include-deleted-p)
  "Verify that all data is deserializable"
  (let ((*cache-enabled* nil))
    (let ((problems nil) (count 0))
      (map-vertices (lambda (v)
                      (incf count)
                      (when (zerop (mod count 1000))
                        (format t "."))
                      (handler-case
                          (maybe-init-node-data v :graph graph)
                        (error (c)
                          (push (cons (string-id v) c) problems))))
                    graph :include-deleted-p include-deleted-p)
      
      (map-edges (lambda (e)
                   (incf count)
                   (handler-case
                       (maybe-init-node-data e :graph graph)
                     (error (c)
                       (push (cons (string-id e) c) problems))))
                 graph :include-deleted-p include-deleted-p)
      
      (terpri)
      problems)))
```

**Snapshot Format:**

```
snapshot-1609459800.000000:

(:v user id1 (:name "Alice" :age 30) :id #(...) :revision 1 :deleted-p nil)
(:v user id2 (:name "Bob") :id #(...) :revision 0 :deleted-p nil)
(:e friend id3 id1 id2 1.0 nil :id #(...) :revision 0 :deleted-p nil)
(:v user id4 (:name "Charlie" :age 35) :id #(...) :revision 2 :deleted-p nil)
(:e friend id5 id2 id1 1.0 nil :id #(...) :revision 0 :deleted-p nil)
...

Each line = one object (vertex or edge)
Format: (:v type data-plist :id ... :revision ... :deleted-p ...)
        (:e type from to weight data-plist :id ... :revision ... :deleted-p ...)
```

### 7. `replication.lisp` (3 lines)

**Purpose:** Placeholder - main replication is in `transaction-streaming.lisp`.

```lisp
(in-package :graph-db)

;; (empty in current version)
```

### 8. `gc.lisp` (133 lines)

**Purpose:** **Garbage Collection** - free unreferenced memory.

**Content:**

```lisp
(defun map-all-nodes (fn graph)
  "Call FN for each node (vertex or edge)"
  (map-vertices fn graph)
  (map-edges fn graph))

(defun map-node-addresses (fn graph)
  "Call FN with heap address of each node"
  (map-all-nodes (lambda (node)
                   (let ((data-pointer (data-pointer node)))
                     (unless (zerop data-pointer)
                       (funcall fn data-pointer))))
                 graph))

(defun map-index-list-addresses (fn index-list)
  "Call FN with address of each pcons in index"
  (let ((address (index-list-head index-list)))
    (loop
      (when (zerop address)
        (return))
      (funcall fn address)
      (let ((pcons (deserialize-pcons index-list address)))
        (setf address (%pcons-cdr pcons))))))

(defun map-edge-index-list-addresses (fn graph)
  "Map addresses of edge indexes (ve-index, vev-index)"
  (let ((in (ve-index-table (ve-index-in graph)))
        (out (ve-index-table (ve-index-out graph)))
        (vev (vev-index-table (vev-index graph))))
    (flet ((map-table (fn table)
             (map-lhash (lambda (cons)
                          (let ((index-list (cdr cons)))
                            (map-index-list-addresses fn index-list)))
                        table)))
      (map-table fn in)
      (map-table fn out)
      (map-table fn vev))))

(defun map-type-index-list-addresses (fn graph)
  "Map type-index addresses"
  (let ((edge-table (type-index-cache (edge-index graph)))
        (vertex-table (type-index-cache (vertex-index graph))))
    (flet ((map-table (fn table)
             (maphash (lambda (key index-list)
                        (declare (ignore key))
                        (map-index-list-addresses fn index-list))
                      table)))
      (map-table fn edge-table)
      (map-table fn vertex-table))))

(defun map-all-index-list-addresses (fn graph)
  "Map ALL index addresses"
  (map-type-index-list-addresses fn graph)
  (map-edge-index-list-addresses fn graph))

(defun heap-allocation-table (graph)
  "Hash table of ALL heap allocations"
  (let ((table (make-hash-table)))
    (map-all-heap-allocations (lambda (address)
                                (setf (gethash address table) t))
                              graph)
    table))

(defun graph-allocation-table (graph)
  "Hash table of allocations REFERENCED by the graph"
  (let ((table (make-hash-table)))
    (map-all-graph-allocations (lambda (address)
                                 (setf (gethash address table) t))
                               graph)
    table))

(defun gc-heap (graph)
  "Garbage-collect: free unreferenced allocations"
  (log:debug "gc-ing heap.")
  
  ;; 1. Create table of ALL allocations
  (let ((allocation-table (heap-allocation-table graph))
        (heap (heap graph)))
    
    ;; 2. Remove those that ARE referenced
    (map-all-graph-allocations (lambda (pointer)
                                 (remhash pointer allocation-table))
                               graph)
    
    ;; 3. Free those that REMAIN (unreferenced)
    (maphash (lambda (pointer _)
               (declare (ignore _))
               (log:debug "Freeing ~A" pointer)
               (free heap pointer))
             allocation-table))
  
  (log:debug "gc complete"))
```

**Mark-and-Sweep GC Algorithm:**

```
GC Mark-and-Sweep (simplified):
═════════════════════════════════════════════════════════════

MARKED SET = { }
FREE LIST = { all allocations }

MARK:
  For each node in the graph:
    For each allocation referenced by the node:
      MARKED.add(allocation)

SWEEP:
  For each allocation in FREE LIST:
    If NOT in MARKED:
      free(allocation)

GUARANTEE:
  - Nothing reachable is freed
  - Everything unreachable is freed
```

## Transaction Flow

### Simple Read

```
Thread A: lookup-node("Vertex1")
    ↓
Is there *transaction*?
    ├─ Yes: add-to-object-set(Vertex1, read-set)
    │       Returns: Vertex1 (from cache if possible)
    └─ No: Returns: Vertex1

read-set(TxnA) = {Vertex1}
write-set(TxnA) = {}
```

### Modification

```
Thread A: 
  (with-transaction ((tm graph))
    (let ((v (lookup-node graph "Vertex1")))  ; read
      (update-node v :value 15)))              ; write
    
    ↓
    
    read-set = {Vertex1}
    write-set = {Vertex1}
    
    Attempts commit
        ↓
    Validates: write-set ∩ (write-sets of committed txns) ?
        ├─ No intersection: OK, commit
        └─ Intersection: CONFLICT, retry
```

### Conflict

```
Thread A: read Vertex1 (revision 5)
Thread B: read Vertex1 (revision 5)
Thread B: modify Vertex1
          successful commit → revision 6

Thread A: attempts commit
          Validates: Vertex1 still revision 5?
          NO (now it's 6) → CONFLICT
          
          Retries:
          1. Re-reads Vertex1 (revision 6)
          2. Applies modification on new state
          3. Attempts commit again
          4. This time OK (no conflict)
```

## Master-Slave Replication

### Architecture

```
MASTER (writable)
├─ Transactions → Transaction Log
├─ Transaction Log → Replication Log (replication-*.log)
├─ Replication Log → SLAVE #1
├─ Replication Log → SLAVE #2
└─ Replication Log → SLAVE #3

SLAVE #1 (read-only)
├─ Receives Replication Log
├─ Applies changes locally
└─ Sends ACK to master

SLAVE #2 (read-only)
├─ Receives Replication Log
├─ Applies changes locally
└─ Sends ACK to master

SLAVE #3 (read-only)
├─ Receives Replication Log
├─ Applies changes locally
└─ Sends ACK to master
```

### Synchronization

```
Slave connects to Master (first time):

Master sends:
  1. Graph schema
  2. Metadata (highest txn id, etc.)

Slave responds:
  :hello {protocol-version 2, graph-name "mydb"}

Master:
  Schema OK?
    ├─ No: :error
    └─ Yes: :ok, begin streaming

Master sends:
  1. Most recent snapshot (replication-log or snapshot file)
  2. Transactions after the snapshot

Slave:
  1. Restores snapshot
  2. Applies each transaction
  3. Sends ACK for each txn

Master continues:
  New transactions are replicated in real-time
```

## Crash Recovery

### Scenario: Crash during transaction

```
Initial state:
  Vertex A: value=10
  Vertex B: value=20

Transaction starts:
  update A.value = 15
  update B.value = 25
  commit

Crash DURING commit (after writing A, before B)
  → On disk: A=15, B=20
  → INCONSISTENT

Recovery:
  1. Read most recent snapshot
  2. See last state (A=10, B=20)
  3. Replay all logs
  4. Is the crashed txn in the log?
     ├─ Yes: replayed, applied completely
     └─ No: ignored, implicit rollback

GUARANTEE: After recovery, state is CONSISTENT
```

### Scenario: Crash after commit

```
Successful transaction:
  update A=15, B=25
  commit recorded in log
  sent to slaves
  returned to client

Crash AFTER commit
  → On disk: A=15, B=25, tx-log OK

Recovery:
  1. Read snapshot
  2. Replay all logs (including recent commit)
  3. Restored state = pre-crash state

GUARANTEE: Transaction survives the crash
         (that's why it's "durability")
```

## Load Order

```
LOAD ORDER - LAYER 3
═══════════════════════════════════════════════════════════════

From LAYER 2:
    ↓
transactions.lisp ............ ACID control
    ├─ Depends: LAYER 2 (locks, memory, mmap)
    ├─ Depends: LAYER 1 (conditions, utilities)
    └─ Defines: transaction, transaction-manager, object-sets
         ↓
    transaction-restore.lisp .. Restore snapshots
         ├─ Depends: transactions.lisp
         └─ Defines: recreate-graph, restore-transaction
              ↓
    transaction-streaming.lisp  Replication
         ├─ Depends: transactions.lisp
         ├─ Depends: usocket (networking)
         └─ Defines: replication protocol, master-slave
              ↓
    transaction-log-streaming.lisp Log streaming
         ├─ Depends: transaction-streaming.lisp
         └─ Defines: replication-log-ranges, applicable-logs
              ↓
    backup.lisp .............. Create snapshots
         ├─ Depends: transactions.lisp
         └─ Defines: backup, check-data-integrity
              ↓
    txn-log.lisp ............. Log management
         ├─ Depends: backup.lisp
         └─ Defines: snapshot, replay
              ↓
    replication.lisp ......... (stub, placeholder)
         └─ Depends: nothing special
              ↓
    gc.lisp .................. Garbage collection
         ├─ Depends: all (maps allocations)
         └─ Defines: gc-heap, mark-and-sweep
```

## Summary

**Layer 3** provides:

1. ✓ **ACID Transactions** (transactions.lisp)
2. ✓ **Disk persistence** (backup.lisp, txn-log.lisp)
3. ✓ **Crash recovery** (transaction-restore.lisp, replay)
4. ✓ **Master-slave replication** (transaction-streaming.lisp)
5. ✓ **Incremental streaming** (transaction-log-streaming.lisp)
6. ✓ **Garbage collection** (gc.lisp)

**Total:** ~2,336 lines that guarantee:
- **Durability:** Data survives crashes
- **Consistency:** State is always valid
- **Isolation:** Transactions don't interfere
- **Atomicity:** All or nothing
- **Scalability:** Horizontal replication

In the following layers (4+), everything depends on these ACID guarantees.

*VivaceGraph Layer 3 Documentation*
*March 2026*
