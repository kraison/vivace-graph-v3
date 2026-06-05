# Layer 1 Component Specification: utilities.lisp

**Component:** VivaceGraph Utility Functions & Cross-Lisp Abstractions  
**File:** src/utilities.lisp  
**Lines:** 483  
**Type:** Mixed utility functions, macros, generic operators  
**Purpose:** Provide foundational utilities (debugging, time, lists, UUID, hashing, locking) used by all layers  
**Scope:**
- ✓ Provides 50+ utility functions
- ✓ Provides 6 cross-Lisp macros for locking/synchronization
- ✓ Provides generic comparison operators for mixed-type sorting
- ✗ Does NOT provide networking
- ✗ Does NOT provide serialization (that's Layer 4)
- ✗ Does NOT provide graph operations

---

## Conceptual Model

### Core Idea

**`utilities.lisp` is the utility foundation upon which all other layers are built.**

It serves three purposes:

1. **Utility Functions:** Common operations (list manipulation, time, UUID, hashing) used throughout codebase
2. **Cross-Lisp Abstractions:** Hide Lisp-specific differences (CCL vs SBCL vs LispWorks) behind uniform interfaces (with-lock, with-locked-hash-table, gettimeofday)
3. **Generic Operators:** Define comparison operations for skip-list indexing (less-than, greater-than)

### Motivation

Without `utilities.lisp`:
- ❌ Code would be littered with #+ conditionals for SBCL/CCL/LW
- ❌ Each layer would reimplement common utilities (flatten, find-all, UUID parsing)
- ❌ No uniform locking interface (code would use different APIs per Lisp)
- ❌ No mixed-type ordering (skip-lists could only handle single type)

With `utilities.lisp`:
- ✅ Clean cross-Lisp abstractions (with-lock works everywhere)
- ✅ DRY principle (utilities defined once, reused everywhere)
- ✅ Uniform API (all layers use same utility functions)
- ✅ Flexible ordering (skip-lists support any type via generics)

### Abstraction Boundary

**What it exposes:**
- 50+ utility functions (debugging, lists, time, UUID, hashing)
- 6 cross-Lisp macros (locking, synchronization)
- 2 generic operator families (less-than, greater-than)
- Supporting functions (key-vector<, key-vector<=, key-vector>)

**What it hides:**
- Lisp implementation details (#+ conditionals become internal)
- FFI calls (gettimeofday LispWorks FFI is hidden)
- Generic method dispatch mechanics

**What it should hide but currently doesn't:** ⚠️
- with-locked-hash-table broken on CCL/LW (does NOT actually lock)
- Inconsistent lock semantics across Lisps (timeout only on CCL, recursive on SBCL)
- Complex endianness handling in read-id-array-from-string

### Key Properties

Property | Value | Rationale
----------|-------|----------
**Completeness** | 🟡 Partial | Covers major utilities but some LW support incomplete
**Consistency** | 🟡 Mixed | APIs uniform across Lisps except locking semantics
**Correctness** | 🟠 At-risk | Blocking issues (#2 blocking, #2 critical)
**Performance** | 🟡 Issues | key-vector< O(n²), needs optimization
**Extensibility** | ✅ Good | Generic operators extensible; easy to add utilities
**Thread-safety** | 🔴 BROKEN | with-locked-hash-table not thread-safe on CCL/LW
**Backward compatibility** | ✅ Good | Stable utilities; unlikely to change

---

## Interface Definition

### Category A: Debugging & Output

#### Operation 1: Debug Print (dbg)

**Signature:**

```lisp
(dbg fmt &rest args)
→ nil
```

**Semantics:**

```lisp
(dbg "Vertex: ~A, Revision: ~D~%" vertex-obj 5)
→ Prints to stdout: "Vertex: #<VERTEX v-123>, Revision: 5"
→ Returns: nil
```

**Behavior:**
- Takes format string + arguments
- Calls (apply #'format t fmt args)
- Calls terpri (print newline)
- Returns nil

**Guarantee:**
- Output printed to stdout
- Newline appended
- No exceptions

**Performance:**
- Time: O(n) where n = format string length + arg serialization
- Space: O(1)

**Common patterns:**

```lisp
; Pattern A: Variable inspection
(dbg "Node: ~A~%" node)

; Pattern B: Condition tracking
(let ((count 0))
  (loop
    (dbg "Attempt ~D~%" (incf count))
    (if (try-operation) (return) (sleep 1))))

; Pattern C: Trace execution
(dbg "Entering function foo with ~A~%" arg)
```

**Risks:**
- ⚠️ Hardcoded to stdout (cannot redirect to file/stream)
- ⚠️ No log levels (debug, info, warn, error)
- ⚠️ Performance impact in tight loops (I/O blocking)

---

#### Operation 2: Suppress Compiler Warnings (ignore-warning)

**Signature:**

```lisp
(ignore-warning condition)
→ nil (never returns; calls muffle-warning)
```

**Semantics:**

```lisp
(handler-bind ((warning #'ignore-warning))
  (some-code-that-warns))
→ Warning suppressed; code continues
```

**Behavior:**
- Accepts condition object
- Calls muffle-warning (Lisp restart)
- Never returns

**Guarantee:**
- Warning suppressed
- Exception restart triggered
- Code after handler continues

**Usage pattern:**

```lisp
; Suppress specific warnings
(handler-bind ((sb-ext:compiler-note #'ignore-warning))
  (compile-function ...))
```

---

### Category B: Time & Clock Functions

#### Operation 3: Get Current Time (gettimeofday)

**Signature:**

```lisp
(gettimeofday)
→ (values secs usecs)  ; or combined seconds
```

**Semantics:**

Cross-Lisp implementation returns different formats:

```lisp
#+sbcl
(gettimeofday)
→ #<number>  ; seconds + microseconds as decimal

#+ccl
(gettimeofday)
→ (values secs usecs)  ; seconds, microseconds

#+lispworks
(gettimeofday)
→ (values secs usecs)  ; seconds, microseconds
```

**Behavior:**
- #+SBCL: Calls sb-ext:get-time-of-day, returns (sec + msec/1000000)
- #+CCL: Uses FFI to C gettimeofday, extracts tv_sec and tv_usec, returns both values
- #+LispWorks: Uses FLI to C gettimeofday, extracts fields, returns both values

**Guarantee:**
- Returns current UNIX time
- Microsecond precision available

**Performance:**
- Time: O(1) system call
- Space: O(1)

**Common patterns:**

```lisp
; Pattern A: Measure elapsed time
(let ((start (gettimeofday)))
  (operation)
  (let ((elapsed (- (gettimeofday) start)))
    (format t "Took ~A seconds~%" elapsed)))

; Pattern B: Timestamp logging
(let ((now (gettimeofday)))
  (log-event event :timestamp now))

; Pattern C: Timeout implementation
(defun with-timeout (seconds fn)
  (let ((deadline (+ (gettimeofday) seconds)))
    (loop
      (if (funcall fn)
        (return)
        (if (> (gettimeofday) deadline)
          (error 'timeout-error))))))
```

**Risks:**
- ⚠️ Inconsistent return type (SBCL vs CCL/LW)
- ⚠️ LispWorks TODO comment suggests incomplete implementation

---

#### Operation 4: Time Conversion (universal-to-unix-time, unix-to-universal-time)

**Signature:**

```lisp
(universal-to-unix-time universal-time) → unix-time
(unix-to-universal-time unix-time)       → universal-time
```

**Semantics:**

```lisp
; Lisp universal time: seconds since 1900-01-01
; Unix time: seconds since 1970-01-01
; Difference: 2208988800 seconds

(universal-to-unix-time 2461276800)  ; 2048-01-01 00:00:00 UTC
→ 2461276800 - 2208988800
→ 252288000

(unix-to-universal-time 252288000)
→ 252288000 + 2208988800
→ 2461276800
```

**Behavior:**
- Precomputed epoch difference stored in *unix-epoch-difference*
- Subtraction/addition straightforward

**Guarantee:**
- Lossless conversion (reversible)
- Correct for all valid times

**Performance:**
- Time: O(1)
- Space: O(1)

---

#### Operation 5: Get Current Unix Time (get-unix-time)

**Signature:**

```lisp
(get-unix-time) → unix-timestamp
```

**Semantics:**

```lisp
(get-unix-time)
→ (universal-to-unix-time (get-universal-time))
→ Current time in Unix format (seconds since 1970)
```

**Behavior:**
- Calls get-universal-time (Lisp standard)
- Converts to Unix format

**Guarantee:**
- Returns current Unix timestamp

---

### Category C: List Utilities

#### Operation 6: List Operations (flatten, find-all, find-anywhere, etc.)

**Signatures:**

```lisp
(flatten x)                                    → list
(find-all item sequence &rest keyword-args)   → list
(find-anywhere item tree)                      → item or nil
(find-if-anywhere predicate tree)              → item or nil
(unique-find-anywhere-if predicate tree)       → list
(last1 lst)                                    → item
(length=1 lst)                                 → boolean
```

**Semantics (example: flatten):**

```lisp
(flatten '(1 (2 3) ((4 5) 6)))
→ (1 2 3 4 5 6)

; Recursive descent: (a (b c)) → (a b c)
```

**Behavior:**
- flatten: Recursively descends list, collects all atoms
- find-all: Uses complement + remove (inverted logic)
- find-anywhere: Recursively searches tree for exact match
- find-if-anywhere: Recursively searches with predicate
- unique-find-anywhere-if: Deduplicates results
- last1: Returns last element
- length=1: Tests if exactly one element

**Guarantee:**
- All operations non-destructive (return new lists)
- No side effects

**Performance:**
- flatten: O(n) where n = total atoms (traverses entire structure)
- find-all: O(n) where n = sequence length
- find-anywhere: O(n) where n = tree size (worst case entire tree)
- find-if-anywhere: O(n)
- unique-find-anywhere-if: O(n log n) due to adjoin
- last1: O(n) (must traverse to end)
- length=1: O(1)

**Common patterns:**

```lisp
; Pattern A: Flatten nested result
(let ((results (query-vertices graph predicate)))
  (flatten results))

; Pattern B: Find matching elements
(find-all :type "person" people :test #'eq)

; Pattern C: Search tree structure
(find-if-anywhere #'vertex-p graph-structure)
```

---

### Category D: UUID & ID Management

#### Operation 7: Generate UUID (gen-id)

**Signature:**

```lisp
(gen-id) → byte-array (16 elements)
```

**Semantics:**

```lisp
(gen-id)
→ #(0xAB 0xCD 0xEF ...)  ; Random UUID as byte-array
```

**Behavior:**
- Generates random v4 UUID
- Converts to byte-array (16 unsigned bytes)

**Guarantee:**
- Returns random UUID (statistically unique)
- Format: 16-byte array

**Performance:**
- Time: O(1) UUID generation
- Space: O(1) (16 bytes allocated)

**Common patterns:**

```lisp
; Pattern A: Generate vertex ID
(defun make-vertex (graph type-name data)
  (let ((id (gen-id)))
    (store-vertex graph id type-name data)))

; Pattern B: Generate edge ID
(defun make-edge (graph from to label data)
  (let ((id (gen-id)))
    (store-edge graph id from to label data)))
```

---

#### Operation 8: Parse UUID from String (read-uuid-from-string)

**Signature:**

```lisp
(read-uuid-from-string string) → uuid:uuid object
```

**Semantics:**

```lisp
(read-uuid-from-string "6ba7b810-9dad11d180b400c04fd430c8")
→ #<UUID 6ba7b810-9dad-11d1-80b4-00c04fd430c8>
```

**Behavior:**
1. Removes dashes from input
2. Validates length (must be 32 hex chars)
3. Parses 5 blocks: time-low (8), time-mid (4), time-high (4), clock-seq-var (2), clock-seq-low (2), node (12)
4. Creates uuid:uuid instance

**Guarantee:**
- Correct UUID object created from string
- Raises error on invalid format

**Performance:**
- Time: O(32) (parse all hex characters)
- Space: O(1)

**Common patterns:**

```lisp
; Pattern A: Parse UUID from user input
(handler-case
  (read-uuid-from-string user-input)
  (error (e) (format t "Invalid UUID: ~A~%" user-input)))

; Pattern B: Convert string to UUID for lookup
(let ((vertex-id (read-uuid-from-string "6ba7b810-9dad11d180b400c04fd430c8")))
  (lookup-vertex graph vertex-id))
```

---

#### Operation 9: Parse UUID to Byte-Array (read-id-array-from-string)

**Signature:**

```lisp
(read-id-array-from-string string) → byte-array (16 elements)
```

**Semantics:**

```lisp
(read-id-array-from-string "6ba7b810-9dad11d180b400c04fd430c8")
→ #(0x6B 0xA7 0xB8 0x10 ...)  ; 16-byte representation
```

**Behavior:**
- Parses UUID string
- Extracts hex blocks
- Uses bit operations (ldb) to convert to little-endian byte-array
- Returns 16-element array of (unsigned-byte 8)

**Guarantee:**
- Returns correct byte representation of UUID
- Byte order: little-endian conversion applied

**Performance:**
- Time: O(32) (parse all hex characters + bit operations)
- Space: O(1) (16 bytes)

**Risks:**
- ⚠️ Complex endianness handling (easy to get wrong)
- ⚠️ Difficult to verify correctness without known test vectors

---

### Category E: Comparison & Ordering (Generic Operators)

#### Operation 10: Multi-Type Ordering (less-than, greater-than)

**Signatures:**

```lisp
(less-than x y)    → boolean
(greater-than x y) → boolean
```

**Semantics:**

Implements total ordering across mixed types:

```lisp
(less-than 1 2)                              → t
(less-than "abc" "def")                      → t
(less-than :symbol "string")                 → t
(less-than +min-sentinel+ 1)                 → t
(less-than 1 +max-sentinel+)                 → t
(less-than '(1 2) '(2 3))                    → t
(less-than '(1 2) '(1 3))                    → t
```

**Ordering Rules (simplified):**

```
+min-sentinel+ < everything < +max-sentinel+

Within types:
  nil < t                     (booleans)
  numbers < symbols < strings (cross-type)
  By value within type:
    numbers: < (numeric)
    symbols: by name (string<)
    strings: lexicographic (string<)
    lists: car-based, then cdr-recursive

Special types:
  uuid:uuid, timestamp: string comparison of print representation
```

**Behavior:**
- Dispatches to appropriate method based on both argument types
- 30+ methods handling all combinations
- Recursive for lists (compare head, then tail if equal)

**Guarantee:**
- Returns consistent ordering
- Handles mixed types
- Sentinels always at extremes

**Performance:**
- Time: O(1) per comparison (dispatch)
- Time: O(n) for lists (recursive cdr traversal)
- Space: O(1)

**Common patterns:**

```lisp
; Pattern A: Sort mixed-type list
(sort '(3 :symbol "string" 1.5) #'less-than)
→ (1.5 3 :symbol "string")

; Pattern B: Skip-list index ordering
(define-skip-list vertex-index
  :key-comparator #'less-than)

; Pattern C: Range query
(defun range-query (index min-key max-key)
  (loop for item in (index-items index)
        when (and (less-than min-key (key item))
                  (less-than (key item) max-key))
        collect item))
```

**Risks:**
- ⚠️ Complex logic (30+ methods); easy to miss edge cases
- ⚠️ Ordering not verified to be transitive for all type combinations
- ⚠️ No docstring explaining rationale

---

#### Operation 11: Byte-Array Comparison (key-vector<, key-vector<=, key-vector>)

**Signatures:**

```lisp
(key-vector< v1 v2)  → boolean
(key-vector<= v1 v2) → boolean
(key-vector> v1 v2)  → boolean
```

**Semantics:**

```lisp
(key-vector< #(1 2 3) #(2 0 0))
→ t  (first byte 1 < 2)

(key-vector< #(1 2 3) #(1 3 0))
→ t  (first byte equal, second byte 2 < 3)

(key-vector< #(1 2 3) #(1 2 3))
→ nil  (equal arrays)
```

**Behavior:**
- Compares byte-arrays lexicographically
- Uses recursive subseq (inefficient!)
- Stops at first differing byte

**Guarantee:**
- Returns correct comparison result

**Performance:**
- Time: O(n²) worst case! (subseq creates copies, O(n) × recursive depth O(n))
- Space: O(n²) worst case (copies of subarrays)

**Risks:**
- ⚠️ **PERFORMANCE BUG:** O(n²) is unacceptable for large keys
- ⚠️ Should use index-based recursion (O(n) time, O(n) space)

---

### Category F: Locking & Synchronization (Cross-Lisp)

#### Operation 12: Acquire Lock (with-lock macro)

**Signature:**

```lisp
(with-lock (lock &key whostate timeout) &body body)
→ (values ...)  ; result of body
```

**Semantics:**

```lisp
(let ((my-lock (make-lock)))
  (with-lock (my-lock :whostate "working" :timeout 5)
    (critical-section)))
```

**Behavior (conditional per Lisp):**

```
#+ccl
  └─ Tries to acquire lock (with optional timeout)
     ├─ If timeout: waits with timeout, then executes body
     └─ Returns body result or nil if timeout

#+sbcl
  └─ Uses recursive lock (can be acquired multiple times by same thread)
     └─ Timeout parameter ignored
     └─ Returns body result

#+lispworks
  └─ Uses mp:with-lock
     └─ Timeout parameter ignored
     └─ Returns body result
```

**Guarantee:**
- Mutual exclusion (lock held while body executes)
- Unwind-protect ensures lock released (even on exception)

**Performance:**
- Time: O(1) lock acquire/release
- Space: O(1)

**Common patterns:**

```lisp
; Pattern A: Protect shared state
(let ((counter 0)
      (counter-lock (make-lock)))
  (with-lock (counter-lock)
    (incf counter)))

; Pattern B: Timeout on lock acquisition
(with-lock (graph-lock :timeout 5)
  (update-graph))  ;; May return nil if timeout

; Pattern C: Nested locks (risky!)
(with-lock (lock1)
  (with-lock (lock2)
    (mutual-operation)))
```

**Risks:**
- ⚠️ **CRITICAL:** Inconsistent semantics across Lisps
  - CCL supports timeout, SBCL/LW don't
  - SBCL recursive, CCL non-recursive
  - If timeout, may return nil (caller must check)
- ⚠️ Nested locks can deadlock (lock ordering required)
- ⚠️ No timeout on SBCL/LW (different behavior from CCL)

---

#### Operation 13: Lock Hash-Table (with-locked-hash-table macro)

**Signature:**

```lisp
(with-locked-hash-table (table) &body body)
→ (values ...)  ; result of body
```

**Semantics:**

```lisp
(let ((table (make-hash-table)))
  (with-locked-hash-table (table)
    (gethash key table)))  ; Safe concurrent access (maybe)
```

**Behavior (conditional per Lisp):**

```
#+sbcl
  └─ Calls sb-ext:with-locked-hash-table
     └─ Actually locks the hash-table

#+ccl
  └─ Just (progn ,@body)
     └─ NO LOCKING! ❌ BUG

#+lispworks
  └─ Just (progn ,@body)
     └─ NO LOCKING! ❌ BUG
```

**Guarantee:**
- ✅ SBCL: Mutual exclusion on hash-table
- ❌ CCL/LW: NO GUARANTEE — not thread-safe!

**Performance:**
- Time: O(1) lock acquire/release (SBCL only)
- Space: O(1)

**Risks:**
- 🔴 **BLOCKING BUG:** CCL and LispWorks do NOT lock!
- 🔴 Concurrent hash-table access on CCL/LW may corrupt data
- 🔴 **SILENT FAILURE:** No warning; just corrupts data

---

#### Operation 14: Make Semaphore (make-semaphore)

**Signature:**

```lisp
(make-semaphore) → semaphore object
```

**Behavior (per Lisp):**

```
#+sbcl → (sb-thread:make-semaphore)
#+ccl  → (ccl:make-semaphore)
#+lispworks → (mp:make-semaphore)
```

**Guarantee:**
- Creates semaphore object for synchronization

**Performance:**
- Time: O(1)
- Space: O(1)

---

#### Operation 15: Read/Write Locking (make-rw-lock, acquire-write-lock, release-write-lock)

**Signatures:**

```lisp
(make-rw-lock)                          → rw-lock
(rw-lock-p thing)                       → boolean
(acquire-write-lock lock &key wait-p)   → lock or nil
(release-write-lock lock)                → nil
```

**Behavior (CCL-only):**

```
#+ccl
  ├─ (make-rw-lock) → creates reader-writer lock
  ├─ (rw-lock-p x) → tests if RW-lock
  ├─ (acquire-write-lock lock) → acquires write lock
  │   └─ If wait-p: wait indefinitely; else try once
  └─ (release-write-lock lock) → releases write lock

Other Lisps: NOT DEFINED
```

**Risk:**
- 🔴 **BUG at line 482:** (declare (ignore wait-p)) but wait-p not in parameters
- ⚠️ CCL-only; SBCL/LW cannot use

---

### Category G: Utility Helpers

#### Operation 16: Symbol Creation (new-interned-symbol)

**Signature:**

```lisp
(new-interned-symbol &rest args) → symbol
```

**Semantics:**

```lisp
(new-interned-symbol "foo" "-" "bar")
→ |FOO-BAR|  (interned symbol)
```

**Behavior:**
- Concatenates all args with format
- Interns result to symbol table

**Guarantee:**
- Returns symbol
- All matching symbols point to same object (interned)

**Risk:**
- ⚠️ Interned symbols persist in memory (cannot be garbage collected)
- ⚠️ Repeated calls create new symbols (memory leak if overused)

---

#### Operation 17: Memory Availability (free-memory)

**Signature:**

```lisp
(free-memory) → bytes (integer)
```

**Behavior (per Lisp):**

```
#+sbcl
  → (- (sb-kernel::dynamic-space-size) (sb-kernel:dynamic-usage))

#+ccl
  → (ccl::%freebytes)

#+lispworks
  → NOT IMPLEMENTED (returns undefined)
```

**Risk:**
- ⚠️ LispWorks not implemented (returns undefined)

---

#### Operation 18: Hash Functions (djb-hash, fast-djb-hash)

**Signatures:**

```lisp
(djb-hash seq)       → integer
(fast-djb-hash seq)  → integer
```

**Status:** MARKED "Not used" in comments

**Behavior:**
- djb-hash: DJB2 hash variant with type conversion
- fast-djb-hash: Simplified version (assumes seq of integers)

**Risk:**
- ⚠️ Why included if unused? Dead code or planned for future?

---

#### Operation 19: List Validation (proper-listp)

**Signature:**

```lisp
(proper-listp x) → boolean
```

**Semantics:**

```lisp
(proper-listp '(1 2 3))    → t
(proper-listp '(1 . 2))    → nil  (dotted)
(proper-listp 'symbol)     → nil
(proper-listp nil)         → t
```

**Behavior:**
- Recursively checks for proper list structure (non-dotted)

**Performance:**
- Time: O(n) where n = list length (must traverse to end)

---

#### Operation 20: Byte-Vector Creation (make-byte-vector)

**Signature:**

```lisp
(make-byte-vector length) → byte-array
```

**Semantics:**

```lisp
(make-byte-vector 16)
→ #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
```

**Behavior:**
- Creates zero-initialized byte array

**Performance:**
- Time: O(n) where n = length (allocation)
- Space: O(n)

---

## Variants / Specializations

**utilities.lisp has minimal specialization:**

- ✓ **Functions:** Mostly standalone, no inheritance
- ✓ **Generics:** less-than/greater-than with 30+ method combinations
- ✓ **Macros:** Conditional per Lisp (#+sbcl, #+ccl, #+lispworks)

**Most important variants:** Cross-Lisp implementations of with-lock, with-locked-hash-table, gettimeofday

---

## Usage Patterns

### Pattern 1: Cross-Lisp Code with Abstractions

**Context:** Code must work on SBCL, CCL, and LispWorks without #+ conditionals

**Example:**

```lisp
(defun critical-operation (graph data)
  ;; Acquire lock (works on all 3 Lisps)
  (with-lock (graph-lock :whostate "updating")
    ;; Protect hash-table access (BROKEN on CCL/LW!)
    (with-locked-hash-table (graph-vertices)
      ;; Do operation
      (update-vertex graph vertex-id data))))
```

**Pattern:** Use provided macros instead of Lisp-specific APIs

**Risk:** ⚠️ with-locked-hash-table silently fails on CCL/LW

---

### Pattern 2: Utility Functions for Code Reuse

**Context:** Common operations used throughout codebase

**Example:**

```lisp
(defun search-graph (graph predicate)
  ;; Flatten query results
  (flatten
    (mapcar (lambda (vertex)
              (find-if-anywhere predicate (vertex-data vertex)))
            (all-vertices graph))))
```

**Pattern:** Reuse utilities instead of reimplementing

---

### Pattern 3: Generic Operators for Flexible Indexing

**Context:** Skip-list indexes need to handle any key type

**Example:**

```lisp
(defun create-index (graph)
  (make-skip-list
    :comparator #'less-than  ;; Works with any type!
    :initial-items
    (mapcar (lambda (v)
              (cons (vertex-id v) v))
            (all-vertices graph))))
```

**Pattern:** Use less-than/greater-than for mixed-type sorting

---

## Performance Characteristics

### Big O Summary

Function | Time | Space | Notes
----------|------|-------|-------
dbg | O(n) | O(1) | Format string processing
gettimeofday | O(1) | O(1) | System call
get-unix-time | O(1) | O(1) | Conversion
flatten | O(n) | O(n) | Traverse entire structure
find-all | O(n) | O(n) | Linear search
find-anywhere | O(n) | O(1) | Tree search (no copy)
unique-find-anywhere-if | O(n log n) | O(n) | Dedup with adjoin
less-than | O(1) | O(1) | Generic dispatch (usually)
less-than (lists) | O(n) | O(1) | Recursive cdr comparison
key-vector< | O(n²) | O(n²) | **PERFORMANCE BUG** (subseq)
with-lock | O(1) | O(1) | Lock acquire/release
make-byte-vector | O(n) | O(n) | Allocation
gen-id | O(1) | O(1) | UUID generation
read-uuid-from-string | O(32) | O(1) | Parse 32 hex chars

**Critical Performance Issue:**

```
key-vector<, key-vector<=, key-vector> use recursive subseq:

  (defun key-vector< (v1 v2)
    (cond ((= (array-dimension v1 0) 0) nil)
          ((< (aref v1 0) (aref v2 0)) t)
          ((= (aref v1 0) (aref v2 0))
           (key-vector< (subseq v1 1) (subseq v2 1)))  ← O(n) copy!
          (t nil)))

Problem: Each recursive call creates new subseq (O(n) copy)
Depth: O(n) in worst case
Total: O(n) × O(n) = O(n²)

Better: Index-based recursion
  (defun key-vector< (v1 v2 &optional (i 0))
    ...)
→ O(n) time, O(n) space (just recursion)
```

---

## Constraints & Safety

### Safe Operations

| Operation | Safe? | Reason |
|-----------|-------|--------|
| dbg | ✅ | No shared state; I/O only |
| flatten | ✅ | Non-destructive; returns new list |
| find-* | ✅ | Non-destructive; read-only |
| less-than | ✅ | Pure function; no side effects |
| with-lock | ✅ | Mutual exclusion guaranteed (SBCL/CCL/LW) |
| gen-id | ✅ | Independent UUID generation |
| with-locked-hash-table (SBCL) | ✅ | Actual locking |

---

### Unsafe Operations

| Operation | Safe? | Reason | Risk |
|-----------|-------|--------|------|
| with-locked-hash-table (CCL/LW) | ❌ | No actual locking | Data corruption |
| with-lock timeout (SBCL/LW) | ⚠️ | Timeout parameter ignored | Different behavior per Lisp |
| read-id-array-from-string | ⚠️ | Complex endianness | Off-by-one errors possible |
| release-write-lock | ❌ | Undefined parameter in declare | Compile warning |
| key-vector< | ⚠️ | O(n²) performance | Slow for large keys |

---

### Recommended Patterns

| Pattern | Rationale |
|---------|-----------|
| Use with-lock for mutual exclusion | Works across all Lisps |
| Use utilities instead of reimplementing | DRY principle; tested |
| Use less-than for generic ordering | Flexible; handles mixed types |
| Avoid with-locked-hash-table on CCL/LW | Not thread-safe (use manual locks instead) |
| Test key-vector< performance with large keys | O(n²) may be problematic |

---

## Edge Cases & Gotchas

### Gotcha 1: with-locked-hash-table is NOT Thread-Safe on CCL/LW

**Problem:**

```lisp
#+ccl
(with-locked-hash-table (table)
  ,@body)
↓
(progn ,@body)  ;; NO LOCK!
```

**Risk:** Multiple threads modifying table simultaneously = data corruption

**Fix:** Use manual locking with with-lock

```lisp
(with-lock (table-lock)
  (with-locked-hash-table (table)
    (operate-on-table)))
```

---

### Gotcha 2: with-lock Timeout Only Works on CCL

**Problem:**

```lisp
#+sbcl
(with-lock (lock :timeout 5)  ;; Timeout ignored!
  (operation))

#+ccl
(with-lock (lock :timeout 5)  ;; Timeout works!
  (operation))
```

**Risk:** Code behaves differently on different Lisps

**Fix:** Document expected behavior per Lisp or use polling

---

### Gotcha 3: key-vector< Performance Cliff

**Problem:**

```lisp
;; Small keys: fast
(key-vector< #(1 2) #(2 1))  ← O(n²) where n=2: ~4 ops

;; Large keys: slow
(key-vector< (make-array 1000) (make-array 1000))
  ← O(n²) where n=1000: ~1,000,000 ops!
```

**Risk:** Queries on large keys slow down dramatically

**Fix:** Rewrite with index-based recursion (O(n) time)

---

### Gotcha 4: Interned Symbols Don't Get Garbage Collected

**Problem:**

```lisp
(loop for i from 1 to 1000000
      do (new-interned-symbol "foo" i))
  → 1,000,000 symbols interned into memory
  → Never garbage collected (part of symbol table)
  → Memory leak!
```

**Risk:** Repeated symbol creation exhausts memory

**Fix:** Reuse symbols or avoid dynamic symbol creation

---

### Gotcha 5: less-than Ordering May Not Be Transitive

**Problem:**

For mixed types, transitivity not guaranteed:

```lisp
; Hypothetically (if implementation is buggy):
(less-than 1 :symbol)        → t
(less-than :symbol "string") → t
(less-than 1 "string")       → nil  ;; Breaks transitivity!
```

**Risk:** Skip-list ordering incorrect

**Fix:** Verify ordering is transitive (need formal proof)

---

## Integration Context

### Upstream Dependencies

| Dependency | Purpose |
|-----------|---------|
| `:graph-db` package | Package context |
| `uuid` library | UUID generation/parsing |
| `timestamp` library | Timestamp comparison |
| Lisp standard libraries | Threading, FFI, system calls |
| globals.lisp | +min-sentinel+, +max-sentinel+ |

---

### Downstream Usage

| Consumer | What uses | Layer |
|----------|-----------|-------|
| package.lisp | Exports utility functions | Layer 1 |
| Layer 2-7 | Uses all utilities (locking, list ops, time) | Layer 2-7 |
| User code | May use utilities directly | External |

---

### Layer Position

**Layer 1 (Infrastructure):** utilities.lisp is core foundational utilities used by everything above

---

## When to Use What

### Decision Table: Which Utility Function?

| Need | Function | Why |
|------|----------|-----|
| Debug output | dbg | Simple, formatted print to stdout |
| Measure time | gettimeofday, get-unix-time | System time with microsecond precision |
| Flatten nested structure | flatten | Recursive descent, clean results |
| Search in list | find-all | Find matching elements |
| Search in tree | find-anywhere | Recursive tree search |
| Generate ID | gen-id | Random UUID |
| Parse UUID from string | read-uuid-from-string | Convert to uuid object |
| Compare mixed types | less-than, greater-than | Generic ordering |
| Mutual exclusion | with-lock | Thread-safe locking |
| Hash-table locking | with-locked-hash-table | Protect hash-table (SBCL only!) |

---

## Summary

| Aspect | Status | Details |
|--------|--------|---------|
| **Completeness** | 🟡 Partial | 50+ functions; LW support incomplete |
| **Consistency** | 🟡 Mixed | APIs uniform except lock semantics |
| **Correctness** | 🔴 BROKEN | 2 blocking issues, 2 critical issues |
| **Performance** | 🟠 ISSUES | key-vector< O(n²); needs optimization |
| **Thread-safety** | 🔴 BROKEN | with-locked-hash-table not thread-safe on CCL/LW |
| **Documentation** | 🔴 POOR | Only 3/50+ functions have docstrings |
| **Cross-Lisp support** | 🟡 INCOMPLETE | Missing LW implementations |
| **Extensibility** | ✅ GOOD | Easy to add utilities/generic methods |

---

### Key Insights

1. **utilities.lisp is foundational.** All layers depend on it; bugs here cascade everywhere.

2. **Thread-safety is broken.** with-locked-hash-table does NOT lock on CCL/LW; silent data corruption possible.

3. **Performance issues exist.** key-vector< O(n²) unacceptable for large keys.

4. **Documentation missing.** No docstrings; developers must read code to understand usage.

5. **Cross-Lisp abstractions mostly work.** But inconsistent semantics (timeout only CCL, recursive lock only SBCL).

6. **Generic operators are complex.** 30+ methods; ordering correctness unverified.

7. **Unused functions present.** djb-hash, fast-djb-hash marked "Not used"; dead code?

8. **Copy-paste errors detected.** release-write-lock references undefined parameter.

---

### Critical Decisions Before Phase 3

**MUST fix:**

1. ☐ **Fix with-locked-hash-table** (CCL/LW) — Implement actual locking or document limitation
2. ☐ **Fix release-write-lock** — Remove erroneous declare or correct signature
3. ☐ **Optimize key-vector<** — Rewrite with index-based recursion (O(n) time)
4. ☐ **Verify less-than ordering** — Prove transitivity for all type combinations

**SHOULD fix:**

5. ☐ **Complete LW support** — Implement free-memory for LispWorks
6. ☐ **Document lock semantics** — Explain per-Lisp behavior (timeout, recursion)
7. ☐ **Add docstrings** — Document all 50+ functions

**NICE to have:**

8. ☐ **Remove unused functions** — Delete djb-hash, fast-djb-hash if truly unused
9. ☐ **Simplify find-all** — Remove confusing complement logic
10. ☐ **Create utility tests** — Verify correctness of all functions

---

**Status:** Specification complete.  
**Blocking issues identified:** 2 (must fix before production)  
**Critical issues identified:** 2 (should fix before Phase 3)  
**Design improvements recommended:** 6+ (docstrings, testing, performance)  
**Ready for Nivel 3 (Docstrings)?** YES — Can proceed, but note blocking issues.  
**Ready for higher layers to use?** 🟡 PARTIAL — Works but risky for concurrent operations.  
**Next action:** Create layer1-utilities-docstrings.lisp (Nivel 3) with exhaustive annotations.

