# VivaceGraph - Layer 5: Indexing

## Table of Contents

1. [Overview](#overview)
2. [Purpose and Responsibilities](#purpose-and-responsibilities)
3. [Layer 5 Components](#layer-5-components)
4. [Detailed Files](#detailed-files)
5. [Specialized Indexes](#specialized-indexes)
6. [Prolog Engine](#prolog-engine)
7. [Materialized Views](#materialized-views)
8. [Load Order](#load-order)

## Overview

**Layer 5** provides **specialized indexes** and a **query engine** that enable:

- **VE-Index:** Fast Vertex→[Edges] lookup
- **VEV-Index:** Fast Vertex→Edge→Vertex lookup
- **Type-Index:** Indexes by node type
- **Prolog Functors:** Predicates for complex queries
- **Views:** Materialized views with map-reduce
- **Prolog Engine:** Full Prolog evaluator

### Key Features

- ✓ **VE/VEV Indexes:** O(log n) edge search
- ✓ **Type-Index:** O(1) access by type
- ✓ **Prolog:** Full logic programming, unification
- ✓ **Views:** Materialized map-reduce
- ✓ **Cache:** Multi-level with weak references
- ✓ **Thread-safe:** Per-index/view locks

### Lines of Code

```
File                       Lines
──────────────────────────────────
ve-index.lisp               194
vev-index.lisp              219
type-index.lisp              70
functor.lisp                 64
views.lisp                  732
prologc.lisp                717
──────────────────────────────────
TOTAL                      1996 lines
```

## Purpose and Responsibilities

### Why does Layer 5 exist?

VivaceGraph needs efficient queries:
1. **"Give me all outgoing edges of V"** → VE-Index
2. **"Give me the edge V1→type→V2"** → VEV-Index
3. **"Give me all vertices of type X"** → Type-Index
4. **"Find paths where..."** → Prolog
5. **"Complex queries with map-reduce"** → Views

### Specific Responsibilities

| Responsibility | File | Reason |
|----------------|------|--------|
| Vertex-edge index | `ve-index.lisp` | O(log n) outgoing edge access |
| Vertex-edge-vertex index | `vev-index.lisp` | O(log n) specific edge access |
| Type-based index | `type-index.lisp` | O(1) node access by type |
| Prolog predicates | `functor.lisp` | Define and compile functors |
| Materialized views | `views.lisp` | Map-reduce, result caches |
| Prolog evaluator | `prologc.lisp` | Full resolution engine |

## Layer 5 Components

### Dependency Diagram

```
LAYER 5 - INTERNAL DEPENDENCIES
================================

From LAYER 4 (skip-list, linear-hash, index-list):
    ↓
ve-index.lisp .............. VE-Index
    ├─ Depends: linear-hash (Layer 4)
    ├─ Uses: ve-key, ve-key-equal
    └─ Defines: ve-index, lookup-ve
         ↓
    vev-index.lisp ......... VEV-Index
         ├─ Depends: linear-hash (Layer 4)
         ├─ Uses: vev-key
         └─ Defines: vev-index, lookup-vev
              ↓
    type-index.lisp ........ Type-Index
         ├─ Depends: index-list (Layer 4)
         └─ Defines: type-index, type-lookup
              ↓
    functor.lisp ........... Prolog Functors
         ├─ Depends: prologc (below)
         └─ Defines: functor, prolog-compile
              ↓
    prologc.lisp ........... Prolog Engine
         ├─ Depends: serialize (Layer 4)
         └─ Defines: var, unify, prove, clauses
              ↓
    views.lisp ............. Views
         ├─ Depends: skip-list (Layer 4)
         ├─ Uses: functor, prologc
         └─ Defines: view, map-reduce, regenerate
```

## Detailed Files

### 1. `ve-index.lisp` (194 lines)

**Purpose:** **Vertex-Edge** index - quickly find outgoing/incoming edges of a vertex.

**Why?**

```
WITHOUT INDEX:
  lookup-outgoing-edges(V)
    → Traverse ALL edges
    → O(E) - catastrophic in dense graphs

WITH VE-INDEX:
  lookup-outgoing-edges(V)
    → Hash lookup + skip-list search
    → O(log E_V) where E_V = edges of V
```

#### 1.1 VE-Key Structure

```lisp
(defstruct (ve-key
             (:print-function ...))
  (id +null-key+ :type (simple-array (unsigned-byte 8) (16)))  ; Vertex UUID
  (type-id 0 :type (integer 0 65535)))                         ; Edge type
```

**Memory Layout:**

```
VE-Key: [16 bytes UUID] [2 bytes type-id]
        ├─ Vertex ID (address)
        └─ Edge type (for multiple relationships)

Example:
  VE-Key { V1, :friend }
  → All :friend outgoing edges from V1
```

#### 1.2 VE-Index Structure

```lisp
(defstruct (ve-index
             (:constructor %make-ve-index))
  table                           ; Linear-hash (Layer 4)
  (cache (make-ve-cache)))        ; Weak cache (weak references)
```

**What it contains:**

```
ve-index.table: Linear-hash
  Key: VE-Key { V1, :friend }
  Value: index-list of edges
         → [E1, E2, E3, ...]
         
Example:
  hash[ VE-Key {V_alice, :friend} ] 
    → index-list [E_alice→bob, E_alice→charlie, E_alice→diana]
```

#### 1.3 Operations

```lisp
(defun make-ve-index (location)
  "Create new VE-Index"
  (let* ((idx (make-lhash 
               :test 've-key-equal
               :location location
               :value-bytes +index-list-bytes+
               :key-bytes +ve-key-bytes+
               :null-key *ve-null-key*
               :bucket-size 24
               :buckets (expt 2 16))))
    (%make-ve-index :table idx)))

(defmethod lookup-ve-in-index-list ((key ve-key) (graph graph))
  "Search edges for VE-Key, O(log n) average"
  (or (gethash key (ve-index-cache (ve-index-in graph)))
      (let ((table (ve-index-table (ve-index-in graph))))
        (with-locked-hash-key (table key)
          (let ((il (lhash-get table key)))
            (when il
              (cache-index-list (ve-index-in graph) key il)
              il))))))

(defmethod add-to-ve-index ((v vertex) (e edge) (graph graph))
  "Add edge to VE-Index"
  ;; Entry: VE-Key { from(e), type(e) }
  ;;        → index-list with e
  (let ((ve-key (make-ve-key :id (id v) :type-id (edge-type-id e))))
    (or (lookup-ve-in-index-list ve-key graph)
        (let ((il (make-index-list)))
          (cache-index-list (ve-index-in graph) ve-key il)
          il))))
```

**Search Flow:**

```
lookup-outgoing-edges(alice, :friend)
    ↓
Create VE-Key { alice_id, :friend }
    ↓
Hash lookup in linear-hash
    ↓
Found in cache?
    ├─ Yes: return immediately
    └─ No: search in linear-hash
    ↓
Return index-list of edges
    ↓
Traverse with cursor (O(1) per edge)
```

### 2. `vev-index.lisp` (219 lines)

**Purpose:** **Vertex-Edge-Vertex** index - quickly find specific edges V1→type→V2.

#### 2.1 VEV-Key Structure

```lisp
(defstruct (vev-key
             (:print-function ...))
  (out-id +null-key+ :type (simple-array (unsigned-byte 8) (16)))  ; Source vertex UUID
  (in-id +null-key+ :type (simple-array (unsigned-byte 8) (16)))   ; Destination vertex UUID
  (type-id 0 :type (integer 0 65535)))                             ; Edge type
```

**Memory Layout:**

```
VEV-Key: [16 bytes out-id] [16 bytes in-id] [2 bytes type-id]
        ├─ Source vertex
        ├─ Destination vertex
        └─ Edge type (relationship)

Example:
  VEV-Key { alice_id, bob_id, :friend }
  → Edge alice :friend→ bob (if it exists)
```

#### 2.2 VEV-Index Structure

```lisp
(defstruct (vev-index
             (:constructor %make-vev-index))
  table                           ; Linear-hash
  (cache (make-vev-cache)))       ; Weak cache
```

#### 2.3 Operations

```lisp
(defun make-vev-index (location)
  "Create VEV-Index"
  (let* ((idx (make-lhash 
               :test 'vev-key-equal
               :location location
               :value-bytes +index-list-bytes+
               :key-bytes +vev-key-bytes+
               :null-key *vev-null-key*
               :bucket-size 24)))
    (%make-vev-index :table idx)))

(defmethod lookup-edge ((from vertex) (to vertex) (type symbol) 
                        (graph graph))
  "Search for specific edge, O(log n) average"
  (let ((vev-key (make-vev-key 
                  :out-id (id from)
                  :in-id (id to)
                  :type-id (get-type-id type))))
    (lhash-get (vev-index-table (vev-index graph)) vev-key)))

(defmethod add-to-vev-index ((e edge) (graph graph))
  "Register edge in VEV-Index"
  (let ((vev-key (make-vev-key 
                  :out-id (id (from e))
                  :in-id (id (to e))
                  :type-id (edge-type-id e))))
    (lhash-put (vev-index-table (vev-index graph)) 
               vev-key 
               e)))
```

**Advantage:**

```
Queries:
  "Is there an edge alice :friend→ bob?"
    → Hash lookup O(1) average
    → Without traversing all edges
```

### 3. `type-index.lisp` (70 lines)

**Purpose:** **Type-based** index - fast node access by type.

#### 3.1 Type-Index Structure

```lisp
(defstruct (type-index
             (:constructor %make-type-index))
  table                           ; Memory-mapped file
  (locks (map-into (make-array +max-node-types+)
                   'make-lock))  ; Per-type lock
  (cache ...)                     ; Cache hash table
```

**Layout:**

```
Memory-mapped file with +max-node-types+ slices
each slice = index-list for a type

Type-Index:
  ├─ Type 0 (user):    [V1, V3, V5, ...]
  ├─ Type 1 (company): [V2, V4, V6, ...]
  ├─ Type 2 (post):    [V7, V8, V9, ...]
  └─ ...
```

#### 3.2 Operations

```lisp
(defun make-type-index (location heap)
  "Create Type-Index with +max-node-types+ slots"
  (let* ((table (mmap-file location 
                           :size (* +max-node-types+ +index-list-bytes+)))
         (idx (%make-type-index :table table)))
    ;; Initialize each type with empty index-list
    (dotimes (i +max-node-types+)
      (let ((offset (* i +index-list-bytes+)))
        (let ((index-list (make-index-list heap)))
          (serialize-index-list table index-list offset)
          (setf (gethash i (type-index-cache idx)) index-list))))
    idx))

(defmethod type-index-push ((uuid array) (type-id integer) 
                            (idx type-index) &key unless-present)
  "Add node to type-index"
  (let ((lock (aref (type-index-locks idx) type-id)))
    (with-lock (lock)
      (let ((il (gethash type-id (type-index-cache idx))))
        (if unless-present
            (index-list-pushnew uuid il)  ; Only if not present
            (index-list-push uuid il))    ; Always add
        ;; Serialize changes to disk
        (serialize-index-list (type-index-table idx)
                              il
                              (* type-id +index-list-bytes+))))))

(defmethod get-all-of-type ((type-id integer) (graph graph))
  "Get all nodes of type, O(1) access"
  (gethash type-id (type-index-cache (type-index graph))))
```

**Usage:**

```lisp
;; Retrieve all users
(let ((users (get-all-of-type :user graph)))
  (map-index-list #'process-user users))
```

### 4. `functor.lisp` (64 lines)

**Purpose:** Define **Prolog functors** - compiled predicates for queries.

**Content:**

```lisp
(defstruct (functor
             (:constructor %make-functor)
             (:predicate functor-p))
  name                    ; Functor name (symbol)
  fn                      ; Compiled function
  clauses                 ; Prolog clauses
  (lock (make-recursive-lock)))

(defvar *user-functors*
  "Hash table of user-defined functors"
  (make-hash-table :synchronized t))

(defun make-functor (&key name clauses)
  "Define new functor"
  (or (lookup-functor name)
      (let ((functor (%make-functor :name name :clauses clauses)))
        (with-recursive-lock-held ((functor-lock functor))
          ;; Compile to Lisp function
          (prog1
              (setf (gethash name *user-functors*) functor)
            (prolog-compile functor))))))

(defun add-functor-clause (functor clause)
  "Add clause to existing functor"
  (with-recursive-lock-held ((functor-lock functor))
    ;; Use atomic CAS to add
    #+sbcl
    (sb-ext:cas (cdr (last (functor-clauses functor)))
                (cdr (last (functor-clauses functor)))
                (list clause))
    
    ;; Recompile
    (prolog-compile functor)
    (functor-clauses functor)))

(defun delete-functor (functor)
  "Delete functor"
  (remhash (functor-name functor) *user-functors*))

(defun reset-functor (functor)
  "Clear all functor clauses"
  (with-recursive-lock-held ((functor-lock functor))
    (setf (functor-clauses functor) nil)
    (prolog-compile functor)))
```

**Usage Example:**

```lisp
;; Define friend/2 predicate
(make-functor 
  :name 'friend
  :clauses '(
    ;; Clause 1: Alice and Bob are friends
    ((friend alice bob))
    ;; Clause 2: Bob and Alice are friends
    ((friend bob alice))
    ;; Clause 3: If X and Y are friends, Y and X are friends (symmetry)
    ((friend ?X ?Y) (friend ?Y ?X))
    ))

;; Now I can make queries:
(prove '(friend alice bob))      ;; → T
(prove '(friend bob alice))      ;; → T
(prove '(friend charlie diana))  ;; → NIL
```

### 5. `views.lisp` (732 lines)

**Purpose:** **Materialized Views** - caches of complex queries with map-reduce.

**What is a view?**

```
VIEW = Cached query result that updates when data changes

Example:
  VIEW user-posts
    MAP: For each user, list of posts they created
    REDUCE: Sum, count, group, etc.
    
  Without view:
    Each query iterates ALL users → ALL posts → filter
    O(U * P) per query
    
  With view:
    Precomputed: user → [list of posts]
    O(1) lookup + O(n_posts) iterate
```

#### 5.1 View Structure

```lisp
(defstruct (view
             (:print-function ...))
  name                    ; View name
  class-name              ; Node class (e.g.: 'user)
  map-fn                  ; MAP function
  map-code                ; Compiled MAP code
  reduce-fn               ; REDUCE function
  reduce-code             ; Compiled REDUCE code
  graph-name              ; Reference graph
  heap                    ; Memory allocator
  pointer                 ; Heap address
  skip-list               ; Skip-list for results
  (lock (make-rw-lock))   ; RW-lock
  lookup-fn               ; Direct lookup function
  (sort-order :lessp))    ; Sort order
```

#### 5.2 View Group

```lisp
(defstruct view-group
  class-name              ; Class (e.g.: 'user)
  (table ...)             ; Hash table of views
  (lock (make-rw-lock)))  ; RW-lock for the group
```

#### 5.3 Operations

```lisp
(defmethod map-view (fn (class-name symbol) (view-name symbol)
                     &key (graph *graph*) key start-key end-key 
                       count skip collect-p)
  "Iterate over view"
  (if (lookup-view-group class-name graph)
      (let ((view (lookup-view graph class-name view-name)))
        (unless view
          (error 'invalid-view-error :class-name class-name))
        
        ;; Create cursor (possibly range)
        (let* ((cursor (if (and (null start-key) (null key) (null end-key))
                          (make-cursor (view-skip-list view))
                          (make-range-cursor (view-skip-list view)
                                           start-key end-key)))
               (result nil) (found-count 0))
          
          ;; Iterate and apply function
          (loop for node = (cursor-next cursor)
                while (and node (not (and count (= found-count count))))
                do
                (incf found-count)
                (if collect-p
                    (push (funcall fn (car (%sn-key node))
                                      (cdr (%sn-key node))
                                      (%sn-value node))
                          result)
                    (funcall fn (car (%sn-key node))
                            (cdr (%sn-key node))
                            (%sn-value node))))
          
          (when collect-p
            (values (nreverse result) found-count))))))

(defmethod regenerate-view ((graph graph) (class-name symbol) 
                            (view-name symbol))
  "Recalculate view (full rebuild)"
  ;; 1. Execute MAP over all nodes
  (let ((view (lookup-view graph class-name view-name))
        (*view-rv* nil))  ; Result vector
    
    ;; 2. For each node of this class
    (with-read-locked-view-group (class-name graph)
      (map-vertices 
       (lambda (node)
         (when (eq (type-of node) class-name)
           ;; 3. Execute map-fn
           (funcall (view-map-fn view) node)
           ;; 4. Results go to *view-rv*
           ))
       graph))
    
    ;; 5. Insert all into skip-list
    (dolist (result *view-rv*)
      (add-to-skip-list (view-skip-list view) 
                       (car result) (cdr result)))))

(defmethod restore-views ((graph graph))
  "Load views from disk on startup"
  (let ((views-file (format nil "~A/views.dat" (location graph))))
    (when (probe-file views-file)
      (let ((blob (cl-store:restore views-file)))
        (dolist (view-data blob)
          ;; Restore each view
          ...)))))
```

**Example:**

```lisp
;; Create view: "posts by user"
(defmethod make-user-posts-view ((graph graph))
  (make-view 
   :name 'by-user
   :class-name 'user
   :map-fn (lambda (user-node)
             ;; For each post by this user
             (let ((user-id (id user-node)))
               (map-edges
                (lambda (edge)
                  (when (eq (edge-type edge) :created)
                    (let ((post-id (to edge)))
                      ;; Yield (user-id, post-id) → post
                      (yield (list user-id post-id) post))))
                graph)))
   :graph-name (graph-name graph)))

;; Use view for fast searches:
(map-view (lambda (user-id post-id post)
            (format t "User ~A created post ~A~%" user-id post))
         'user 'by-user
         :start-key alice-id
         :end-key bob-id
         :limit 10)
```

### 6. `prologc.lisp` (717 lines)

**Purpose:** **Full Prolog engine** based on Norvig's PAIP (Paradigms of AI Programming).

**What is Prolog?**

```
Prolog = Logic programming + Backtracking + Unification

Example:

  Facts:
    (friend alice bob)
    (friend bob charlie)
    
  Rules:
    (mutual-friend ?X ?Y) :- 
      (friend ?X ?Y), 
      (friend ?Y ?X)
    
  Query:
    ?- mutual-friend alice bob
    
  Resolution:
    1. Search clauses that unify with mutual-friend
    2. Try to prove (friend alice bob)
    3. Search in database → MATCH
    4. Try to prove (friend bob alice)
    5. Search in database → MATCH
    6. ✓ SUCCESS
```

#### 6.1 Prolog Variables

```lisp
(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*))        ; Unique identifier
  (binding +unbound+))               ; Bound value

(defun bound-p (var) 
  "Is the variable bound?"
  (not (eq (var-binding var) +unbound+)))

(defmacro var-deref (exp)
  "Dereference variable (follow pointers)"
  `(progn 
     (loop while (and (var-p ,exp) (bound-p ,exp))
        do (setf ,exp (var-binding ,exp)))
     ,exp))
```

**Usage:**

```lisp
(let ((X (?)))       ; New variable _X1
  ;; Initially unbound
  (assert (not (bound-p X)))
  
  ;; Bind to value
  (unify X 'alice)
  (assert (bound-p X))
  
  ;; Dereference
  (assert (eq (var-deref X) 'alice)))
```

#### 6.2 Unification

```lisp
(defgeneric prolog-equal (x y)
  "Equal according to Prolog?"
  (:method ((x number) (y number)) (= x y))
  (:method ((x string) (y string)) (string= x y))
  (:method ((x node) (y node)) (equalp (id x) (id y)))
  (:method (x y) (equal x y)))

(defun unify (x y)
  "Unify two expressions (destructive)"
  (cond
    ;; Both dereferenced and equal
    ((prolog-equal (var-deref x) (var-deref y)) t)
    
    ;; X is variable: bind to Y
    ((var-p x) (set-binding x y))
    
    ;; Y is variable: bind to X
    ((var-p y) (set-binding y x))
    
    ;; Both lists: unify recursively
    ((and (consp x) (consp y))
     (and (unify (first x) (first y))
          (unify (rest x) (rest y))))
    
    ;; Don't unify
    (t nil)))

(defun set-binding (var value)
  "Bind variable, saving in trail for backtracking"
  (unless (eq var value)
    ;; Save in trail so we can undo
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)
```

**Example:**

```lisp
(let ((X (?))
      (Y (?)))
  ;; Unify X with 'alice'
  (unify X 'alice)
  ;; Unify Y with X (which is bound to 'alice')
  (unify Y X)
  ;; Now both point to 'alice'
  (assert (eq (var-deref Y) 'alice)))
```

#### 6.3 Backtracking and Trail

```lisp
(defvar *trail*
  "Variable vector for backtracking undo"
  (make-array 0 :fill-pointer 0 :adjustable t))

(defun undo-bindings (old-trail)
  "Undo bindings up to checkpoint"
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) +unbound+)))

(defun save-trail ()
  "Save backtracking point"
  (fill-pointer *trail*))
```

**Backtracking pattern:**

```lisp
(defun prove-goals (goals bindings depth)
  "Prove list of goals with backtracking"
  (if (null goals)
      ;; Base case: all goals successful
      (return-from prove-goals bindings)
      
      (let ((goal (first goals)))
        ;; Save backtracking point
        (let ((saved-trail (save-trail)))
          ;; Try each possible clause
          (dolist (clause (get-clauses goal))
            ;; Unify goal with clause
            (if (unify goal (clause-head clause))
                ;; Prove subgoals
                (let ((result (prove-goals 
                              (append (clause-body clause)
                                     (rest goals))
                              bindings
                              (1+ depth))))
                  (if result
                      (return-from prove-goals result)
                      ;; Backtrack: undo bindings
                      (undo-bindings saved-trail)))))))
        
        ;; Total failure: no applicable clauses
        nil)))
```

#### 6.4 Functor Compilation

```lisp
(defmethod prolog-compile ((functor functor))
  "Compile clauses to executable Lisp function"
  (if (null (functor-clauses functor))
      ;; No clauses: compile to function that always fails
      (prolog-compile-null functor)
      
      ;; Compile by arity
      (prolog-compile-help functor (functor-clauses functor))))

(defun compile-functor (functor arity clauses)
  "Generate Lisp function for specific functor/arity"
  (let ((params (make-parameters arity)))
    ;; Generate body that tries each clause
    (let ((body
           `(let ((old-trail (fill-pointer *trail*)))
              (or 
                ;; Try each clause
                ,@(mapcar (lambda (clause)
                            (compile-clause params clause))
                         clauses)
                ;; All failed: backtrack
                (progn
                  (undo-bindings old-trail)
                  nil)))))
      ;; Compile to function
      (compile functor (lambda (&rest args) 
                         (apply body args))))))
```

## Specialized Indexes

### Comparison

```
SEARCH                 WITHOUT INDEX  WITH INDEX
─────────────────────────────────────────────────
Edges of V?           O(E)          O(log E_V)
Edge V1→V2?           O(E)          O(log E_V)
Nodes of type X?      O(V)          O(1)
Path V1→V2            O(V*E)        O((V+E) log E)

E = total edges
V = total vertices
E_V = edges of V
```

## Prolog Engine

### Proof Pattern

```
PROVE { friend(alice, ?X) }
  ├─ Unify with friend(alice, bob)
  │  ├─ friend = friend ✓
  │  ├─ alice = alice ✓
  │  └─ ?X = bob ✓ (bind X to bob)
  └─ SUCCESS with ?X = bob

PROVE { friend(alice, ?X), likes(?X, pizza) }
  ├─ Prove friend(alice, ?X)
  │  └─ ?X = bob
  ├─ Prove likes(bob, pizza)
  │  ├─ Does clause likes(bob, pizza) exist?
  │  └─ Yes ✓
  └─ SUCCESS
```

### Trail for Backtracking

```
Initial: *trail* = []

Unify X = alice
  → *trail* = [X]

Unify Y = 1
  → *trail* = [X, Y]

Backtrack to checkpoint
  → Undo Y = unbound
  → Undo X = unbound
  → *trail* = []
```

## Materialized Views

### Map-Reduce Pattern

```
VIEW user-posts:

MAP FUNCTION:
  For each user U:
    For each edge E (created):
      Yield (U, post_id) → post

Example:
  User alice created:
    - "Hello World" (post_1)
    - "Goodbye" (post_2)
  
  User bob created:
    - "Hi" (post_3)
  
  Results:
    (alice, post_1) → post_1
    (alice, post_2) → post_2
    (bob, post_3) → post_3

REDUCE FUNCTION (optional):
  Group by user:
    alice → [post_1, post_2]
    bob → [post_3]
```

## Load Order

```
LOAD ORDER - LAYER 5
═══════════════════════════════════════════════════════════

From LAYER 4 (skip-list, linear-hash, index-list):
    ↓
ve-index.lisp .............. VE-Index
    ├─ Depends: linear-hash
    └─ Defines: ve-index, lookup-ve
         ↓
    vev-index.lisp ......... VEV-Index
         ├─ Depends: linear-hash
         └─ Defines: vev-index, lookup-vev
              ↓
    type-index.lisp ........ Type-Index
         ├─ Depends: index-list, mmap
         └─ Defines: type-index, get-all-of-type
              ↓
    functor.lisp ........... Prolog Functors
         ├─ Depends: prologc
         └─ Defines: functor, prolog-compile
              ↓
    prologc.lisp ........... Prolog Engine
         ├─ Depends: serialize (Layer 4)
         └─ Defines: var, unify, prove, trail
              ↓
    views.lisp ............. Views
         ├─ Depends: skip-list (Layer 4)
         ├─ Uses: functor, prologc
         └─ Defines: view, map-reduce, regenerate
```

## Summary

**Layer 5** provides:

1. ✓ **VE-Index (194 lines)** - Find outgoing edges O(log n)
2. ✓ **VEV-Index (219 lines)** - Find specific edge O(log n)
3. ✓ **Type-Index (70 lines)** - Access by type O(1)
4. ✓ **Functors (64 lines)** - Define Prolog predicates
5. ✓ **Views (732 lines)** - Materialized map-reduce views
6. ✓ **Prolog (717 lines)** - Full Prolog engine

**Total:** ~1,996 lines providing:
- Efficient graph queries
- Logic programming (Prolog)
- Precomputed views
- Unification and backtracking
- Multi-index for fast access

In the following layers (6-7), this indexing is used for high-level APIs.


*VivaceGraph Layer 5 Documentation*
*March 2026*
