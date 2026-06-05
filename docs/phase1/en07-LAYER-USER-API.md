# VivaceGraph - Layer 7: User API

## Table of Contents

1. [Overview](#overview)
2. [Purpose and Responsibilities](#purpose-and-responsibilities)
3. [Layer 7 Components](#layer-7-components)
4. [Detailed Files](#detailed-files)
5. [Complete REST API](#complete-rest-api)
6. [Graph Traversal](#graph-traversal)
7. [Authentication](#authentication)
8. [Usage Examples](#usage-examples)
9. [Load Order](#load-order)

## Overview

**Layer 7** is the **API layer** - the final interface that exposes all previous layers:

- **Interface:** Generics for CRUD operations
- **Traverse:** Graph traversal (BFS)
- **REST:** Complete HTTP server with authentication

### Key Features

- ✓ **REST HTTP server** with Ningle/Clack
- ✓ **Full CRUD** for vertices and edges
- ✓ **Authentication** with htpasswd
- ✓ **Automatic JSON encoding**
- ✓ **BFS Traversal** with depth limit
- ✓ **Schema introspection** via REST

### Lines of Code

```
File                       Lines
──────────────────────────────────
interface.lisp               27
traverse.lisp                81
rest.lisp                   409
──────────────────────────────────
TOTAL                       517 lines
```

## Purpose and Responsibilities

### Why does Layer 7 exist?

VivaceGraph needs to:
1. **Expose API to end users** (REST HTTP)
2. **Generic operations** (copy, save, delete)
3. **Traverse graphs** efficiently (BFS)
4. **Security** via authentication
5. **Automatic JSON serialization**

### Specific Responsibilities

| Responsibility | File | Reason |
|----------------|------|--------|
| CRUD generics | `interface.lisp` | copy, mark-deleted, save for all types |
| BFS traversal | `traverse.lisp` | Explore graph with depth limit |
| REST server | `rest.lisp` | HTTP endpoints, JSON, authentication |

## Layer 7 Components

### Dependency Diagram

```
LAYER 7 - INTERNAL DEPENDENCIES
================================

From LAYER 6 (vertex, edge, schema):
    ↓
interface.lisp .............. CRUD generics
    ├─ Depends: Layer 6 (vertex, edge)
    └─ Defines: copy, mark-deleted, save
         ↓
    traverse.lisp ........... BFS Traversal
         ├─ Depends: Layer 6 (edges, lookup)
         └─ Defines: traverse, traversal class
              ↓
    rest.lisp ............... HTTP REST server
         ├─ Depends: Ningle, Clack, Bordeaux-threads
         ├─ Uses: interface, traverse, schema
         └─ Defines: REST endpoints, JSON encoding
```

## Detailed Files

### 1. `interface.lisp` (27 lines)

**Purpose:** Generics for common operations on all node types.

#### 1.1 Content

```lisp
(defgeneric copy (node)
  "Create copy of node (for transactions)"
  (:method (thing)
    (error "Cannot save ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex))
    (copy-vertex vertex))
  (:method ((edge edge))
    (copy-edge edge)))

(defgeneric mark-deleted (node)
  "Mark node as deleted (soft delete)"
  (:method (thing)
    (error "Cannot delete ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex))
    (delete-vertex vertex))
  (:method ((edge edge))
    (delete-edge edge)))

(defgeneric save (object &key graph)
  "Save node changes to disk"
  (:method (thing &key graph)
    (declare (ignore graph))
    (error "Cannot save ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex) &key (graph *graph*))
    (update-node vertex graph))
  (:method ((edge edge) &key (graph *graph*))
    (update-node edge graph)))
```

**Purpose of each generic:**

```
copy:
  Create full copy of node (for editing in transaction)
  → read-only → local copy → edit → save
  
mark-deleted:
  Mark as deleted (soft delete)
  → Logical deletion (does not free space)
  → Reversible (can be undone)
  
save:
  Persist changes to disk
  → Inside transaction: automatic
  → Outside: manual
```

### 2. `traverse.lisp` (81 lines)

**Purpose:** Graph traversal using BFS (Breadth-First Search).

#### 2.1 Traversal Class

```lisp
(defclass traversal ()
  ((end-vertex :accessor end-vertex 
               :initarg :end-vertex :initform nil)
   (reverse-path :accessor reverse-path 
                 :initarg :path :initform nil)))

(defmethod traversal-path ((traversal traversal))
  "Get path in forward order"
  (reverse (reverse-path traversal)))

(defmethod depth ((traversal traversal))
  "Current traversal depth"
  (length (reverse-path traversal)))
```

**Structure:**

```
Traversal represents a path in the graph:
  
  ┌─────────────────────────────────────┐
  │ Traversal                           │
  ├─────────────────────────────────────┤
  │ end-vertex: V3                      │
  │ reverse-path: [E1, E2]              │
  │                                     │
  │ Interpretation:                     │
  │   V0 --E1--> V1 --E2--> V3          │
  │                                     │
  │ traversal-path: [E1, E2] (forward)  │
  │ depth: 2                            │
  └─────────────────────────────────────┘
```

#### 2.2 Operations

```lisp
(defun make-traversal (vertex path)
  "Create new traversal at vertex with path"
  (make-instance 'traversal
                 :end-vertex vertex
                 :path path))

(defmethod update-traversal ((traversal traversal) 
                              (vertex vertex) 
                              (edge edge))
  "Create new traversal extending a previous one"
  (let ((new-traversal
         (make-instance 'traversal
                        :end-vertex vertex
                        :path (copy-list (reverse-path traversal)))))
    (push edge (reverse-path new-traversal))
    new-traversal))

(defmethod traverse ((vertex vertex) 
                    &key (graph *graph*) 
                      (order :bfs)
                      (direction :both) 
                      (uniqueness :global)
                      edge-type 
                      max-depth 
                      return-paths)
  "Explore graph from vertex using BFS"
  
  ;; 1. Initialize queue with starting vertex
  (let ((queue (make-queue :elements
                           (list (make-instance 'traversal
                                              :end-vertex vertex))))
        (result-table (make-hash-table :test 'equalp))
        (memory (make-hash-table :test 'equalp)))  ; Visited
    
    ;; 2. BFS loop
    (loop until (empty-queue-p queue) do
         (let* ((traversal (dequeue queue))
                (current-vertex (end-vertex traversal)))
           
           ;; 3. Check depth
           (unless (and max-depth
                        (> (depth traversal) max-depth))
             
             ;; 4. Explore outgoing edges
             (when (or (eql direction :out) (eql direction :both))
               (map-edges (lambda (edge)
                            (let* ((next-vertex (lookup-vertex (to edge)))
                                   (new-traversal
                                    (update-traversal traversal
                                                      next-vertex
                                                      edge)))
                              
                              ;; Only if not visited (uniqueness: global)
                              (unless (gethash next-vertex memory)
                                (setf (gethash next-vertex memory) t)
                                (enqueue queue new-traversal))
                              
                              ;; Filter by edge-type
                              (when (typep edge edge-type)
                                (setf (gethash next-vertex result-table)
                                      new-traversal))))
                          graph
                          :vertex current-vertex
                          :direction :out))
             
             ;; 5. Explore incoming edges
             (when (or (eql direction :in) (eql direction :both))
               (map-edges (lambda (edge)
                            (let* ((prev-vertex (lookup-vertex (from edge)))
                                   (new-traversal
                                    (update-traversal traversal
                                                      prev-vertex
                                                      edge)))
                              (unless (gethash prev-vertex memory)
                                (setf (gethash prev-vertex memory) t)
                                (enqueue queue new-traversal))
                              (when (typep edge edge-type)
                                (setf (gethash prev-vertex result-table)
                                      new-traversal))))
                          graph
                          :vertex current-vertex
                          :direction :in))))
    
    ;; 6. Return vertices or paths
    (if return-paths
        (loop for p being the hash-values in result-table collecting p)
        (loop for v being the hash-keys in result-table collecting v))))
```

**Complexity:**

```
Time:    O(V + E) in worst case
         O(V^d) if max-depth = d
         
Space:   O(V) for queue + memory
```

**Usage Examples:**

```lisp
;; Find all friends (depth 1)
(traverse alice-vertex 
         :direction :out
         :edge-type 'friend
         :max-depth 1
         :return-paths nil)
→ (bob charlie diana)

;; Find friends of friends with paths
(traverse alice-vertex
         :direction :out
         :edge-type 'friend
         :max-depth 2
         :return-paths t)
→ (traversal[bob, [friend_edge1]]
   traversal[charlie, [friend_edge2]]
   traversal[eve, [friend_edge1, friend_edge3]])
```

### 3. `rest.lisp` (409 lines) - **REST SERVER**

**Purpose:** Complete HTTP REST server that exposes the entire VivaceGraph API.

#### 3.1 Setup and Authentication

```lisp
(defvar *rest-port* 8080)
(defvar *rest-app* nil)
(defvar *clack-app* nil)
(defvar *rest-procedures* 
  (make-hash-table :synchronized t :test 'equalp))
(defvar *rest-passwd-file* "rpasswd")
(defvar *htpasswd-bin* "/usr/bin/htpasswd")

(defun add-rest-user (username password)
  "Add user with htpasswd"
  (trivial-shell:shell-command
   (format nil "~A -b ~A ~A ~A ~A"
           *htpasswd-bin*
           (if (probe-file *rest-passwd-file*) "" "-c")
           *rest-passwd-file*
           username
           password)))

(defun auth-rest-user (username password)
  "Authenticate user against htpasswd"
  (with-open-file (stream *rest-passwd-file*)
    (do ((line (read-line stream nil :eof)
               (read-line stream nil :eof)))
        ((eql line :eof))
      (let ((pair (cl-ppcre:split "\\:" line)))
        (when (equalp (first pair) username)
          ;; Verify password with openssl
          (multiple-value-bind (out err exit-code)
              (trivial-shell:shell-command
               (format nil "openssl passwd -~A -salt ~A ~A"
                       hash salt password))
            (declare (ignore exit-code err))
            (return-from auth-rest-user
              (equalp (string-trim '(#\space #\newline) out)
                     (second pair)))))))))

(defmacro with-rest-auth ((username password) &body body)
  "Macro to verify authentication before executing"
  `(if (auth-rest-user ,username ,password)
       (progn ,@body)
       (progn
         (setf (lack.response:response-status ningle:*response*) 401)
         (json:encode-json-to-string
          (list (cons :error "Invalid credentials"))))))
```

#### 3.2 Convenience Macros

```lisp
(defmacro with-rest-graph ((graph-name) &body body)
  "Get graph, return 404 if not found"
  `(let ((*graph* (lookup-graph 
                   (intern (json:camel-case-to-lisp ,graph-name) :keyword))))
     (if (graph-p *graph*)
         (progn ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list (cons :error
                       (format nil "Unknown graph ~A" ,graph-name))))))))

(defmacro with-rest-vertex ((id) &body body)
  "Get vertex, return 404 if not found"
  `(let ((vertex (lookup-vertex ,id)))
     (if vertex
         (progn ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list (cons :error
                       (format nil "Unknown vertex ~A" ,id))))))))

(defmacro with-rest-edge ((id) &body body)
  "Get edge, return 404 if not found"
  `(let ((edge (lookup-edge ,id)))
     (if edge
         (progn ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list (cons :error
                       (format nil "Unknown edge ~A" ,id))))))))
```

#### 3.3 JSON Encoding

```lisp
(defmethod json-encode ((vertex vertex))
  "Serialize vertex to JSON"
  (let* ((class (class-of vertex))
         (class-name (class-name class)))
    (json-rpc::encode-json-to-string
     (nconc
      (list (cons :id (string-id vertex))
            (cons :type (json:lisp-to-camel-case
                        (symbol-name class-name))))
      ;; Add all data slots
      (mapcar (lambda (slot-name)
                (cons slot-name (slot-value vertex slot-name)))
              (data-slots (find-class class-name)))))))

(defmethod json-encode ((edge edge))
  "Serialize edge to JSON"
  (let* ((class (class-of edge))
         (class-name (class-name class)))
    (json-rpc::encode-json-to-string
     (nconc
      (list (cons :id (string-id edge))
            (cons :type (json:lisp-to-camel-case
                        (symbol-name class-name)))
            (cons :from (string-id (from edge)))
            (cons :to (string-id (to edge))))
      (mapcar (lambda (slot-name)
                (cons slot-name (slot-value edge slot-name)))
              (data-slots (find-class class-name)))))))

(defmethod json-encode ((graph graph))
  "Serialize graph and its schema to JSON"
  (json-rpc::encode-json-to-string
   (list
    (cons :name (graph-name graph))
    (cons :type :graph)
    (cons :mode (if (typep graph 'slave-graph) :read-only :read-write))
    ;; Vertex types with slots
    (cons :vertex-types
          (loop for key being the hash-keys
                in (gethash :vertex (schema-type-table (schema graph)))
                using (hash-value type-def)
                if (numberp key) collecting
                (list (cons :name (node-type-name type-def))
                      (cons :slots
                           (mapcar (lambda (slot-def)
                                     (list (cons :name (first slot-def))
                                           (cons :type (or (getf slot-def :type) :any))))
                                   (node-type-slots type-def))))))
    ;; Edge types with slots
    (cons :edge-types
          (loop for key being the hash-keys
                in (gethash :edge (schema-type-table (schema graph)))
                using (hash-value type-def)
                if (numberp key) collecting
                (list (cons :name (node-type-name type-def))
                      (cons :slots
                           (mapcar (lambda (slot-def)
                                     (list (cons :name (first slot-def))
                                           (cons :type (or (getf slot-def :type) :any))))
                                   (node-type-slots type-def)))))))))
```

#### 3.4 REST Endpoints

```lisp
(defun rest-get-graph (params)
  "GET /graph/:graph-name → JSON schema"
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (json-encode *graph*))))

(defun rest-get-vertex (params)
  "GET /graph/:graph-name/vertex/:node-id → JSON vertex"
  (with-rest-auth (...)
    (with-rest-graph (...)
      (with-rest-vertex (...)
        (json-encode vertex)))))

(defun rest-post-vertex (params)
  "POST /graph/:graph-name/vertex/:type → Create vertex"
  ;; Parse type and slots from params
  ;; Create instance with make-vertex
  ;; Return JSON

(defun rest-put-vertex (params)
  "PUT /graph/:graph-name/vertex/:node-id → Update vertex"
  ;; Copy vertex
  ;; Update slots
  ;; Save within transaction
  ;; Return updated JSON

(defun rest-delete-vertex (params)
  "DELETE /graph/:graph-name/vertex/:node-id → Delete"
  (mark-deleted vertex)

(defun rest-get-edge (params)
  "GET /graph/:graph-name/edge/:node-id → JSON edge"

(defun rest-post-edge (params)
  "POST /graph/:graph-name/edge/:type → Create edge"
  ;; Requires from and to vertex IDs

(defun rest-put-edge (params)
  "PUT /graph/:graph-name/edge/:node-id → Update edge"

(defun rest-delete-edge (params)
  "DELETE /graph/:graph-name/edge/:node-id → Delete"

(defun rest-list-edges (params)
  "GET /graph/:graph-name/vertex/:node-id/edges → List edges"
  ;; Returns outgoing + incoming
```

#### 3.5 Server Control

```lisp
(defun start-rest (&optional (port *rest-port*))
  "Start REST server on port"
  (prog1
      (setq *rest-app* (make-instance 'ningle:<app>))
    ;; Register routes
    (setf (ningle:route *rest-app* "/graph/:graph-name" :method :get)
          'rest-get-graph)
    
    (setf (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :get)
          'rest-get-vertex)
    
    (setf (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :delete)
          'rest-delete-vertex)
    
    ;; ... more routes ...
    
    ;; Start with Clack
    (setq *clack-app* (clack:clackup *rest-app* :port port))))

(defun stop-rest (&optional (app *clack-app*))
  "Stop REST server"
  (clack:stop app)
  (setq *rest-app* nil))
```

## Complete REST API

### HTTP Routes

```
VERTICES:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name/vertex/:node-id
        → Get vertex
        → Response: { id, type, name, age, ... }

POST    /graph/:graph-name/vertex/:type
        → Create vertex of type
        → Body: { username, password, name: "Alice", age: 30 }
        → Response: { id, type, name, age, ... }

PUT     /graph/:graph-name/vertex/:node-id
        → Update vertex
        → Body: { username, password, age: 31 }
        → Response: { id, type, name, age: 31, ... }

DELETE  /graph/:graph-name/vertex/:node-id
        → Delete vertex (soft delete)
        → Response: { deleted: true }


EDGES:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name/edge/:node-id
        → Get edge
        → Response: { id, type, from, to, weight, ... }

POST    /graph/:graph-name/edge/:type
        → Create edge
        → Body: { username, password, from: "UUID1", to: "UUID2", weight: 0.8 }
        → Response: { id, type, from, to, weight, ... }

PUT     /graph/:graph-name/edge/:node-id
        → Update edge
        → Body: { username, password, weight: 0.9 }
        → Response: { id, type, from, to, weight: 0.9, ... }

DELETE  /graph/:graph-name/edge/:node-id
        → Delete edge (soft delete)
        → Response: { deleted: true }


GRAPHS:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name
        → Get graph schema
        → Response: { name, type, mode, vertexTypes[], edgeTypes[] }


VERTEX EDGES:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name/vertex/:node-id/edges
        → List all edges (incoming + outgoing)
        → Response: [{ id, type, from, to, ... }, ...]
```

### Authentication

```
All endpoints require:
  ?username=alice&password=secret123

Or in POST/PUT body:
  { "username": "alice", "password": "secret123", ... }

User management:
  (add-rest-user "alice" "secret123")
  (delete-rest-user "alice")
```

## Graph Traversal

### Usage

```lisp
;; Example: find friends up to depth 2
(traverse alice-vertex
         :graph *graph*
         :direction :out
         :edge-type 'friend
         :max-depth 2
         :return-paths t)

;; Result: list of traversals with paths
;; Each traversal = { end-vertex, path: [edge1, edge2, ...] }
```

### BFS vs DFS

```
BFS (current):
  ┌─────────┐
  │  Alice  │
  └────┬────┘
       ├─→ Bob (depth 1)
       ├─→ Charlie (depth 1)
       └─→ Diana (depth 1)
           ├─→ Eve (depth 2)
           └─→ Frank (depth 2)

Discovery order: Bob, Charlie, Diana, Eve, Frank

DFS (not implemented):
  ┌─────────┐
  │  Alice  │
  └────┬────┘
       ├─→ Bob (depth 1)
       │   └─→ Eve (depth 2)
       ├─→ Charlie (depth 1)
       └─→ Diana (depth 1)
           └─→ Frank (depth 2)

Discovery order: Bob, Eve, Charlie, Diana, Frank
```

## Authentication

### Flow

```
1. User registers with add-rest-user
   → Writes to /rpasswd (htpasswd format)
   → Format: username:$2a$10$salt$hash

2. Client sends request:
   GET /graph/mygraph/vertex/UUID?username=alice&password=secret

3. Server calls auth-rest-user
   → Reads /rpasswd
   → Finds username
   → Validates password with openssl
   → If OK: executes endpoint
   → If NOT: returns 401 + { error: "Invalid credentials" }

4. Inside transaction (optional):
   with-transaction (graph)
     (setf (slot-value vertex 'name) "New Name")
     (save vertex)
```

### Security

```
✓ Passwords hashed with bcrypt (htpasswd)
✓ Verification with openssl
✓ 401 if auth fails
✓ All logic inside with-rest-auth

⚠️ HTTPS recommended in production
⚠️ /rpasswd should have 600 permissions
```

## Usage Examples

### Using curl

```bash
# Create user
curl http://localhost:8080/user/add -d "username=alice&password=secret"

# Get graph schema
curl "http://localhost:8080/graph/mygraph?username=alice&password=secret"

# Create "user" type vertex
curl -X POST "http://localhost:8080/graph/mygraph/vertex/user" \
  -d "username=alice&password=secret&name=Alice&email=alice@ex.com"

# Get vertex
curl "http://localhost:8080/graph/mygraph/vertex/550e8400-e29b-41d4-a716-446655440000?username=alice&password=secret"

# Update vertex
curl -X PUT "http://localhost:8080/graph/mygraph/vertex/550e..." \
  -d "username=alice&password=secret&age=30"

# Create edge
curl -X POST "http://localhost:8080/graph/mygraph/edge/follows" \
  -d "username=alice&password=secret&from=UUID1&to=UUID2&weight=0.9"

# List vertex edges
curl "http://localhost:8080/graph/mygraph/vertex/UUID/edges?username=alice&password=secret"

# Delete vertex
curl -X DELETE "http://localhost:8080/graph/mygraph/vertex/UUID?username=alice&password=secret"
```

### Programmatically (Lisp)

```lisp
;; Start server
(start-rest 8080)
;; → Server listening at http://localhost:8080

;; Create user
(add-rest-user "alice" "secret123")

;; From client (e.g.: Python requests, JavaScript fetch, etc.)
;; GET http://localhost:8080/graph/mygraph?username=alice&password=secret123
;; → Returns JSON schema

;; Stop server
(stop-rest)
```

## Load Order

```
LOAD ORDER - LAYER 7 (FINAL)
═══════════════════════════════════════════════════════════

From LAYER 6 (vertex, edge, schema):
    ↓
interface.lisp .............. CRUD generics
    ├─ Depends: Layer 6 (vertex, edge)
    └─ Defines: copy, mark-deleted, save
         ↓
    traverse.lisp ........... BFS Traversal
         ├─ Depends: Layer 6 (lookup, map-edges)
         └─ Defines: traverse, traversal class
              ↓
    rest.lisp ............... HTTP REST server
         ├─ Depends: Ningle, Clack, Bordeaux-threads
         ├─ Uses: interface, traverse, schema
         └─ Defines: REST endpoints, JSON encoding
```


*VivaceGraph Layer 7 Documentation*  
*March 2026*
