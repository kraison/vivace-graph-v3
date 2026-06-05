# VivaceGraph - Capa 7: API de Usuario

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Propósito y Responsabilidades](#propósito-y-responsabilidades)
3. [Componentes de la Capa 7](#componentes-de-la-capa-7)
4. [Archivos Detallados](#archivos-detallados)
5. [API REST Completa](#api-rest-completa)
6. [Traversal del Grafo](#traversal-del-grafo)
7. [Autenticación](#autenticación)
8. [Ejemplos de Uso](#ejemplos-de-uso)
9. [Orden de Carga](#orden-de-carga)

## Visión General

La **Capa 7** es la **capa de API** - interfaz final que expone todas las capas anteriores:

- **Interface:** Genéricos para operaciones CRUD
- **Traverse:** Recorrido del grafo (BFS)
- **REST:** Servidor HTTP completo con autenticación

### Características Clave

- ✓ **Servidor REST HTTP** con Ningle/Clack
- ✓ **CRUD completo** para vértices y aristas
- ✓ **Autenticación** con htpasswd
- ✓ **JSON encoding** automático
- ✓ **Traversal BFS** con límite de profundidad
- ✓ **Introspección de schema** vía REST

### Líneas de Código

```
Archivo                    Líneas
──────────────────────────────────
interface.lisp               27
traverse.lisp                81
rest.lisp                   409
──────────────────────────────────
TOTAL                       517 líneas
```

## Propósito y Responsabilidades

### ¿Por qué existe la Capa 7?

VivaceGraph necesita:
1. **Exponer API a usuarios finales** (REST HTTP)
2. **Operaciones genéricas** (copy, save, delete)
3. **Recorrer grafos** eficientemente (BFS)
4. **Seguridad** mediante autenticación
5. **Serialización a JSON** automática

### Responsabilidades Específicas

| Responsabilidad | Archivo | Razón |
|-----------------|---------|-------|
| Genéricos CRUD | `interface.lisp` | copy, mark-deleted, save para todos tipos |
| Recorrido BFS | `traverse.lisp` | Explorar grafo con límite de profundidad |
| Servidor REST | `rest.lisp` | Endpoints HTTP, JSON, autenticación |

## Componentes de la Capa 7

### Diagrama de Dependencias

```
CAPA 7 - DEPENDENCIAS INTERNAS
================================

Desde CAPA 6 (vertex, edge, schema):
    ↓
interface.lisp .............. Genéricos CRUD
    ├─ Depende: Capa 6 (vertex, edge)
    └─ Define: copy, mark-deleted, save
         ↓
    traverse.lisp ........... Traversal BFS
         ├─ Depende: Capa 6 (edges, lookup)
         └─ Define: traverse, traversal class
              ↓
    rest.lisp ............... Servidor HTTP REST
         ├─ Depende: Ningle, Clack, Bordeaux-threads
         ├─ Usa: interface, traverse, schema
         └─ Define: REST endpoints, JSON encoding
```

## Archivos Detallados

### 1. `interface.lisp` (27 líneas)

**Propósito:** Genéricos para operaciones comunes en todos los tipos de nodos.

#### 1.1 Contenido

```lisp
(defgeneric copy (node)
  "Crear copia de nodo (para transacciones)"
  (:method (thing)
    (error "Cannot save ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex))
    (copy-vertex vertex))
  (:method ((edge edge))
    (copy-edge edge)))

(defgeneric mark-deleted (node)
  "Marcar nodo como borrado (soft delete)"
  (:method (thing)
    (error "Cannot delete ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex))
    (delete-vertex vertex))
  (:method ((edge edge))
    (delete-edge edge)))

(defgeneric save (object &key graph)
  "Guardar cambios de nodo a disco"
  (:method (thing &key graph)
    (declare (ignore graph))
    (error "Cannot save ~S of type ~S" thing (type-of thing)))
  (:method ((vertex vertex) &key (graph *graph*))
    (update-node vertex graph))
  (:method ((edge edge) &key (graph *graph*))
    (update-node edge graph)))
```

**Propósito de cada genérico:**

```
copy:
  Crear copia completa de nodo (para edición en transacción)
  → read-only → copia local → editar → save
  
mark-deleted:
  Marcar como borrado (soft delete)
  → Logical deletion (no libera espacio)
  → Reversible (puede deshacerse)
  
save:
  Persistir cambios a disco
  → Dentro de transacción: auto
  → Fuera: manual
```

### 2. `traverse.lisp` (81 líneas)

**Propósito:** Recorrido del grafo usando BFS (Breadth-First Search).

#### 2.1 Clase Traversal

```lisp
(defclass traversal ()
  ((end-vertex :accessor end-vertex 
               :initarg :end-vertex :initform nil)
   (reverse-path :accessor reverse-path 
                 :initarg :path :initform nil)))

(defmethod traversal-path ((traversal traversal))
  "Obtener camino en orden forward"
  (reverse (reverse-path traversal)))

(defmethod depth ((traversal traversal))
  "Profundidad actual del recorrido"
  (length (reverse-path traversal)))
```

**Estructura:**

```
Traversal representa un camino en el grafo:
  
  ┌─────────────────────────────────────┐
  │ Traversal                           │
  ├─────────────────────────────────────┤
  │ end-vertex: V3                      │
  │ reverse-path: [E1, E2]              │
  │                                     │
  │ Interpretación:                     │
  │   V0 --E1--> V1 --E2--> V3          │
  │                                     │
  │ traversal-path: [E1, E2] (forward)  │
  │ depth: 2                            │
  └─────────────────────────────────────┘
```

#### 2.2 Operaciones

```lisp
(defun make-traversal (vertex path)
  "Crear nuevo traversal en vértice con camino"
  (make-instance 'traversal
                 :end-vertex vertex
                 :path path))

(defmethod update-traversal ((traversal traversal) 
                              (vertex vertex) 
                              (edge edge))
  "Crear nuevo traversal extendiendo uno anterior"
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
  "Explorar grafo desde vértice usando BFS"
  
  ;; 1. Inicializar cola con starting vertex
  (let ((queue (make-queue :elements
                           (list (make-instance 'traversal
                                              :end-vertex vertex))))
        (result-table (make-hash-table :test 'equalp))
        (memory (make-hash-table :test 'equalp)))  ; Visited
    
    ;; 2. BFS loop
    (loop until (empty-queue-p queue) do
         (let* ((traversal (dequeue queue))
                (current-vertex (end-vertex traversal)))
           
           ;; 3. Check profundidad
           (unless (and max-depth
                        (> (depth traversal) max-depth))
             
             ;; 4. Explorar aristas salientes
             (when (or (eql direction :out) (eql direction :both))
               (map-edges (lambda (edge)
                            (let* ((next-vertex (lookup-vertex (to edge)))
                                   (new-traversal
                                    (update-traversal traversal
                                                      next-vertex
                                                      edge)))
                              
                              ;; Solo si no visitado (uniqueness: global)
                              (unless (gethash next-vertex memory)
                                (setf (gethash next-vertex memory) t)
                                (enqueue queue new-traversal))
                              
                              ;; Filtrar por edge-type
                              (when (typep edge edge-type)
                                (setf (gethash next-vertex result-table)
                                      new-traversal))))
                          graph
                          :vertex current-vertex
                          :direction :out))
             
             ;; 5. Explorar aristas entrantes
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
    
    ;; 6. Retornar vértices o caminos
    (if return-paths
        (loop for p being the hash-values in result-table collecting p)
        (loop for v being the hash-keys in result-table collecting v))))
```

**Complejidad:**

```
Tiempo:  O(V + E) en el peor caso
         O(V^d) si max-depth = d
         
Espacio: O(V) para cola + memoria
```

**Ejemplos de Uso:**

```lisp
;; Encontrar todos los amigos (profundidad 1)
(traverse alice-vertex 
         :direction :out
         :edge-type 'friend
         :max-depth 1
         :return-paths nil)
→ (bob charlie diana)

;; Encontrar amigos de amigos con caminos
(traverse alice-vertex
         :direction :out
         :edge-type 'friend
         :max-depth 2
         :return-paths t)
→ (traversal[bob, [friend_edge1]]
   traversal[charlie, [friend_edge2]]
   traversal[eve, [friend_edge1, friend_edge3]])
```

### 3. `rest.lisp` (409 líneas) - **SERVIDOR REST**

**Propósito:** Servidor HTTP REST completo que expone toda la API de VivaceGraph.

#### 3.1 Setup y Autenticación

```lisp
(defvar *rest-port* 8080)
(defvar *rest-app* nil)
(defvar *clack-app* nil)
(defvar *rest-procedures* 
  (make-hash-table :synchronized t :test 'equalp))
(defvar *rest-passwd-file* "rpasswd")
(defvar *htpasswd-bin* "/usr/bin/htpasswd")

(defun add-rest-user (username password)
  "Añadir usuario con htpasswd"
  (trivial-shell:shell-command
   (format nil "~A -b ~A ~A ~A ~A"
           *htpasswd-bin*
           (if (probe-file *rest-passwd-file*) "" "-c")
           *rest-passwd-file*
           username
           password)))

(defun auth-rest-user (username password)
  "Autenticar usuario contra htpasswd"
  (with-open-file (stream *rest-passwd-file*)
    (do ((line (read-line stream nil :eof)
               (read-line stream nil :eof)))
        ((eql line :eof))
      (let ((pair (cl-ppcre:split "\\:" line)))
        (when (equalp (first pair) username)
          ;; Verificar contraseña con openssl
          (multiple-value-bind (out err exit-code)
              (trivial-shell:shell-command
               (format nil "openssl passwd -~A -salt ~A ~A"
                       hash salt password))
            (declare (ignore exit-code err))
            (return-from auth-rest-user
              (equalp (string-trim '(#\space #\newline) out)
                     (second pair)))))))))

(defmacro with-rest-auth ((username password) &body body)
  "Macro para verificar autenticación antes de ejecutar"
  `(if (auth-rest-user ,username ,password)
       (progn ,@body)
       (progn
         (setf (lack.response:response-status ningle:*response*) 401)
         (json:encode-json-to-string
          (list (cons :error "Invalid credentials"))))))
```

#### 3.2 Macros de Conveniencia

```lisp
(defmacro with-rest-graph ((graph-name) &body body)
  "Obtener grafo, retornar 404 si no existe"
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
  "Obtener vértice, retornar 404 si no existe"
  `(let ((vertex (lookup-vertex ,id)))
     (if vertex
         (progn ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list (cons :error
                       (format nil "Unknown vertex ~A" ,id))))))))

(defmacro with-rest-edge ((id) &body body)
  "Obtener arista, retornar 404 si no existe"
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
  "Serializar vértice a JSON"
  (let* ((class (class-of vertex))
         (class-name (class-name class)))
    (json-rpc::encode-json-to-string
     (nconc
      (list (cons :id (string-id vertex))
            (cons :type (json:lisp-to-camel-case
                        (symbol-name class-name))))
      ;; Añadir todos los slots de datos
      (mapcar (lambda (slot-name)
                (cons slot-name (slot-value vertex slot-name)))
              (data-slots (find-class class-name)))))))

(defmethod json-encode ((edge edge))
  "Serializar arista a JSON"
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
  "Serializar grafo y su schema a JSON"
  (json-rpc::encode-json-to-string
   (list
    (cons :name (graph-name graph))
    (cons :type :graph)
    (cons :mode (if (typep graph 'slave-graph) :read-only :read-write))
    ;; Vertex types con slots
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
    ;; Edge types con slots
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

#### 3.4 Endpoints REST

```lisp
(defun rest-get-graph (params)
  "GET /graph/:graph-name → JSON schema"
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (json-encode *graph*))))

(defun rest-get-vertex (params)
  "GET /graph/:graph-name/vertex/:node-id → JSON vértice"
  (with-rest-auth (...)
    (with-rest-graph (...)
      (with-rest-vertex (...)
        (json-encode vertex)))))

(defun rest-post-vertex (params)
  "POST /graph/:graph-name/vertex/:type → Create vértice"
  ;; Parsear tipo y slots desde params
  ;; Crear instancia con make-vertex
  ;; Retornar JSON

(defun rest-put-vertex (params)
  "PUT /graph/:graph-name/vertex/:node-id → Update vértice"
  ;; Copiar vértice
  ;; Actualizar slots
  ;; Guardar dentro de transacción
  ;; Retornar JSON actualizado

(defun rest-delete-vertex (params)
  "DELETE /graph/:graph-name/vertex/:node-id → Borrar"
  (mark-deleted vertex)

(defun rest-get-edge (params)
  "GET /graph/:graph-name/edge/:node-id → JSON arista"

(defun rest-post-edge (params)
  "POST /graph/:graph-name/edge/:type → Create arista"
  ;; Requiere from y to vertex IDs

(defun rest-put-edge (params)
  "PUT /graph/:graph-name/edge/:node-id → Update arista"

(defun rest-delete-edge (params)
  "DELETE /graph/:graph-name/edge/:node-id → Borrar"

(defun rest-list-edges (params)
  "GET /graph/:graph-name/vertex/:node-id/edges → Lista aristas"
  ;; Retorna salientes + entrantes
```

#### 3.5 Server Control

```lisp
(defun start-rest (&optional (port *rest-port*))
  "Iniciar servidor REST en puerto"
  (prog1
      (setq *rest-app* (make-instance 'ningle:<app>))
    ;; Registrar rutas
    (setf (ningle:route *rest-app* "/graph/:graph-name" :method :get)
          'rest-get-graph)
    
    (setf (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :get)
          'rest-get-vertex)
    
    (setf (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :delete)
          'rest-delete-vertex)
    
    ;; ... más rutas ...
    
    ;; Iniciar con Clack
    (setq *clack-app* (clack:clackup *rest-app* :port port))))

(defun stop-rest (&optional (app *clack-app*))
  "Detener servidor REST"
  (clack:stop app)
  (setq *rest-app* nil))
```

## API REST Completa

### Rutas HTTP

```
VÉRTICES:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name/vertex/:node-id
        → Obtener vértice
        → Respuesta: { id, type, name, age, ... }

POST    /graph/:graph-name/vertex/:type
        → Crear vértice de tipo
        → Body: { username, password, name: "Alice", age: 30 }
        → Respuesta: { id, type, name, age, ... }

PUT     /graph/:graph-name/vertex/:node-id
        → Actualizar vértice
        → Body: { username, password, age: 31 }
        → Respuesta: { id, type, name, age: 31, ... }

DELETE  /graph/:graph-name/vertex/:node-id
        → Borrar vértice (soft delete)
        → Respuesta: { deleted: true }


ARISTAS:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name/edge/:node-id
        → Obtener arista
        → Respuesta: { id, type, from, to, weight, ... }

POST    /graph/:graph-name/edge/:type
        → Crear arista
        → Body: { username, password, from: "UUID1", to: "UUID2", weight: 0.8 }
        → Respuesta: { id, type, from, to, weight, ... }

PUT     /graph/:graph-name/edge/:node-id
        → Actualizar arista
        → Body: { username, password, weight: 0.9 }
        → Respuesta: { id, type, from, to, weight: 0.9, ... }

DELETE  /graph/:graph-name/edge/:node-id
        → Borrar arista (soft delete)
        → Respuesta: { deleted: true }


GRAFOS:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name
        → Obtener schema del grafo
        → Respuesta: { name, type, mode, vertexTypes[], edgeTypes[] }


ARISTAS DE VÉRTICE:
═════════════════════════════════════════════════════════════

GET     /graph/:graph-name/vertex/:node-id/edges
        → Listar todas las aristas (entrantes + salientes)
        → Respuesta: [{ id, type, from, to, ... }, ...]
```

### Autenticación

```
Todos los endpoints requieren:
  ?username=alice&password=secret123

O en POST/PUT body:
  { "username": "alice", "password": "secret123", ... }

Gestión de usuarios:
  (add-rest-user "alice" "secret123")
  (delete-rest-user "alice")
```

## Traversal del Grafo

### Uso

```lisp
;; Ejemplo: encontrar amigos hasta profundidad 2
(traverse alice-vertex
         :graph *graph*
         :direction :out
         :edge-type 'friend
         :max-depth 2
         :return-paths t)

;; Resultado: lista de traversals con caminos
;; Cada traversal = { end-vertex, path: [edge1, edge2, ...] }
```

### BFS vs DFS

```
BFS (actual):
  ┌─────────┐
  │  Alice  │
  └────┬────┘
       ├─→ Bob (depth 1)
       ├─→ Charlie (depth 1)
       └─→ Diana (depth 1)
           ├─→ Eve (depth 2)
           └─→ Frank (depth 2)

Orden descubrimiento: Bob, Charlie, Diana, Eve, Frank

DFS (no implementado):
  ┌─────────┐
  │  Alice  │
  └────┬────┘
       ├─→ Bob (depth 1)
       │   └─→ Eve (depth 2)
       ├─→ Charlie (depth 1)
       └─→ Diana (depth 1)
           └─→ Frank (depth 2)

Orden descubrimiento: Bob, Eve, Charlie, Diana, Frank
```

## Autenticación

### Flujo

```
1. User registra con add-rest-user
   → Escribe a /rpasswd (htpasswd format)
   → Format: username:$2a$10$salt$hash

2. Client envía request:
   GET /graph/mygraph/vertex/UUID?username=alice&password=secret

3. Server llama auth-rest-user
   → Lee /rpasswd
   → Busca username
   → Valida contraseña con openssl
   → Si OK: ejecuta endpoint
   → Si NO: retorna 401 + { error: "Invalid credentials" }

4. Inside transaction (opcional):
   with-transaction (graph)
     (setf (slot-value vertex 'name) "New Name")
     (save vertex)
```

### Seguridad

```
✓ Contraseñas hasheadas con bcrypt (htpasswd)
✓ Verificación con openssl
✓ 401 si auth falla
✓ Toda la lógica dentro de with-rest-auth

⚠️ HTTPS recommended en producción
⚠️ /rpasswd debe tener permisos 600
```

## Ejemplos de Uso

### Usando curl

```bash
# Crear usuario
curl http://localhost:8080/user/add -d "username=alice&password=secret"

# Obtener schema del grafo
curl "http://localhost:8080/graph/mygraph?username=alice&password=secret"

# Crear vértice de tipo "user"
curl -X POST "http://localhost:8080/graph/mygraph/vertex/user" \
  -d "username=alice&password=secret&name=Alice&email=alice@ex.com"

# Obtener vértice
curl "http://localhost:8080/graph/mygraph/vertex/550e8400-e29b-41d4-a716-446655440000?username=alice&password=secret"

# Actualizar vértice
curl -X PUT "http://localhost:8080/graph/mygraph/vertex/550e..." \
  -d "username=alice&password=secret&age=30"

# Crear arista
curl -X POST "http://localhost:8080/graph/mygraph/edge/follows" \
  -d "username=alice&password=secret&from=UUID1&to=UUID2&weight=0.9"

# Listar aristas de vértice
curl "http://localhost:8080/graph/mygraph/vertex/UUID/edges?username=alice&password=secret"

# Borrar vértice
curl -X DELETE "http://localhost:8080/graph/mygraph/vertex/UUID?username=alice&password=secret"
```

### Programáticamente (Lisp)

```lisp
;; Iniciar servidor
(start-rest 8080)
;; → Servidor escuchando en http://localhost:8080

;; Crear usuario
(add-rest-user "alice" "secret123")

;; Desde cliente (ej: Python requests, JavaScript fetch, etc.)
;; GET http://localhost:8080/graph/mygraph?username=alice&password=secret123
;; → Retorna JSON schema

;; Detener servidor
(stop-rest)
```

## Orden de Carga

```
ORDEN DE CARGA - CAPA 7 (FINAL)
═══════════════════════════════════════════════════════════

Desde CAPA 6 (vertex, edge, schema):
    ↓
interface.lisp .............. Genéricos CRUD
    ├─ Depende: Capa 6 (vertex, edge)
    └─ Define: copy, mark-deleted, save
         ↓
    traverse.lisp ........... Traversal BFS
         ├─ Depende: Capa 6 (lookup, map-edges)
         └─ Define: traverse, traversal class
              ↓
    rest.lisp ............... Servidor HTTP REST
         ├─ Depende: Ningle, Clack, Bordeaux-threads
         ├─ Usa: interface, traverse, schema
         └─ Define: REST endpoints, JSON encoding
```


*Documentación de la Capa 7 de VivaceGraph*  
*Marzo 2026*  
