# VivaceGraph - Capa 6: Modelo de Datos

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Propósito y Responsabilidades](#propósito-y-responsabilidades)
3. [Componentes de la Capa 6](#componentes-de-la-capa-6)
4. [Archivos Detallados](#archivos-detallados)
5. [Jerarquía de Clases](#jerarquía-de-clases)
6. [Sistema de Tipos Dinámico](#sistema-de-tipos-dinámico)
7. [Operaciones CRUD](#operaciones-crud)
8. [Serialización de Nodos](#serialización-de-nodos)
9. [Orden de Carga](#orden-de-carga)

## Visión General

La **Capa 6** define las **estructuras de datos concretas** que VivaceGraph manipula:

- **Nodos primitivos:** Base, flags, serialización
- **Vértices:** Nodos sin conectividad
- **Aristas:** Conexiones dirigidas con peso
- **Sistema de tipos:** Extensión dinámica de clases
- **Operaciones CRUD:** Create, Read, Update, Delete

### Características Clave

- ✓ **Jerarquía CLOS:** Vértex/Edge heredan de Node
- ✓ **Tipos dinámicos:** Definir nuevos tipos en runtime
- ✓ **Slots persistentes/efímeros:** Control fino de persistencia
- ✓ **Serialización automática:** Objetos ↔ bytes
- ✓ **Lazy loading:** Datos deserializados bajo demanda
- ✓ **Versionado:** Cada nodo tiene revisión

### Líneas de Código

```
Archivo                    Líneas
──────────────────────────────────
primitive-node.lisp         319
vertex.lisp                 194
edge.lisp                   427
schema.lisp                 411
──────────────────────────────────
TOTAL                      1351 líneas
```

## Propósito y Responsabilidades

### ¿Por qué existe la Capa 6?

VivaceGraph necesita:
1. **Representar nodos y aristas** como objetos CLOS
2. **Persistencia automática** sin código manual
3. **Extensión de tipos** sin recompilación
4. **Acceso rápido** mediante tablas hash
5. **Versionado y concurrencia** con revisiones

### Responsabilidades Específicas

| Responsabilidad | Archivo | Razón |
|-----------------|---------|-------|
| Nodo base primitivo | `primitive-node.lisp` | Serialización, flags, CRUD base |
| Vértices | `vertex.lisp` | Nodos sin aristas salientes |
| Aristas | `edge.lisp` | Conexiones con from/to/weight |
| Esquema dinámico | `schema.lisp` | Definir nuevos tipos Vertex/Edge |

## Componentes de la Capa 6

### Diagrama de Dependencias

```
CAPA 6 - DEPENDENCIAS INTERNAS
================================

Desde CAPA 5 (ve-index, vev-index, type-index):
    ↓
primitive-node.lisp ........ Clase base Node
    ├─ Depende: Capa 4 (allocator, serialize)
    ├─ Usa: CLOS, metaclases
    └─ Define: node, flags, CRUD base
         ↓
    vertex.lisp ............. Clase Vertex
         ├─ Depende: primitive-node
         └─ Define: vertex, make-vertex, map-vertices
              ↓
    edge.lisp ............... Clase Edge
         ├─ Depende: primitive-node
         ├─ Usa: ve-index, vev-index (Capa 5)
         └─ Define: edge, make-edge, outgoing/incoming
              ↓
    schema.lisp ............. Sistema de tipos
         ├─ Depende: vertex, edge, prologc (Capa 5)
         └─ Define: node-type, def-vertex, def-edge
```

## Archivos Detallados

### 1. `primitive-node.lisp` (319 líneas) - **ARCHIVO CLAVE**

**Propósito:** Clase base `Node` con operaciones de serialización, flags, y CRUD.

#### 1.1 Node Header (Metadata)

```
NODE HEADER EN MEMORIA (15 bytes):
═══════════════════════════════════════════════════════════

Byte 0:     FLAGS (1 byte, 7 flags)
            ├─ bit 0: deleted-p (¿borrado?)
            ├─ bit 1: written-p (¿escribió a disco?)
            ├─ bit 2: heap-written-p (¿datos en heap?)
            ├─ bit 3: type-idx-written-p (¿en type-index?)
            ├─ bit 4: views-written-p (¿vistas actualizadas?)
            ├─ bit 5: ve-written-p (¿en VE-index?) [solo edges]
            └─ bit 6: vev-written-p (¿en VEV-index?) [solo edges]

Bytes 1-2:  TYPE-ID (2 bytes, big-endian)
            ├─ 0 = Generic (vertex/edge)
            └─ 1-65535 = Custom type ID

Bytes 3-6:  REVISION (4 bytes, big-endian)
            └─ Número de versión (MVCC)

Bytes 7-14: DATA-POINTER (8 bytes, big-endian)
            └─ Dirección en heap donde están datos
```

#### 1.2 Estructura Node

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
   
   ;; FLAGS (metadata, no persistentes)
   (%deleted-p :meta t)
   (%written-p :meta t)
   (%heap-written-p :meta t)
   (%type-idx-written-p :meta t)
   (%views-written-p :meta t)
   (%ve-written-p :meta t)        ; solo edges
   (%vev-written-p :meta t)       ; solo edges
   
   ;; DATOS
   (%data-pointer :type word :meta t)  ; Dirección en heap
   (%data :persistent t)                ; Alist o map
   (%bytes :ephemeral t))               ; Serialización caché
  (:metaclass node-class))
```

**Atributos:**
- `:persistent t` - Datos almacenados en disco
- `:ephemeral t` - Datos solo en memoria (no persisten)
- `:meta t` - Metadatos (no versionados)

#### 1.3 Operaciones Primitivas

```lisp
(defun serialize-node-head (mf n offset)
  "Serializar header de nodo (15 bytes)"
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
  "Deserializar header"
  ;; Retorna 7 valores: flags (7 bytes bool), type-id, revision, pointer
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
  "Buscar nodo por ID en tabla hash"
  ;; 1. Verificar caché
  (or (and *cache-enabled*
           (gethash key (cache graph)))
      ;; 2. Buscar en tabla
      (let ((node (lhash-get table key)))
        (when (node-p node)
          ;; 3. Guardar en caché
          (setf (gethash key (cache graph)) node)
          (record-graph-read)
          node))))

(defun save-node (node table &key (graph *graph*))
  "Guardar nodo a disco"
  ;; 1. Serializar datos si hay
  (when (plusp (data-pointer node))
    (if (data node)
        (setf (bytes node) (serialize (data node)))
        (maybe-init-node-data node :graph graph))
    ;; 2. Allocar en heap
    (let ((addr (allocate (heap graph) (length (bytes node)))))
      (dotimes (i (length (bytes node)))
        (set-byte (heap graph)
                 (+ i addr) (aref (bytes node) i)))
      (setf (data-pointer node) addr)))
  
  ;; 3. Actualizar en hash table
  (with-locked-hash-key (table (id node))
    (lhash-put table (id node) node)
    ;; 4. Cachear
    (setf (gethash (id node) (cache graph)) node)))
```

### 2. `vertex.lisp` (194 líneas)

**Propósito:** Clase `Vertex` - nodo sin aristas salientes implícitas.

#### 2.1 Clase Vertex

```lisp
(defclass vertex (node)
  ()
  (:metaclass node-class))
```

Vertex hereda TODO de Node, sin campos adicionales.

#### 2.2 Operaciones

```lisp
(defun make-vertex (type-id data &key id deleted-p revision 
                               retry-p (graph *graph*))
  "Crear nuevo vértice"
  ;; 1. Resolver tipo
  (let ((type-meta 
         (or (eq type-id :generic)
             (lookup-node-type-by-name type-id :vertex :graph graph))))
    
    ;; 2. Serializar datos
    (let ((bytes (when data (serialize data))))
      
      ;; 3. Crear instancia
      (let ((v (%make-vertex 
                :id (or id (gen-vertex-id))
                :type-id (if (eq type-meta :generic) 0 (node-type-id type-meta))
                :revision (or revision 0)
                :deleted-p deleted-p
                :bytes bytes
                :data data)))
        
        ;; 4. Cambiar clase si es custom type
        (when (and type-meta (not (eq type-meta :generic)))
          (change-class v (node-type-name type-meta)))
        
        ;; 5. Guardar a disco
        (handler-case
            (create-node v graph)
          (duplicate-key-error (c)
            ;; Reintentar con nuevo ID si se solicita
            (if retry-p
                (make-vertex type-id data 
                           :id (gen-vertex-id)
                           :revision revision :graph graph)
                (error c))))
        v))))

(defmethod lookup-vertex ((id array) &key (graph *graph*))
  "Buscar vértice por ID"
  (lookup-object id (vertex-table graph) *transaction* graph))

(defmethod lookup-vertex ((id string) &key (graph *graph*))
  "Buscar vértice por string UUID"
  (lookup-vertex (read-id-array-from-string id) :graph graph))

(defmethod delete-vertex ((vertex vertex) &key (graph *graph*))
  "Borrar vértice lógicamente"
  (when (deleted-p vertex)
    (error 'vertex-already-deleted-error :node vertex))
  (delete-node vertex graph))

(defun map-vertices (fn graph &key collect-p vertex-type 
                               include-deleted-p 
                               (include-subclasses-p t))
  "Iterar sobre vértices (potencialmente filtrados)"
  (let ((result nil))
    (cond
      ;; Filtrar por tipo
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
      
      ;; Sin filtro: iterar todos
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

### 3. `edge.lisp` (427 líneas) - **ARCHIVO IMPORTANTE**

**Propósito:** Clase `Edge` - aristas dirigidas con from/to/weight.

#### 3.1 Estructura Edge

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

**Campos:**
- `from` - UUID del vértice origen (16 bytes)
- `to` - UUID del vértice destino (16 bytes)
- `weight` - Peso de la arista (float 64-bit IEEE)

#### 3.2 Edge Header Layout (35 bytes)

```
EDGE HEADER:
═══════════════════════════════════════════════════════════

Bytes 0-14:     Node header (15 bytes)
                ├─ Flags, type-id, revision, data-pointer
                
Bytes 15-30:    FROM UUID (16 bytes)
                ├─ Identificador del vértice origen
                
Bytes 31-46:    TO UUID (16 bytes)
                ├─ Identificador del vértice destino
                
Bytes 47-54:    WEIGHT (8 bytes, IEEE 754 double)
                ├─ Peso de la arista
```

#### 3.3 Operaciones Edge

```lisp
(defun serialize-edge-head (mf e offset)
  "Serializar header edge (35 bytes)"
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
  "Crear nueva arista dirigida from → to"
  ;; 1. Resolver tipo
  (let ((type-meta (lookup-node-type-by-name type-id :edge :graph graph)))
    (when (or (null from) (null to))
      (error "from y to son requeridos"))
    
    ;; 2. Serializar datos
    (let ((bytes (when data (serialize data))))
      
      ;; 3. Crear edge
      (let ((e (%make-edge
                :id (or id (gen-edge-id))
                :from (if (node-p from) (id from) from)
                :to (if (node-p to) (id to) to)
                :type-id (if type-meta (node-type-id type-meta) 0)
                :revision (or revision 0)
                :deleted-p deleted-p
                :bytes bytes
                :data data)))
        
        ;; 4. Guardar
        (create-node e graph)
        
        ;; 5. Indexar
        (add-to-ve-index e graph)
        (add-to-vev-index e graph)
        (add-to-type-index e graph)
        
        e))))

(defmethod outgoing-edges ((v vertex) &key (graph *graph*) 
                                          edge-type)
  "Obtener todas las aristas salientes de vértice"
  (map-edges 'identity graph 
            :vertex v 
            :edge-type edge-type
            :direction :out
            :collect-p t))

(defmethod incoming-edges ((v vertex) &key (graph *graph*) 
                                          edge-type)
  "Obtener todas las aristas entrantes"
  (map-edges 'identity graph
            :vertex v
            :edge-type edge-type
            :direction :in
            :collect-p t))

(defmethod map-edges (fn graph &key collect-p edge-type vertex 
                                   direction 
                                   (include-deleted-p nil))
  "Iterar sobre aristas con múltiples filtros"
  (let ((result nil))
    (cond
      ;; Filtro: edge-type + direction
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
      
      ;; Filtro: solo edge-type
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
      
      ;; Sin filtros: todas
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

### 4. `schema.lisp` (411 líneas) - **SISTEMA DE TIPOS**

**Propósito:** Sistema dinámico de tipos - definir nuevas clases en runtime.

#### 4.1 Estructura Schema

```lisp
(defstruct schema
  (lock (make-recursive-lock))
  
  ;; Tabla de tipos: { :vertex → { type-id → node-type, ... }, 
  ;;                   :edge → { type-id → node-type, ... } }
  (type-table (make-hash-table :test 'eql :synchronized t))
  
  ;; Locks por clase para actualización concurrente
  (class-locks (make-hash-table :test 'eql :synchronized t))
  
  ;; Contadores para generar nuevos IDs
  (next-edge-id 1 :type (unsigned-byte 16))
  (next-vertex-id 1 :type (unsigned-byte 16)))

(defstruct node-type
  name                    ; Nombre de clase (símbolo)
  parent-type             ; Clase padre
  id                      ; Type ID (0-65535)
  graph-name              ; Grafo donde está definido
  slots                   ; Specs de slots
  package                 ; Package para la clase
  constructor)            ; Función para crear instancias
```

#### 4.2 Macros de Definición

```lisp
(defmacro def-vertex (name parent-types slot-specs graph-name)
  "Definir nuevo tipo de vértice"
  `(def-node-type ,name (,@parent-types vertex) 
                  ,slot-specs ,graph-name))

(defmacro def-edge (name parent-types slot-specs graph-name)
  "Definir nuevo tipo de arista"
  `(def-node-type ,name (,@parent-types edge) 
                  ,slot-specs ,graph-name))

(defmacro def-node-type (name parent-types slot-specs graph-name)
  "Macro principal para definir tipos"
  `(let ((meta (make-node-type 
               :name ',name
               :parent-type ',(first parent-types)
               :id (get-next-type-id (schema (lookup-graph ',graph-name))
                                    ',(if (find 'edge parent-types)
                                          :edge :vertex))
               :graph-name ',graph-name
               :slots ',slot-specs)))
     
     ;; 1. Definir clase CLOS
     (defclass ,name (,@parent-types)
       ,(mapcar (lambda (spec)
                  (let ((name (car spec)))
                    `(,name :initarg ,(intern (symbol-name name) :keyword)
                           :accessor ,name
                           :allocation :instance)))
               slot-specs)
       (:metaclass node-class))
     
     ;; 2. Registrar en schema
     (setf (gethash (node-type-id meta)
                   (gethash ,(if (find 'edge parent-types) :edge :vertex)
                           (schema-type-table (schema (lookup-graph ',graph-name)))))
          meta)
     
     ;; 3. Generar predicados Prolog automáticamente
     ;; Para queries tipo: ?- user(Name, Email).
     (def-global-prolog-functor ,(intern (format nil "~A/~A" name 
                                                 (length slot-specs)))
       ,params
       ...)
     
     meta))
```

#### 4.3 Ejemplo de Definición

```lisp
;; Definir tipo de vértice "User"
(def-vertex user (user)
  ((name :persistent t :type string)
   (email :persistent t :type string)
   (age :persistent t :type integer))
  my-graph)

;; Esto genera:
;; - Clase CLOS: user
;; - Type ID: (ej) 1
;; - Predicado Prolog: user/3 para queries
;; - Slots: name, email, age (persistentes)

;; Definir tipo de arista "follows"
(def-edge follows (follows)
  ((strength :persistent t :type float))
  my-graph)

;; Crear instancias:
(let ((alice (make-vertex 'user 
                         '((:name "Alice") (:email "alice@example.com")))))
  (let ((bob (make-vertex 'user 
                         '((:name "Bob") (:email "bob@example.com")))))
    (let ((edge (make-edge alice bob 'follows 
                          :data '((:strength 0.9)))))
      edge)))
```

#### 4.4 Operaciones Schema

```lisp
(defmethod init-schema ((graph graph))
  "Inicializar schema vacío para grafo"
  (let ((schema (make-schema)))
    (setf (schema graph) schema)
    
    ;; Crear tablas de tipos para vertex y edge
    (setf (gethash :edge (schema-type-table schema))
          (make-hash-table :test 'eql :synchronized t))
    (setf (gethash :vertex (schema-type-table schema))
          (make-hash-table :test 'eql :synchronized t))
    
    ;; Crear locks base
    (setf (gethash 'edge (schema-class-locks schema))
          (make-rw-lock))
    (setf (gethash 'vertex (schema-class-locks schema))
          (make-rw-lock))
    
    schema))

(defmethod save-schema ((schema schema) (graph graph))
  "Persistir schema a disco"
  (with-recursive-lock-held ((schema-lock schema))
    (let ((schema-file (format nil "~A/schema.dat" (location graph))))
      ;; Guardar con cl-store (serialización Lisp)
      (cl-store:store schema schema-file))))

(defmethod schema-digest ((schema schema))
  "Obtener hash MD5 del esquema (para replicación)"
  (map nil 
       (lambda (octet)
         (format t "~(~2,'0X~)'" octet))
       (md5:md5sum-string (schema-string-representation schema)
                         :external-format :utf8)))

(defun list-vertex-types (&optional (graph *graph*))
  "Obtener IDs de todos los tipos de vértices"
  (nconc (list 0)  ; Generic vertex type
        (loop for key being the hash-keys
              in (gethash :vertex (schema-type-table (schema graph)))
              if (numberp key) collecting key)))

(defun list-edge-types (&optional (graph *graph*))
  "Obtener IDs de todos los tipos de aristas"
  (nconc (list 0)  ; Generic edge type
        (loop for key being the hash-keys
              in (gethash :edge (schema-type-table (schema graph)))
              if (numberp key) collecting key)))
```

## Jerarquía de Clases

```
HERENCIA CLOS
═════════════════════════════════════════════════════════════

Object (Lisp)
    │
    ├─ Node (node-class metaclass)
    │   │   [Clase base para todos los nodos]
    │   │   Slots: id, type-id, revision, deleted-p, 
    │   │          written-p, data-pointer, data, bytes
    │   │
    │   ├─ Vertex
    │   │   [Sin campos adicionales]
    │   │   Ejemplo: generic vertex
    │   │
    │   │   ├─ User [custom type]
    │   │   │   [name, email, age]
    │   │   │
    │   │   ├─ Post [custom type]
    │   │   │   [title, content, published-at]
    │   │   │
    │   │   └─ ...otros types...
    │   │
    │   └─ Edge
    │       [from, to, weight]
    │       Ejemplo: generic edge
    │
    │       ├─ Follows [custom type]
    │       │   [strength]
    │       │
    │       ├─ Likes [custom type]
    │       │   [rating]
    │       │
    │       └─ ...otros types...
    │
    └─ ... otras clases CLOS ...
```

## Sistema de Tipos Dinámico

### Ciclo de Vida

```
1. DEFINICIÓN (en tiempo de carga o runtime):
   ─────────────────────────────────────────────
   (def-vertex user (user)
     ((name :persistent t)
      (email :persistent t)))
   
   → Crea estructura node-type
   → Registra en schema
   → Define clase CLOS
   → Genera predicados Prolog

2. LOOKUP (cuando se crea instancia):
   ─────────────────────────────────────
   (make-vertex 'user data)
   
   → lookup-node-type-by-name 'user
   → Obtiene node-type meta
   → change-class v user
   → Procede como user instance

3. CONSULTA (queries Prolog):
   ──────────────────────────
   ?- user(Name, Email)
   
   → Busca predicado user/2
   → Itera sobre type-index
   → Unifica con variables
```

### Campos de Slot

```
(def-vertex person ()
  ((name :persistent t :type string)
   (age :persistent t :type integer)
   (temp-cache :ephemeral t)
   (readonly :readonly t)))

:persistent t     → Almacenar en disco
:persistent nil   → Solo en memoria, no persiste
:ephemeral t      → Solo en RAM, nunca serializar
:readonly t       → No permite setf
:type T           → Validación (opt)
```

## Operaciones CRUD

### CREATE

```lisp
;; Crear vértice genérico
(let ((v (make-vertex :generic '((:name "Alice")))))
  v)

;; Crear vértice typed
(let ((u (make-vertex 'user '((:name "Bob") (:email "bob@ex.com")))))
  u)

;; Crear arista
(let ((e (make-edge v1 v2 'follows :data '((:weight 0.8)))))
  e)
```

### READ

```lisp
;; Lookup vértice por ID
(lookup-vertex vertex-id :graph graph)

;; Lookup arista por ID
(lookup-edge edge-id :graph graph)

;; Acceso a slots
(slot-value user 'name)
(node-slot-value user :email)

;; Lazy loading de datos
(maybe-init-node-data node :graph graph)
```

### UPDATE

```lisp
;; Modificar slot
(setf (slot-value user 'name) "Charlie")

;; Guardar cambios
(save-node user (vertex-table graph) :graph graph)

;; Dentro de transacción: cambios automáticos
(with-transaction (graph)
  (setf (slot-value user 'name) "Dave"))
```

### DELETE

```lisp
;; Borrado lógico
(delete-vertex v :graph graph)
(delete-edge e :graph graph)

;; Revisar si está borrado
(deleted-p node)

;; Borrado físico (compactación)
(compact-vertices graph)
(compact-edges graph)
```

## Serialización de Nodos

### Proceso

```
SAVING NODE
═════════════════════════════════════════════════════════════

1. Serializar datos (alist → bytes)
   ────────────────────────────────
   data = (:name "Alice" :age 30)
         ↓
   bytes = [type-tag, length, bytes...]
   
2. Allocar en heap
   ────────────────
   addr = allocate(heap, length(bytes))
   
3. Escribir a heap
   ───────────────
   for i in 0..length(bytes):
     set-byte(heap, addr+i, bytes[i])
   
4. Actualizar data-pointer
   ───────────────────────
   node.data-pointer = addr
   
5. Serializar header
   ──────────────────
   header = [flags, type-id, revision, addr]
   write-to-table(node.id, header)
   
6. Cachear
   ──────
   cache[node.id] = node


LOADING NODE
═════════════════════════════════════════════════════════════

1. Buscar en caché
   ────────────
   node = cache.get(id)
   if found: return node
   
2. Deserializar header
   ───────────────────
   (flags, type-id, revision, ptr) = read-header(table, id)
   
3. Crear instancia
   ───────────────
   node = new Node(id, type-id, revision, ptr)
   
4. Lazy load datos
   ────────────
   upon access:
     bytes = read-bytes(heap, ptr)
     data = deserialize(bytes)
     node.data = data
     node.bytes = bytes (caché)
   
5. Cachear
   ──────
   cache[id] = node
```

## Orden de Carga

```
ORDEN DE CARGA - CAPA 6
═══════════════════════════════════════════════════════════

Desde CAPA 5 (ve-index, vev-index, type-index):
    ↓
primitive-node.lisp ........ Clase base Node
    ├─ Depende: Capa 4 (allocator, serialize)
    ├─ Usa: CLOS, metaclases
    └─ Define: node, flags, CRUD base
         ↓
    vertex.lisp ............. Clase Vertex
         ├─ Depende: primitive-node
         └─ Define: vertex, make-vertex, map-vertices
              ↓
    edge.lisp ............... Clase Edge
         ├─ Depende: primitive-node
         ├─ Usa: ve-index, vev-index (Capa 5)
         └─ Define: edge, make-edge, outgoing/incoming
              ↓
    schema.lisp ............. Sistema de tipos
         ├─ Depende: vertex, edge, prologc (Capa 5)
         └─ Define: node-type, def-vertex, def-edge
```

## Resumen

La **Capa 6** proporciona:

1. ✓ **Primitive Node (319 líneas)** - Clase base, serialización, CRUD
2. ✓ **Vertex (194 líneas)** - Nodos sin aristas
3. ✓ **Edge (427 líneas)** - Aristas dirigidas from→to→weight
4. ✓ **Schema (411 líneas)** - Sistema dinámico de tipos

**Total:** ~1,351 líneas que proporcionan:
- Representación CLOS de datos de grafo
- Serialización automática
- Sistema de tipos extensible
- Operaciones CRUD completas
- Lazy loading de datos
- Indexación automática
- Caché de objetos

En la capa siguiente (7), esta infraestructura se expone mediante APIs de alto nivel.

*Documentación de la Capa 6 de VivaceGraph*
*Marzo 2026*
