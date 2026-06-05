# VivaceGraph - Capa 5: Indexación

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Propósito y Responsabilidades](#propósito-y-responsabilidades)
3. [Componentes de la Capa 5](#componentes-de-la-capa-5)
4. [Archivos Detallados](#archivos-detallados)
5. [Índices Especializados](#índices-especializados)
6. [Motor Prolog](#motor-prolog)
7. [Vistas Materializadas](#vistas-materializadas)
8. [Orden de Carga](#orden-de-carga)

## Visión General

La **Capa 5** proporciona **índices especializados** y **motor de queries** que permiten:

- **VE-Index:** Búsqueda rápida Vértice→[Aristas]
- **VEV-Index:** Búsqueda rápida Vértice→Arista→Vértice
- **Type-Index:** Índices por tipo de nodo
- **Functores Prolog:** Predicados para queries complejas
- **Vistas:** Vistas materializadas con map-reduce
- **Motor Prolog:** Evaluador Prolog completo

### Características Clave

- ✓ **VE/VEV Índices:** O(log n) búsqueda de aristas
- ✓ **Type-Index:** O(1) acceso por tipo
- ✓ **Prolog:** Full logic programming, unificación
- ✓ **Vistas:** Map-reduce materializadas
- ✓ **Caché:** Multi-nivel con weak references
- ✓ **Thread-safe:** Locks por índice/vista

### Líneas de Código

```
Archivo                    Líneas
──────────────────────────────────
ve-index.lisp               194
vev-index.lisp              219
type-index.lisp              70
functor.lisp                 64
views.lisp                  732
prologc.lisp                717
──────────────────────────────────
TOTAL                      1996 líneas
```

## Propósito y Responsabilidades

### ¿Por qué existe la Capa 5?

VivaceGraph necesita queries eficientes:
1. **"Dame todas las aristas salientes de V"** → VE-Index
2. **"Dame la arista V1→tipo→V2"** → VEV-Index
3. **"Dame todos los vértices de tipo X"** → Type-Index
4. **"Encuentra caminos donde..."** → Prolog
5. **"Queries complejas con map-reduce"** → Vistas

### Responsabilidades Específicas

| Responsabilidad | Archivo | Razón |
|-----------------|---------|-------|
| Índice vértice-arista | `ve-index.lisp` | O(log n) acceso aristas salientes |
| Índice vértice-arista-vértice | `vev-index.lisp` | O(log n) acceso aristas específicas |
| Índice por tipo | `type-index.lisp` | O(1) acceso nodos por tipo |
| Predicados Prolog | `functor.lisp` | Definir y compilar functores |
| Vistas materializadas | `views.lisp` | Map-reduce, cachés de resultados |
| Evaluador Prolog | `prologc.lisp` | Motor completo de resolución |

## Componentes de la Capa 5

### Diagrama de Dependencias

```
CAPA 5 - DEPENDENCIAS INTERNAS
================================

Desde CAPA 4 (skip-list, linear-hash, index-list):
    ↓
ve-index.lisp .............. VE-Index
    ├─ Depende: linear-hash (Capa 4)
    ├─ Usa: ve-key, ve-key-equal
    └─ Define: ve-index, lookup-ve
         ↓
    vev-index.lisp ......... VEV-Index
         ├─ Depende: linear-hash (Capa 4)
         ├─ Usa: vev-key
         └─ Define: vev-index, lookup-vev
              ↓
    type-index.lisp ........ Type-Index
         ├─ Depende: index-list (Capa 4)
         └─ Define: type-index, type-lookup
              ↓
    functor.lisp ........... Functores Prolog
         ├─ Depende: prologc (inferior)
         └─ Define: functor, prolog-compile
              ↓
    prologc.lisp ........... Motor Prolog
         ├─ Depende: serialize (Capa 4)
         └─ Define: var, unify, prove, clauses
              ↓
    views.lisp ............. Vistas
         ├─ Depende: skip-list (Capa 4)
         ├─ Usa: functor, prologc
         └─ Define: view, map-reduce, regenerate
```

## Archivos Detallados

### 1. `ve-index.lisp` (194 líneas)

**Propósito:** Índice **Vertex-Edge** - buscar aristas salientes/entrantes de un vértice rápidamente.

**¿Por qué?**

```
SIN ÍNDICE:
  lookup-outgoing-edges(V)
    → Recorrer TODAS las aristas
    → O(E) - catastrófico en grafos densos

CON VE-INDEX:
  lookup-outgoing-edges(V)
    → Hash lookup + skip-list search
    → O(log E_V) donde E_V = aristas de V
```

#### 1.1 Estructura VE-Key

```lisp
(defstruct (ve-key
             (:print-function ...))
  (id +null-key+ :type (simple-array (unsigned-byte 8) (16)))  ; UUID del vértice
  (type-id 0 :type (integer 0 65535)))                         ; Tipo arista
```

**Layout en Memoria:**

```
VE-Key: [16 bytes UUID] [2 bytes type-id]
        ├─ Vértice ID (dirección)
        └─ Tipo arista (para múltiples relaciones)

Ejemplo:
  VE-Key { V1, :friend }
  → Todas las aristas :friend salientes de V1
```

#### 1.2 Estructura VE-Index

```lisp
(defstruct (ve-index
             (:constructor %make-ve-index))
  table                           ; Linear-hash (Capa 4)
  (cache (make-ve-cache)))        ; Cache débil (weak references)
```

**Qué contiene:**

```
ve-index.table: Linear-hash
  Clave: VE-Key { V1, :friend }
  Valor: index-list de aristas
         → [E1, E2, E3, ...]
         
Ejemplo:
  hash[ VE-Key {V_alice, :friend} ] 
    → index-list [E_alice→bob, E_alice→charlie, E_alice→diana]
```

#### 1.3 Operaciones

```lisp
(defun make-ve-index (location)
  "Crear nuevo VE-Index"
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
  "Buscar aristas para VE-Key, O(log n) promedio"
  (or (gethash key (ve-index-cache (ve-index-in graph)))
      (let ((table (ve-index-table (ve-index-in graph))))
        (with-locked-hash-key (table key)
          (let ((il (lhash-get table key)))
            (when il
              (cache-index-list (ve-index-in graph) key il)
              il))))))

(defmethod add-to-ve-index ((v vertex) (e edge) (graph graph))
  "Añadir arista al VE-Index"
  ;; Entrada: VE-Key { from(e), type(e) }
  ;;          → index-list con e
  (let ((ve-key (make-ve-key :id (id v) :type-id (edge-type-id e))))
    (or (lookup-ve-in-index-list ve-key graph)
        (let ((il (make-index-list)))
          (cache-index-list (ve-index-in graph) ve-key il)
          il))))
```

**Flujo de Búsqueda:**

```
lookup-outgoing-edges(alice, :friend)
    ↓
Crear VE-Key { alice_id, :friend }
    ↓
Hash lookup en linear-hash
    ↓
¿Encontrado en caché?
    ├─ Sí: retorna inmediatamente
    └─ No: búsqueda en linear-hash
    ↓
Retorna index-list de aristas
    ↓
Recorrer con cursor (O(1) por arista)
```

### 2. `vev-index.lisp` (219 líneas)

**Propósito:** Índice **Vertex-Edge-Vertex** - buscar aristas específicas V1→tipo→V2 rápidamente.

#### 2.1 Estructura VEV-Key

```lisp
(defstruct (vev-key
             (:print-function ...))
  (out-id +null-key+ :type (simple-array (unsigned-byte 8) (16)))  ; UUID vértice origen
  (in-id +null-key+ :type (simple-array (unsigned-byte 8) (16)))   ; UUID vértice destino
  (type-id 0 :type (integer 0 65535)))                             ; Tipo arista
```

**Layout en Memoria:**

```
VEV-Key: [16 bytes out-id] [16 bytes in-id] [2 bytes type-id]
        ├─ Vértice origen
        ├─ Vértice destino
        └─ Tipo arista (relación)

Ejemplo:
  VEV-Key { alice_id, bob_id, :friend }
  → Arista alice :friend→ bob (si existe)
```

#### 2.2 Estructura VEV-Index

```lisp
(defstruct (vev-index
             (:constructor %make-vev-index))
  table                           ; Linear-hash
  (cache (make-vev-cache)))       ; Cache débil
```

#### 2.3 Operaciones

```lisp
(defun make-vev-index (location)
  "Crear VEV-Index"
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
  "Buscar arista específica, O(log n) promedio"
  (let ((vev-key (make-vev-key 
                  :out-id (id from)
                  :in-id (id to)
                  :type-id (get-type-id type))))
    (lhash-get (vev-index-table (vev-index graph)) vev-key)))

(defmethod add-to-vev-index ((e edge) (graph graph))
  "Registrar arista en VEV-Index"
  (let ((vev-key (make-vev-key 
                  :out-id (id (from e))
                  :in-id (id (to e))
                  :type-id (edge-type-id e))))
    (lhash-put (vev-index-table (vev-index graph)) 
               vev-key 
               e)))
```

**Ventaja:**

```
Consultas:
  "¿Hay arista alice :friend→ bob?"
    → Hash lookup O(1) promedio
    → Sin recorrer todas las aristas
```

### 3. `type-index.lisp` (70 líneas)

**Propósito:** Índice **Type-based** - acceso rápido a nodos por tipo.

#### 3.1 Estructura Type-Index

```lisp
(defstruct (type-index
             (:constructor %make-type-index))
  table                           ; Memory-mapped file
  (locks (map-into (make-array +max-node-types+)
                   'make-lock))  ; Lock por tipo
  (cache ...)                     ; Caché hash table
```

**Layout:**

```
Memory-mapped file con +max-node-types+ slices
cada slice = index-list para un tipo

Type-Index:
  ├─ Type 0 (user):    [V1, V3, V5, ...]
  ├─ Type 1 (company): [V2, V4, V6, ...]
  ├─ Type 2 (post):    [V7, V8, V9, ...]
  └─ ...
```

#### 3.2 Operaciones

```lisp
(defun make-type-index (location heap)
  "Crear Type-Index con +max-node-types+ slots"
  (let* ((table (mmap-file location 
                           :size (* +max-node-types+ +index-list-bytes+)))
         (idx (%make-type-index :table table)))
    ;; Inicializar cada tipo con index-list vacío
    (dotimes (i +max-node-types+)
      (let ((offset (* i +index-list-bytes+)))
        (let ((index-list (make-index-list heap)))
          (serialize-index-list table index-list offset)
          (setf (gethash i (type-index-cache idx)) index-list))))
    idx))

(defmethod type-index-push ((uuid array) (type-id integer) 
                            (idx type-index) &key unless-present)
  "Añadir nodo al type-index"
  (let ((lock (aref (type-index-locks idx) type-id)))
    (with-lock (lock)
      (let ((il (gethash type-id (type-index-cache idx))))
        (if unless-present
            (index-list-pushnew uuid il)  ; Solo si no existe
            (index-list-push uuid il))    ; Siempre añadir
        ;; Serializar cambios a disco
        (serialize-index-list (type-index-table idx)
                              il
                              (* type-id +index-list-bytes+))))))

(defmethod get-all-of-type ((type-id integer) (graph graph))
  "Obtener todos los nodos de tipo, O(1) acceso"
  (gethash type-id (type-index-cache (type-index graph))))
```

**Uso:**

```lisp
;; Recuperar todos los usuarios
(let ((users (get-all-of-type :user graph)))
  (map-index-list #'process-user users))
```

### 4. `functor.lisp` (64 líneas)

**Propósito:** Definir **functores Prolog** - predicados compilados para queries.

**Contenido:**

```lisp
(defstruct (functor
             (:constructor %make-functor)
             (:predicate functor-p))
  name                    ; Nombre del functor (símbolo)
  fn                      ; Función compilada
  clauses                 ; Clauses Prolog
  (lock (make-recursive-lock)))

(defvar *user-functors*
  "Hash table de functores definidos por usuario"
  (make-hash-table :synchronized t))

(defun make-functor (&key name clauses)
  "Definir nuevo functor"
  (or (lookup-functor name)
      (let ((functor (%make-functor :name name :clauses clauses)))
        (with-recursive-lock-held ((functor-lock functor))
          ;; Compilar a función Lisp
          (prog1
              (setf (gethash name *user-functors*) functor)
            (prolog-compile functor))))))

(defun add-functor-clause (functor clause)
  "Añadir cláusula a functor existente"
  (with-recursive-lock-held ((functor-lock functor))
    ;; Usar atomic CAS para agregar
    #+sbcl
    (sb-ext:cas (cdr (last (functor-clauses functor)))
                (cdr (last (functor-clauses functor)))
                (list clause))
    
    ;; Recompilar
    (prolog-compile functor)
    (functor-clauses functor)))

(defun delete-functor (functor)
  "Eliminar functor"
  (remhash (functor-name functor) *user-functors*))

(defun reset-functor (functor)
  "Borrar todas las clauses de functor"
  (with-recursive-lock-held ((functor-lock functor))
    (setf (functor-clauses functor) nil)
    (prolog-compile functor)))
```

**Ejemplo de Uso:**

```lisp
;; Definir predicado friend/2
(make-functor 
  :name 'friend
  :clauses '(
    ;; Cláusula 1: Alice y Bob son amigos
    ((friend alice bob))
    ;; Cláusula 2: Bob y Alice son amigos
    ((friend bob alice))
    ;; Cláusula 3: Si X e Y son amigos, Y y X son amigos (simetría)
    ((friend ?X ?Y) (friend ?Y ?X))
    ))

;; Ahora puedo hacer queries:
(prove '(friend alice bob))      ;; → T
(prove '(friend bob alice))      ;; → T
(prove '(friend charlie diana))  ;; → NIL
```

### 5. `views.lisp` (732 líneas)

**Propósito:** **Vistas Materializadas** - cachés de queries complejas con map-reduce.

**¿Qué es una vista?**

```
VIEW = Cached query result que se actualiza cuando datos cambian

Ejemplo:
  VIEW user-posts
    MAP: Para cada usuario, lista de posts que creó
    REDUCE: Suma, contar, agrupar, etc.
    
  Sin vista:
    Cada query itera ALL users → ALL posts → filter
    O(U * P) por query
    
  Con vista:
    Precalculado: user → [list of posts]
    O(1) lookup + O(n_posts) iterate
```

#### 5.1 Estructura View

```lisp
(defstruct (view
             (:print-function ...))
  name                    ; Nombre de la vista
  class-name              ; Clase de nodos (ej: 'user)
  map-fn                  ; Función MAP
  map-code                ; Código MAP compilado
  reduce-fn               ; Función REDUCE
  reduce-code             ; Código REDUCE compilado
  graph-name              ; Grafo de referencia
  heap                    ; Memory allocator
  pointer                 ; Dirección en heap
  skip-list               ; Skip-list para resultados
  (lock (make-rw-lock))   ; RW-lock
  lookup-fn               ; Función para lookup directo
  (sort-order :lessp))    ; Orden de sort
```

#### 5.2 Vista de Grupo

```lisp
(defstruct view-group
  class-name              ; Clase (ej: 'user)
  (table ...)             ; Hash table de vistas
  (lock (make-rw-lock)))  ; RW-lock para el grupo
```

#### 5.3 Operaciones

```lisp
(defmethod map-view (fn (class-name symbol) (view-name symbol)
                     &key (graph *graph*) key start-key end-key 
                       count skip collect-p)
  "Iterar sobre vista"
  (if (lookup-view-group class-name graph)
      (let ((view (lookup-view graph class-name view-name)))
        (unless view
          (error 'invalid-view-error :class-name class-name))
        
        ;; Crear cursor (posiblemente rango)
        (let* ((cursor (if (and (null start-key) (null key) (null end-key))
                          (make-cursor (view-skip-list view))
                          (make-range-cursor (view-skip-list view)
                                           start-key end-key)))
               (result nil) (found-count 0))
          
          ;; Iterar y aplicar función
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
  "Recalcular vista (full rebuild)"
  ;; 1. Ejecutar MAP sobre todos los nodos
  (let ((view (lookup-view graph class-name view-name))
        (*view-rv* nil))  ; Result vector
    
    ;; 2. Para cada nodo de esta clase
    (with-read-locked-view-group (class-name graph)
      (map-vertices 
       (lambda (node)
         (when (eq (type-of node) class-name)
           ;; 3. Ejecutar map-fn
           (funcall (view-map-fn view) node)
           ;; 4. Resultados van a *view-rv*
           ))
       graph))
    
    ;; 5. Insertar todos en skip-list
    (dolist (result *view-rv*)
      (add-to-skip-list (view-skip-list view) 
                       (car result) (cdr result)))))

(defmethod restore-views ((graph graph))
  "Cargar vistas desde disco al iniciar"
  (let ((views-file (format nil "~A/views.dat" (location graph))))
    (when (probe-file views-file)
      (let ((blob (cl-store:restore views-file)))
        (dolist (view-data blob)
          ;; Restaurar cada vista
          ...)))))
```

**Ejemplo:**

```lisp
;; Crear vista: "posts by user"
(defmethod make-user-posts-view ((graph graph))
  (make-view 
   :name 'by-user
   :class-name 'user
   :map-fn (lambda (user-node)
             ;; Para cada post de este usuario
             (let ((user-id (id user-node)))
               (map-edges
                (lambda (edge)
                  (when (eq (edge-type edge) :created)
                    (let ((post-id (to edge)))
                      ;; Yield (user-id, post-id) → post
                      (yield (list user-id post-id) post))))
                graph)))
   :graph-name (graph-name graph)))

;; Usar vista para búsquedas rápidas:
(map-view (lambda (user-id post-id post)
            (format t "User ~A created post ~A~%" user-id post))
         'user 'by-user
         :start-key alice-id
         :end-key bob-id
         :limit 10)
```

### 6. `prologc.lisp` (717 líneas)

**Propósito:** Motor **Prolog completo** basado en Norvig's PAIP (Paradigms of AI Programming).

**¿Qué es Prolog?**

```
Prolog = Logic programming + Backtracking + Unification

Ejemplo:

  Hechos:
    (friend alice bob)
    (friend bob charlie)
    
  Reglas:
    (mutual-friend ?X ?Y) :- 
      (friend ?X ?Y), 
      (friend ?Y ?X)
    
  Query:
    ?- mutual-friend alice bob
    
  Resolución:
    1. Buscar clauses que unifiquen con mutual-friend
    2. Intentar probar (friend alice bob)
    3. Buscar en base de datos → MATCH
    4. Intentar probar (friend bob alice)
    5. Buscar en base de datos → MATCH
    6. ✓ ÉXITO
```

#### 6.1 Variables Prolog

```lisp
(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*))        ; Identificador único
  (binding +unbound+))               ; Valor ligado

(defun bound-p (var) 
  "¿Variable está ligada?"
  (not (eq (var-binding var) +unbound+)))

(defmacro var-deref (exp)
  "Dereferenciar variable (seguir pointers)"
  `(progn 
     (loop while (and (var-p ,exp) (bound-p ,exp))
        do (setf ,exp (var-binding ,exp)))
     ,exp))
```

**Uso:**

```lisp
(let ((X (?)))       ; Nueva variable _X1
  ;; Inicialmente sin ligar
  (assert (not (bound-p X)))
  
  ;; Ligar a valor
  (unify X 'alice)
  (assert (bound-p X))
  
  ;; Dereferenciar
  (assert (eq (var-deref X) 'alice)))
```

#### 6.2 Unificación

```lisp
(defgeneric prolog-equal (x y)
  "¿Iguales según Prolog?"
  (:method ((x number) (y number)) (= x y))
  (:method ((x string) (y string)) (string= x y))
  (:method ((x node) (y node)) (equalp (id x) (id y)))
  (:method (x y) (equal x y)))

(defun unify (x y)
  "Unificar dos expresiones (destructivo)"
  (cond
    ;; Ambas dereferenciadas y iguales
    ((prolog-equal (var-deref x) (var-deref y)) t)
    
    ;; X es variable: ligar a Y
    ((var-p x) (set-binding x y))
    
    ;; Y es variable: ligar a X
    ((var-p y) (set-binding y x))
    
    ;; Ambas listas: unificar recursivamente
    ((and (consp x) (consp y))
     (and (unify (first x) (first y))
          (unify (rest x) (rest y))))
    
    ;; No unifican
    (t nil)))

(defun set-binding (var value)
  "Ligar variable, guardando en trail para backtracking"
  (unless (eq var value)
    ;; Guardar en trail para poder deshacer
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)
```

**Ejemplo:**

```lisp
(let ((X (?))
      (Y (?)))
  ;; Unificar X con 'alice'
  (unify X 'alice)
  ;; Unificar Y con X (que está ligado a 'alice')
  (unify Y X)
  ;; Ahora ambas apuntan a 'alice'
  (assert (eq (var-deref Y) 'alice)))
```

#### 6.3 Backtracking y Trail

```lisp
(defvar *trail*
  "Vector de variables para deshacimiento en backtracking"
  (make-array 0 :fill-pointer 0 :adjustable t))

(defun undo-bindings (old-trail)
  "Deshacer bindings hasta checkpoint"
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) +unbound+)))

(defun save-trail ()
  "Guardar punto de backtracking"
  (fill-pointer *trail*))
```

**Patrón de backtracking:**

```lisp
(defun prove-goals (goals bindings depth)
  "Probar lista de goals con backtracking"
  (if (null goals)
      ;; Base case: todos los goals exitosos
      (return-from prove-goals bindings)
      
      (let ((goal (first goals)))
        ;; Guardar punto de backtracking
        (let ((saved-trail (save-trail)))
          ;; Intentar cada posible cláusula
          (dolist (clause (get-clauses goal))
            ;; Unificar goal con cláusula
            (if (unify goal (clause-head clause))
                ;; Probar subgoals
                (let ((result (prove-goals 
                              (append (clause-body clause)
                                     (rest goals))
                              bindings
                              (1+ depth))))
                  (if result
                      (return-from prove-goals result)
                      ;; Backtrack: deshacer bindings
                      (undo-bindings saved-trail)))))))
        
        ;; Fallo total: sin cláusulas aplicables
        nil)))
```

#### 6.4 Compilación de Functores

```lisp
(defmethod prolog-compile ((functor functor))
  "Compilar clauses a función Lisp ejecutable"
  (if (null (functor-clauses functor))
      ;; Sin clauses: compilar a función que siempre falla
      (prolog-compile-null functor)
      
      ;; Compilar por arity
      (prolog-compile-help functor (functor-clauses functor))))

(defun compile-functor (functor arity clauses)
  "Generar función Lisp para functor/arity específico"
  (let ((params (make-parameters arity)))
    ;; Generar cuerpo que intenta cada cláusula
    (let ((body
           `(let ((old-trail (fill-pointer *trail*)))
              (or 
                ;; Intentar cada cláusula
                ,@(mapcar (lambda (clause)
                            (compile-clause params clause))
                         clauses)
                ;; Todas fallaron: backtrack
                (progn
                  (undo-bindings old-trail)
                  nil)))))
      ;; Compilar a función
      (compile functor (lambda (&rest args) 
                         (apply body args))))))
```

## Índices Especializados

### Comparación

```
BÚSQUEDA               SIN ÍNDICE    CON ÍNDICE
─────────────────────────────────────────────────
¿Aristas de V?        O(E)          O(log E_V)
¿Arista V1→V2?        O(E)          O(log E_V)
¿Nodos tipo X?        O(V)          O(1)
Camino V1→V2          O(V*E)        O((V+E) log E)

E = total aristas
V = total vértices
E_V = aristas de V
```

## Motor Prolog

### Patrón de Prueba

```
PROVE { friend(alice, ?X) }
  ├─ Unificar con friend(alice, bob)
  │  ├─ friend = friend ✓
  │  ├─ alice = alice ✓
  │  └─ ?X = bob ✓ (ligar X a bob)
  └─ ÉXITO con ?X = bob

PROVE { friend(alice, ?X), likes(?X, pizza) }
  ├─ Probar friend(alice, ?X)
  │  └─ ?X = bob
  ├─ Probar likes(bob, pizza)
  │  ├─ ¿Existe cláusula likes(bob, pizza)?
  │  └─ Sí ✓
  └─ ÉXITO
```

### Trail para Backtracking

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

## Vistas Materializadas

### Map-Reduce Pattern

```
VIEW user-posts:

MAP FUNCTION:
  Para cada usuario U:
    Para cada arista E (creó):
      Yield (U, post_id) → post

Ejemplo:
  User alice creó:
    - "Hello World" (post_1)
    - "Goodbye" (post_2)
  
  User bob creó:
    - "Hi" (post_3)
  
  Resultados:
    (alice, post_1) → post_1
    (alice, post_2) → post_2
    (bob, post_3) → post_3

REDUCE FUNCTION (opcional):
  Agrupar por usuario:
    alice → [post_1, post_2]
    bob → [post_3]
```

## Orden de Carga

```
ORDEN DE CARGA - CAPA 5
═══════════════════════════════════════════════════════════

Desde CAPA 4 (skip-list, linear-hash, index-list):
    ↓
ve-index.lisp .............. VE-Index
    ├─ Depende: linear-hash
    └─ Define: ve-index, lookup-ve
         ↓
    vev-index.lisp ......... VEV-Index
         ├─ Depende: linear-hash
         └─ Define: vev-index, lookup-vev
              ↓
    type-index.lisp ........ Type-Index
         ├─ Depende: index-list, mmap
         └─ Define: type-index, get-all-of-type
              ↓
    functor.lisp ........... Functores Prolog
         ├─ Depende: prologc
         └─ Define: functor, prolog-compile
              ↓
    prologc.lisp ........... Motor Prolog
         ├─ Depende: serialize (Capa 4)
         └─ Define: var, unify, prove, trail
              ↓
    views.lisp ............. Vistas
         ├─ Depende: skip-list (Capa 4)
         ├─ Usa: functor, prologc
         └─ Define: view, map-reduce, regenerate
```

## Resumen

La **Capa 5** proporciona:

1. ✓ **VE-Index (194 líneas)** - Buscar aristas salientes O(log n)
2. ✓ **VEV-Index (219 líneas)** - Buscar arista específica O(log n)
3. ✓ **Type-Index (70 líneas)** - Acceso por tipo O(1)
4. ✓ **Functores (64 líneas)** - Definir predicados Prolog
5. ✓ **Vistas (732 líneas)** - Vistas materializadas map-reduce
6. ✓ **Prolog (717 líneas)** - Motor Prolog completo

**Total:** ~1,996 líneas que proporcionan:
- Queries eficientes sobre grafos
- Logic programming (Prolog)
- Vistas precalculadas
- Unificación y backtracking
- Multi-index para acceso rápido

En las capas siguientes (6-7), se usa esta indexación para APIs de alto nivel.


*Documentación de la Capa 5 de VivaceGraph*
*Marzo 2026*
