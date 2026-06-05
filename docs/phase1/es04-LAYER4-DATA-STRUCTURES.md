# VivaceGraph - Capa 4: Estructuras de Datos

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Propósito y Responsabilidades](#propósito-y-responsabilidades)
3. [Componentes de la Capa 4](#componentes-de-la-capa-4)
4. [Archivos Detallados](#archivos-detallados)
5. [Skip Lists - Estructura Probabilística](#skip-lists---estructura-probabilística)
6. [Linear Hashing - Tabla Hash Dinámica](#linear-hashing---tabla-hash-dinámica)
7. [Memoria y Allocación](#memoria-y-allocación)
8. [Serialización](#serialización)
9. [Orden de Carga](#orden-de-carga)

---

## Visión General

La **Capa 4** proporciona **estructuras de datos eficientes** que VivaceGraph necesita para:

- **Skip Lists:** Índices ordenados con búsqueda O(log n)
- **Linear Hash:** Tabla hash que crece dinámicamente
- **Allocator:** Gestor de memoria con bins y fragmentación
- **Buffer Pool:** Pool de objetos reutilizables
- **Serialización:** Conversión de objetos a/desde bytes
- **Índices:** Wrappers sobre skip lists para diferentes tipos de datos

### Características Clave

- ✓ **Skip Lists:** O(log n) búsqueda sin necesidad de rebalanceo
- ✓ **Linear Hashing:** O(1) promedio, crece dinámicamente
- ✓ **Allocator:** Fragmentación eficiente, múltiples bins
- ✓ **Buffer Pool:** Reutilización de objetos para evitar GC
- ✓ **Serialización:** Tipo-agnóstica, extensible
- ✓ **Persistencia:** Todas las estructuras en memoria mapeada

### Líneas de Código

```
Archivo                    Líneas
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
TOTAL                       3501 líneas
```

---

## Propósito y Responsabilidades

### ¿Por qué existe la Capa 4?

VivaceGraph necesita estructuras de datos que:
1. **Funcionen en memoria persistente** (no solo RAM)
2. **Crezcan dinámicamente** (sin recolección)
3. **Sean eficientes** (logarítmicas, no lineales)
4. **Soporten índices** (para búsquedas rápidas)

### Responsabilidades Específicas

| Responsabilidad | Archivo | Razón |
|-----------------|---------|-------|
| Índices ordenados | `skip-list.lisp` | Búsqueda binaria logarítmica |
| Iteradores skip-list | `skip-list-cursors.lisp` | Recorrer skip lists |
| Tabla hash dinámica | `linear-hash.lisp` | Hash con auto-crecimiento |
| Gestión de memoria | `allocator.lisp` | Bins, free-lists |
| Reutilización objetos | `buffer-pool.lisp` | Pool para no fragmen memory |
| Tipo-conversión | `serialize.lisp` | Objetos ↔ bytes |
| Generación IDs | `node-id.lisp` | UUIDs v5 SHA1 |
| Wrapppers índices | `index.lisp` | Interfaz sobre skip-lists |
| Índices en memoria | `index-list.lisp` | Listas enlazadas persistentes |
| Vectores índices | `index-vector.lisp` | Arrays persistentes |

---

## Componentes de la Capa 4

### Diagrama de Dependencias

```
CAPA 4 - DEPENDENCIAS INTERNAS
================================

Desde CAPA 3 (allocator, buffer-pool):
    ↓
skip-list.lisp ............... Skip lists
    ├─ Depende: allocator, buffer-pool
    ├─ Usa: random-level (Mersenne Twister)
    └─ Define: skip-node, skip-list
         ↓
    skip-list-cursors.lisp ... Iteradores
         ├─ Depende: skip-list.lisp
         └─ Define: cursor, range-cursor, value-cursor
              ↓
    linear-hash.lisp ......... Tabla hash dinámica
         ├─ Depende: allocator, buffer-pool
         └─ Define: lhash, buckets, overflow
              ↓
    allocator.lisp ........... Memory management
         ├─ Depende: CAPA 2 (mmap, pmem)
         └─ Define: memory, free-list, bins
              ↓
    buffer-pool.lisp ......... Pool de objetos
         ├─ Depende: allocator
         └─ Define: buffer-pool, recycling
              ↓
    serialize.lisp ........... Serialización tipo-agnóstica
         ├─ Depende: buffer-pool
         └─ Define: tipo-tags, encoding
              ↓
    node-id.lisp ............. Generación de IDs
         ├─ Depende: random, uuid, sha1 (ironclad)
         └─ Define: gen-vertex-id, gen-edge-id
              ↓
    index.lisp ............... Interfaz índices
         ├─ Depende: skip-list
         └─ Define: index wrapper
              ↓
    index-list.lisp .......... Listas persistentes
         ├─ Depende: allocator, pcons (Capa 2)
         └─ Define: index-list, linked-lists
              ↓
    index-vector.lisp ........ Vectores persistentes
         ├─ Depende: allocator
         └─ Define: index-vector, arrays persistentes
```

---

## Archivos Detallados

### 1. `skip-list.lisp` (888 líneas) - **ARCHIVO CLAVE**

**Propósito:** Implementar **skip lists** - estructura probabilística para búsqueda logarítmica ordenada.

**¿Qué es un Skip List?**

```
LISTA NORMAL (búsqueda lineal O(n)):
┌─┬─┬─┬─┬─┬─┬─┬─┬─┐
│1│2│3│4│5│6│7│8│9│
└─┴─┴─┴─┴─┴─┴─┴─┴─┘
 ↑ recorrer hasta encontrar

SKIP LIST (búsqueda O(log n)):
Nivel 3: ┌─────────────────────────┐
         │1                        9│
Nivel 2: │1     │3     │5     │8  9│
Nivel 1: │1│2 │3│4│5│6│7│8  9│
Nivel 0: │1│2│3│4│5│6│7│8│9│
         └─┴─┴─┴─┴─┴─┴─┴─┴─┘

Buscar 7:
  Nivel 3: 1 < 7, salta a 9
  Nivel 2: 1 < 7, prueba 3, 3 < 7, prueba 5, 5 < 7, prueba 8, 8 > 7
           vuelve a 5
  Nivel 1: 5 < 7, prueba 6, 6 < 7, prueba 7, ENCONTRADO
  
Comparaciones: ~3 (en lugar de 7)
```

#### 1.1 Constantes y Estructuras

```lisp
(alexandria:define-constant +max-level+ 64
  "Máximo nivel de skip-list (enough para 2^64 elementos)")

(alexandria:define-constant +skip-list-header-size+ 25
  "Bytes para: type, count, head, tail pointers")

;; ESTRUCTURA DE UN NODO:
;; ├─ Size (8 bytes) - tamaño total del nodo
;; ├─ Level (1 byte) - nivel actual de este nodo
;; ├─ Flags (1 byte) - marked, fully-linked, locked
;; ├─ Pointers (level * 8 bytes) - forward pointers
;; ├─ Key (variable) - clave serializada
;; └─ Value (variable) - valor serializado
```

#### 1.2 Estructura Skip-Node

```lisp
(defstruct (skip-node
             (:conc-name %sn-)
             (:predicate skip-node-p))
  (addr 0 :type (unsigned-byte 64))      ; Dirección en heap
  (size 0 :type (unsigned-byte 64))      ; Tamaño bytes
  (level 0 :type (unsigned-byte 8))      ; Nivel actual
  key                                     ; Clave (en RAM, caché)
  skey                                    ; Clave serializada
  value                                   ; Valor (en RAM)
  svalue                                  ; Valor serializado
  pointers                                ; Array de forward pointers
  flags                                   ; Byte de flags
  (head-p nil)                            ; ¿Es head?
  (tail-p nil))                           ; ¿Es tail?
```

**Layout en Memoria (disco):**

```
Dirección heap: 0x1000
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

#### 1.3 Estructura Skip-List

```lisp
(defstruct (skip-list
             (:conc-name %sl-))
  (length 0 :type (unsigned-byte 64))       ; Número de nodos
  (max-level +max-level+ :type fixnum)      ; Nivel máximo
  (head nil)                                ; Nodo sentinela (head)
  (tail nil)                                ; Nodo sentinela (tail)
  (heap nil)                                ; Memory allocator
  (mmap nil)                                ; Memory-mapped file
  
  ;; Funciones de comparación/igualdad
  (key-equal #'equal)                       ; ¿Claves iguales?
  (key-comparison #'<)                      ; ¿Clave1 < Clave2?
  
  ;; Funciones de serialización
  (key-serializer #'serialize)              ; Clave → bytes
  (key-deserializer #'deserialize)          ; bytes → Clave
  (value-serializer #'serialize)            ; Valor → bytes
  (value-deserializer #'deserialize)        ; bytes → Valor
  
  ;; Sentinelas
  (head-key nil)                            ; Clave mínima (-∞)
  (head-value nil)                          ; Valor dummy
  (tail-key nil)                            ; Clave máxima (+∞)
  (tail-value nil)                          ; Valor dummy
  
  ;; Configuración
  (duplicates-allowed-p t)                  ; ¿Permitir duplicados?
  
  ;; Caché y locks
  (node-cache (make-hash-table))            ; Caché addr → skip-node
  (lock (make-rw-lock)))                    ; RW lock por lista
```

#### 1.4 Operaciones Principales

```lisp
(defun make-skip-list (&key heap key-equal key-comparison ...)
  "Crear nueva skip-list"
  (let ((sl (%make-skip-list ...)))
    (let ((head (make-head sl ...))
          (tail (make-tail sl head ...)))
      sl)))

(defun add-to-skip-list (skip-list key value)
  "Añadir key→value, O(log n)"
  ;; 1. Encontrar posición
  (find-in-skip-list sl key)
  ;; 2. Crear nodo con nivel aleatorio
  (let ((level (random-level)))
    (make-skip-node sl key value level))
  ;; 3. Insertar en todos los niveles
  (loop for i from 0 below level
        do (update-pointer pred i new-node))
  ;; 4. Marcar fully-linked
  (set-node-fully-linked sl node)
  ...)

(defun find-in-skip-list (skip-list key &optional preds succs)
  "Buscar clave, O(log n)"
  ;; Retorna: (node, level-found, preds, succs)
  (with-read-lock ((skip-list-lock skip-list))
    (let ((pred (%sl-head skip-list))
          (level-found -1))
      ;; Descender desde nivel máximo
      (loop for level from (%sl-max-level skip-list) downto 0
            do
            ;; En cada nivel, avanzar mientras sea menor
            (loop for node = (read-skip-node sl 
                                             (aref pointers pred level))
                  while (funcall (%sl-key-comparison sl)
                                (%sn-key node) key)
                  do (setq pred node))
            ;; Registrar predecessors/successors
            (setf (aref preds level) pred
                  (aref succs level) next-node)
            ;; ¿Encontrado?
            (when (funcall (%sl-key-equal sl)
                          (%sn-key next-node) key)
              (setq level-found level)))
      (values found-node level-found preds succs))))

(defun remove-from-skip-list (skip-list key)
  "Remover clave, O(log n)"
  (with-write-lock ((skip-list-lock skip-list))
    (let ((node (find-in-skip-list sl key)))
      (when node
        ;; Marcar como deleted
        (mark-node sl node)
        ;; Retirar de todos los niveles
        (loop for i from 0 below (%sn-level node)
              do (set-node-pointer sl pred i successor))
        ;; Liberar memoria
        (free (sl-heap sl) (sn-addr node))
        t))))
```

**Niveles Aleatorios:**

```lisp
(defun random-level (&optional (max-level +max-level+))
  "Nivel aleatorio siguiendo patrón de Pugh"
  ;; L1: 50%, L2: 25%, L3: 12.5%, etc.
  (do ((level 1 (1+ level)))
      ((or (= level max-level)
           (= (random 4) 3))  ; 1 en 4 probabilidad
       level)))
```

**Complejidad:**

```
Operación      Peor Caso    Promedio    Espacio
────────────────────────────────────────────────
Búsqueda       O(n)         O(log n)    O(1)
Inserción      O(n)         O(log n)    O(log n)
Borrado        O(n)         O(log n)    O(1)
```

---

### 2. `skip-list-cursors.lisp` (122 líneas)

**Propósito:** Iteradores para recorrer skip lists.

**Contenido:**

```lisp
(defclass skip-list-cursor (cursor)
  ((node :initarg :node :accessor skip-list-cursor-node)
   (skip-list :initarg :skip-list :accessor skip-list)))

(defmethod cursor-next ((slc skip-list-cursor) &optional eoc)
  "Obtener siguiente nodo"
  (with-slots (node) slc
    (if (tail-p node)
        eoc  ; End of collection
        (let ((result node))
          (setq node (node-forward (skip-list slc) node))
          result))))

(defclass skip-list-value-cursor (skip-list-cursor)
  ;; Cursor que retorna solo valores
  ())

(defmethod cursor-next :around ((slc skip-list-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (%sn-value result))))

(defclass skip-list-key-cursor (skip-list-cursor)
  ;; Cursor que retorna solo claves
  ())

(defclass skip-list-range-cursor (skip-list-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod cursor-next :around ((slc skip-list-range-cursor) &optional eoc)
  "Cursor limitado a rango [start, end)"
  (with-slots (node end) slc
    (if (funcall (%sl-key-comparison sl)
                (%sn-key node) end)
        (call-next-method)
        eoc)))
```

**Patrones de Uso:**

```lisp
;; Iterar sobre todos
(let ((cursor (make-cursor sl)))
  (do ((node (cursor-next cursor) (cursor-next cursor)))
      ((null node))
    (process node)))

;; Iterar valores
(let ((cursor (make-values-cursor sl)))
  (do ((value (cursor-next cursor) (cursor-next cursor)))
      ((null value))
    (process-value value)))

;; Rango [start, end)
(let ((cursor (make-range-cursor sl "alice" "bob")))
  (do ((key (cursor-next cursor) (cursor-next cursor)))
      ((null key))
    (process-key key)))
```

---

### 3. `linear-hash.lisp` (764 líneas)

**Propósito:** Tabla hash dinámica que crece sin recolección completa.

**¿Qué es Linear Hashing?**

```
HASH TABLE NORMAL:
  Size = 16 buckets
  ┌──┬──┬──┬──┬──┬──┬──┬──┐
  │  │  │ X│ X│  │ X│  │  │
  └──┴──┴──┴──┴──┴──┴──┴──┘
  
  Insertar más: O(n) para rehashing completo

LINEAR HASHING:
  Size = 4 buckets (base-buckets)
  Level = 0
  Split pointer = 0
  
  Insertar → overflow?
    ├─ Sí: Expandir (split) bucket[0]
    │        bucket[0] → bucket[0], bucket[4]
    │        Level++
    │        Split++
    └─ No: Insert normal
  
  Ventaja: Incremental, no full rehash
```

#### 3.1 Estructura LHash

```lisp
(defstruct (lhash
             (:conc-name %lhash-))
  (test 'uuid-array-equal)        ; Función de igualdad
  (base-buckets 4)                ; Buckets iniciales
  (level 0)                        ; Nivel actual
  (key-bytes +key-bytes+)         ; Bytes de clave
  (value-bytes +value-bytes+)     ; Bytes de valor
  (bucket-size +bucket-size+)     ; Items por bucket
  (next-split 0)                  ; Próximo a split
  (next-overflow-pointer 1)       ; Próximo overflow
  (count 0)                        ; Total items
  
  count-lock                       ; Lock para count
  split-lock                       ; Lock para splits
  overflow-lock                    ; Lock para overflow
  
  table                            ; Hash table (buckets)
  overflow                         ; Overflow buckets
  location                         ; Archivo mmap
  lock-vector                      ; Locks por bucket
  
  key-serializer                   ; Clave → bytes
  key-deserializer                 ; bytes → Clave
  value-serializer                 ; Valor → bytes
  value-deserializer)              ; bytes → Valor
```

#### 3.2 Operaciones

```lisp
(defun lhash-get (lhash key)
  "Buscar en tabla, O(1) promedio"
  (let ((bucket-index (hash-key-to-bucket lhash key)))
    (let ((bucket (get-bucket lhash bucket-index)))
      (loop for entry in bucket
            when (funcall (%lhash-test lhash) key entry)
            return entry))))

(defun lhash-put (lhash key value)
  "Insertar, con resize si es necesario"
  (with-write-lock ((lhash-lock lhash))
    ;; 1. Encontrar bucket
    (let ((bucket-index (hash-key-to-bucket lhash key)))
    
    ;; 2. Verificar overflow
    (if (> (bucket-size lhash) threshold)
        ;; Expandir
        (split-bucket lhash)
        ;; Insertar normal
        (insert-in-bucket lhash bucket-index key value))))

(defun split-bucket (lhash)
  "Dividir bucket[next-split] en bucket[next-split] + new-bucket"
  ;; 1. Crear nuevo bucket
  (let ((new-bucket (create-bucket lhash)))
    ;; 2. Redistribuir entries de bucket[next-split]
    (loop for entry in (get-bucket lhash (%lhash-next-split lhash))
          do
          (let ((new-index (hash-key-to-bucket lhash (car entry))))
            (if (= new-index (%lhash-next-split lhash))
                (add-to-bucket lhash (%lhash-next-split lhash) entry)
                (add-to-bucket lhash new-bucket entry))))
    ;; 3. Actualizar state
    (incf (%lhash-next-split lhash))
    (when (>= (%lhash-next-split lhash) (1- (expt 2 (%lhash-level lhash))))
      (setf (%lhash-next-split lhash) 0)
      (incf (%lhash-level lhash)))))
```

**Ventajas vs Tradicional:**

```
TRADITIONAL HASHING (resize completo):
  Size: 4 → 8
  ┌─┬─┬─┬─┐    Rehash    ┌─┬─┬─┬─┬─┬─┬─┬─┐
  │X│X│X│X│ ─────────→  │ │ │ │ │ │ │ │ │
  └─┴─┴─┴─┘   O(n)       └─┴─┴─┴─┴─┴─┴─┴─┘
  
LINEAR HASHING (incremental):
  Size: 4, split 0       resize 1       resize 2
  ┌─┬─┬─┬─┐              ┌─┬─┬─┬─┬─┐   ┌─┬─┬─┬─┬─┬─┐
  │X│X│X│X│ ─O(1)→      │X│X│X│X│ │ → │X│X│X│X│ │ │
  └─┴─┴─┴─┘              └─┴─┴─┴─┴─┘   └─┴─┴─┴─┴─┴─┘
  
VENTAJA: Incremental, no pause O(n)
```

---

### 4. `allocator.lisp` (334 líneas)

**Propósito:** Gestor de memoria persistente con bins y free-lists.

**Contenido:**

```lisp
(defstruct (memory
             (:print-function ...))
  location                         ; Archivo location
  size                             ; Tamaño total
  mmap                             ; Memory-mapped file
  
  (free-list                       ; Free list por tamaño
   (make-hash-table :synchronized t))
  
  (pointer 0)                      ; Pointer actual
  (lock (make-rw-lock))            ; RW lock
  extent-size                      ; Tamaño de extensión
  data-offset                      ; Offset de datos
  
  (bin-locks                       ; Locks por bin
   (map-into (make-array 128) 'make-recursive-lock))
  
  (cache-lock (make-rw-lock))      ; Cache lock
  (cache                           ; Caché addr → objeto
   (make-hash-table :weakness :value)))
```

**Bins (Buckets de Tamaño):**

```
Bins para diferentes tamaños:
  Bin 0:   allocaciones de 8 bytes
  Bin 1:   allocaciones de 16 bytes
  Bin 2:   allocaciones de 32 bytes
  ...
  Bin 127: allocaciones de 16384+ bytes

Asignación:
  allocate(memory, 50 bytes)
    ↓
  50 cabe en Bin 3 (64 bytes)
    ↓
  ¿Hay free en Bin 3?
    ├─ Sí: reusar
    └─ No: allocate nuevo
```

**Operaciones:**

```lisp
(defun allocate (memory size)
  "Asignar memoria, O(1) si hay free"
  (with-write-lock ((memory-lock memory))
    ;; 1. Encontrar bin para size
    (let ((bin (size-to-bin size)))
    ;; 2. ¿Hay free en bin?
    (if (free-list-for-bin memory bin)
        ;; Reusar
        (let ((addr (pop (free-list-for-bin memory bin))))
          (initialize-allocation memory addr size)
          addr)
        ;; Allocate nuevo
        (let ((addr (memory-pointer memory)))
          (incf (memory-pointer memory) size)
          (initialize-allocation memory addr size)
          addr))))

(defun free (memory address)
  "Liberar memoria, devolver a free-list"
  (with-write-lock ((memory-lock memory))
    (let ((size (get-allocation-size memory address)))
      (let ((bin (size-to-bin size)))
        (push address (free-list-for-bin memory bin))))))

(defun grow-memory (memory length)
  "Extender archivo mmap"
  (let ((num-extents (ceiling (/ length (memory-extent-size memory)))))
    (extend-mapped-file (memory-mmap memory)
                        (* num-extents (memory-extent-size memory)))))
```

---

### 5. `buffer-pool.lisp` (424 líneas)

**Propósito:** Pool de objetos reutilizables para evitar fragmentación.

**Contenido:**

```lisp
(defvar *buffer-pool*
  "Global pool de buffers reutilizables"
  (make-hash-table :test 'eq :synchronized t))

(defvar *buffer-pool-stats*
  "Estadísticas del pool")

(defvar *buffer-pool-low-water-mark* 1000
  "Mínimo de buffers antes de refrescar")

(defvar *free-memory-low-water-mark* 10485760
  "Mínimo de free memory (10MB) antes de GC fuerza")

(defstruct (buffer-pool-stats
             (:conc-name bps-))
  (buffer-8 0)        ; Buffers de 8 bytes
  (buffer-16 0)       ; Buffers de 16 bytes
  (buffer-18 0)       ; Buffers de 18 bytes
  (pcons 0)           ; Persistent cons cells
  (vertex 0)          ; Vertex objects
  (edge 0)            ; Edge objects
  (skip-node 0))      ; Skip-list nodes
```

**Operaciones:**

```lisp
(defun get-buffer (size)
  "Obtener buffer de tamaño, desde pool si posible"
  #+sbcl (sb-ext:atomic-pop (first (gethash size *buffer-pool*)))
  #+lispworks (sys:atomic-pop (car (gethash size *buffer-pool*)))
  ;; Si no hay: crear uno
  (or existing (make-byte-vector size)))

(defun return-buffer (buffer)
  "Devolver buffer al pool"
  (let ((size (length buffer)))
    #+sbcl (sb-ext:atomic-push buffer (first (gethash size *buffer-pool*)))
    #+lispworks (sys:atomic-push buffer (car (gethash size *buffer-pool*)))))

(defun monitor-buffer-pool ()
  "Monitor thread que mantiene pool lleno"
  (loop
    (sleep 5)  ; Check cada 5 segundos
    (let ((free-memory (free-memory)))
      (if (<= free-memory *free-memory-low-water-mark*)
          ;; Force GC si memória baja
          #+sbcl (sb-ext:gc :full t)
          #+ccl (gc)
          #+lispworks (hcl:gc-generation 2)
          ;; Refrescar buffers si pool bajo
          (dolist (buffer-type '(8 16 18 24 34))
            (when (< (pool-count buffer-type) *buffer-pool-low-water-mark*)
              (dotimes (i 1000)
                (return-buffer (make-byte-vector buffer-type)))))))))
```

---

### 6. `serialize.lisp` (470 líneas)

**Propósito:** Serialización tipo-agnóstica de objetos.

**Contenido:**

```lisp
(defgeneric serialize (object)
  (:documentation "Convertir objeto a byte-array"))

(defgeneric deserialize (bytes)
  (:documentation "Convertir byte-array a objeto"))

;; Type tags (identificadores de tipo):
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

**Layout de Serialización:**

```
Tipos de tamaño fijo (1 byte tipo + 1 byte size + data):
┌──────┬───────┬────────────────────┐
│Type  │Size   │ Data               │
│+uuid │2      │16 bytes UUID       │
└──────┴───────┴────────────────────┘

Tipos de tamaño variable (1 byte tipo + N bytes length + data):
┌──────┬──┬──────┬────────────────────┐
│Type  │N │Length│ Data               │
│+string│1 │25    │"hello world..."    │
└──────┴──┴──────┴────────────────────┘
```

**Operaciones:**

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
  ;; Inspeccionar type tag
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

**Extensibilidad:**

```lisp
;; Usuario puede definir new types:
(defmethod serialize ((my-obj my-custom-class))
  (let ((v (make-byte-vector ...)))
    (setf (aref v 0) +my-custom-type+)  ; Nuevo tag
    ;; Serializar campos
    v))

(defmethod deserialize-my-custom ((bytes array) offset)
  ;; Deserializar
  (make-instance 'my-custom-class ...))
```

---

### 7. `node-id.lisp` (54 líneas)

**Propósito:** Generación de identificadores únicos (UUIDs v5).

**Contenido:**

```lisp
(defun generate-uuid-name ()
  "Generar nombre para UUID v5 usando tiempo + random"
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (sec msec)
      #+sbcl (sb-ext:get-time-of-day)
      #-sbcl (osicat-posix:gettimeofday)
    (let* ((total-bytes 40)
           (vec (make-array total-bytes :element-type '(unsigned-byte 8))))
      ;; Escribir segundos
      (let ((n-bytes (ceiling (integer-length sec) 8)))
        (dotimes (i n-bytes)
          (setf (aref vec i) (ldb (byte 8 (* i 8)) sec))))
      ;; Escribir milisegundos
      (let ((n-bytes (ceiling (integer-length msec) 8)))
        (dotimes (i n-bytes)
          (setf (aref vec (+ offset i)) (ldb (byte 8 (* i 8)) msec))))
      ;; Bytes aleatorios
      (loop for i from offset below total-bytes
            do (setf (aref vec i) (random 256)))
      vec)))

(defun gen-v5-uuid (namespace)
  "Generar UUID v5 (basado en nombre SHA1)"
  (let ((name (generate-uuid-name))
        (digester (ironclad:make-digest :sha1)))
    ;; Hash: SHA1(namespace || name)
    (ironclad:update-digest digester namespace)
    (ironclad:update-digest digester name)
    (let ((hash (ironclad:produce-digest digester)))
      ;; Formato UUID v5 (RFC 4122)
      (let ((id (subseq hash 0 16)))
        ;; Set version (5) y variant (RFC)
        (let ((time-high (dpb #b0101 (byte 4 12) ...)))
          ;; ... actualizar bits de versión
          id))))

(defun gen-vertex-id ()
  "Generar UUID para vértice"
  (gen-v5-uuid *vertex-namespace*))

(defun gen-edge-id ()
  "Generar UUID para arista"
  (gen-v5-uuid *edge-namespace*))
```

**Propiedades:**

```
- Determinístico: mismo input → mismo UUID
- Único: P(colisión) < 2^-128
- Distribuido: espaciado uniformemente
- No secuencial: imposible predecir siguiente
```

---

### 8. `index.lisp` (56 líneas)

**Propósito:** Wrapper sobre skip-lists para crear índices tipificados.

**Contenido:**

```lisp
(defstruct index
  skip-list              ; Skip-list subyacente
  key-type              ; :string, :number, :custom
  order                 ; :ascending, :descending
  unique-p              ; ¿Claves únicas?
  heap                  ; Memory allocator
  addr)                 ; Dirección en heap

(defun make-string-index (heap &key (order :ascending) unique-p)
  "Crear índice de strings"
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
      ;; Serializar metadata
      (set-byte (memory-mmap heap) pointer +db-version+)
      (set-byte (memory-mmap heap) (incf pointer) +string-index+)
      (set-byte (memory-mmap heap) (incf pointer)
               (if (eql order :ascending) 
                   +index-ascending+ +index-descending+))
      (set-byte (memory-mmap heap) (incf pointer)
               (if unique-p +index-unique+ +index-not-unique+))
      index)))

(defun make-number-index (heap &key ...)
  "Crear índice numérico")

(defun make-custom-index (heap key-type ...)
  "Crear índice custom")
```

---

### 9. `index-list.lisp` (192 líneas)

**Propósito:** Listas enlazadas persistentes usando pcons.

**Contenido:**

```lisp
(defstruct (index-list
             (:constructor %make-index-list))
  heap                  ; Memory allocator
  (cache ...)           ; Caché pcons
  head                  ; Dirección del head
  (lock (make-rw-lock)) ; RW lock
  dirty-p)              ; ¿Modificado?

(defmethod deserialize-pcons ((il index-list) address &optional buffer)
  "Leer pcons desde heap"
  (let ((pcons (or buffer (get-pcons-buffer))))
    (setf (%pcons-car pcons) (get-bytes (index-list-heap il) address 16)
          (%pcons-cdr pcons) (deserialize-uint64 
                              (index-list-heap il) (+ 16 address))
          (%pcons-deleted-p pcons) (ldb-test (byte 1 0) 
                                             (get-byte ...)))
    pcons))

(defmethod map-index-list (fn (il index-list) &key collect-p)
  "Iterar sobre index-list"
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
  "Añadir al frente de index-list"
  (let ((pcons (get-pcons-buffer)))
    (setf (%pcons-car pcons) uuid
          (%pcons-cdr pcons) (index-list-head il)
          (%pcons-deleted-p pcons) nil)
    (let ((address (serialize-pcons pcons (index-list-heap il))))
      (setf (index-list-head il) address)
      (setf (index-list-dirty-p il) t))))
```

---

### 10. `index-vector.lisp` (197 líneas)

**Propósito:** Vectores persistentes (arrays dinámicos).

**Contenido:**

```lisp
(defstruct (index-vector
             (:constructor %make-index-vector))
  (address 0)          ; Dirección en heap
  (size 0)             ; Número de elementos
  (vector #())         ; Array de UUIDs
  heap)                ; Memory allocator

(defun make-index-vector (heap key-vector)
  "Crear vector persistente"
  (let* ((total-size (+ 8 (* 16 (length key-vector))))
         (address (allocate heap total-size))
         (index-vector (%make-index-vector 
                       :address address
                       :heap heap
                       :size (length key-vector)
                       :vector key-vector)))
    ;; Serializar size
    (serialize-uint64 (memory-mmap heap) 
                     (index-vector-size index-vector) address)
    ;; Serializar elementos
    (incf address 8)
    (dotimes (i (index-vector-size index-vector))
      (dotimes (j 16)
        (set-byte (memory-mmap heap) (incf address)
                 (aref (aref key-vector i) j))))
    index-vector))

(defun get-index-vector (heap address)
  "Leer vector persistente"
  (let* ((size (deserialize-uint64 (memory-mmap heap) address))
         (index-vector (%make-index-vector 
                       :address address :heap heap :size size
                       :vector (make-array size))))
    ;; Deserializar elementos
    (incf address 8)
    (dotimes (i size)
      (let ((key (get-buffer 16)))
        (dotimes (j 16)
          (setf (aref key j) 
                (get-byte (memory-mmap heap) (incf address))))
        (setf (aref (index-vector-vector index-vector) i) key)))
    index-vector))

(defun index-vector-push-extend (index-vector key)
  "Añadir elemento, expandiendo si necesario"
  (let* ((heap (index-vector-heap index-vector))
         (new-size (1+ (index-vector-size index-vector)))
         (total-size (+ 8 (* 16 new-size)))
         (new-address (allocate heap total-size)))
    ;; Copiar elementos viejos
    (dotimes (i (index-vector-size index-vector))
      (dotimes (j 16)
        (set-byte (memory-mmap heap) (incf new-address)
                 (aref (aref old-vector i) j))))
    ;; Añadir nuevo elemento
    (dotimes (j 16)
      (set-byte (memory-mmap heap) (incf new-address)
               (aref key j)))
    ;; Liberar viejo
    (free heap (index-vector-address index-vector))
    ;; Actualizar
    (setf (index-vector-address index-vector) new-address
          (index-vector-size index-vector) new-size)))
```

---

## Skip Lists - Estructura Probabilística

### Ventajas vs B-Trees

```
SKIP LISTS:
  ✓ Más simple de implementar
  ✓ Comparable performance O(log n)
  ✓ No requiere rebalanceo
  ✓ Buena caché locality
  ✗ Uso extra de memoria (pointers)

B-TREES:
  ✓ Mejor caché locality
  ✓ Menos pointers
  ✗ Más complejo
  ✗ Requiere rebalanceo
```

### Distribución de Niveles

```
Con +max-level+ = 64 y P = 1/4:

Nivel 0: 100%    ████████████████████████████████████████
Nivel 1:  25%    ██████████
Nivel 2:   6%    ██
Nivel 3:   1%    
Nivel 4:   0%    

Total nodos esperados por nivel:
Nivel 0: n
Nivel 1: n/4
Nivel 2: n/16
Nivel 3: n/64
...
Total overhead: n * (1 + 1/4 + 1/16 + ...) ≈ 1.33n
```

---

## Linear Hashing - Tabla Hash Dinámica

### Ventajas vs Rehashing

```
REHASHING TRADICIONAL:
  Load factor > threshold
  → Rehash todo (O(n))
  → Pausa observable
  
LINEAR HASHING:
  Load factor > threshold
  → Split bucket[next]
  → Rehashe solo ~ n/initial_size items
  → Incremental, sin pausa
```

---

## Memoria y Allocación

### Bins y Free-Lists

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

---

## Serialización

### Encoding de Longitudes

```
Variable-length encoding para economía:

Entero 5 (necesita 1 byte):
  Encoded: [1, 5]  (1 byte para length, 5 es el valor)

Entero 256 (necesita 2 bytes):
  Encoded: [2, 0, 1]  (1 byte length=2, 2 bytes para valor)

Entero 65536 (necesita 3 bytes):
  Encoded: [3, 0, 0, 1]  (1 byte length=3, 3 bytes para valor)
```

---

## Orden de Carga

```
ORDEN DE CARGA - CAPA 4
═══════════════════════════════════════════════════════════════

Desde CAPA 3:
    ↓
allocator.lisp ............. Memory management
    ├─ Depende: CAPA 2 (mmap, pmem)
    └─ Define: memory, bins, free-lists
         ↓
    buffer-pool.lisp ........ Pool de objetos
         ├─ Depende: allocator
         └─ Define: buffer-pool, recycling
              ↓
    node-id.lisp ........... Generación IDs
         ├─ Depende: ironclad (SHA1)
         └─ Define: gen-vertex-id, gen-edge-id
              ↓
    serialize.lisp ......... Serialización
         ├─ Depende: buffer-pool
         └─ Define: type-tags, encode/decode
              ↓
    skip-list.lisp ......... Skip lists
         ├─ Depende: allocator, serialize
         └─ Define: skip-node, skip-list, insert/search
              ↓
    skip-list-cursors.lisp . Iteradores
         ├─ Depende: skip-list
         └─ Define: cursor, range-cursor
              ↓
    linear-hash.lisp ....... Tabla hash
         ├─ Depende: allocator, serialize
         └─ Define: lhash, buckets, splits
              ↓
    index.lisp ............. Wrapper índices
         ├─ Depende: skip-list
         └─ Define: index, string/number/custom
              ↓
    index-list.lisp ........ Listas persistentes
         ├─ Depende: allocator, pcons (Capa 2)
         └─ Define: index-list, linked-lists
              ↓
    index-vector.lisp ...... Vectores persistentes
         ├─ Depende: allocator
         └─ Define: index-vector, dynamic-arrays
```

---

## Resumen

La **Capa 4** proporciona:

1. ✓ **Skip Lists (888 líneas)** - Índices ordenados O(log n)
2. ✓ **Skip List Cursors (122 líneas)** - Iteradores
3. ✓ **Linear Hashing (764 líneas)** - Hash dinámico
4. ✓ **Allocator (334 líneas)** - Gestión memoria con bins
5. ✓ **Buffer Pool (424 líneas)** - Pool reutilizable
6. ✓ **Serialization (470 líneas)** - Conversión tipo-agnóstica
7. ✓ **Node ID (54 líneas)** - Generación de UUIDs
8. ✓ **Index (56 líneas)** - Wrapper índices
9. ✓ **Index List (192 líneas)** - Listas persistentes
10. ✓ **Index Vector (197 líneas)** - Vectores persistentes

**Total:** ~3,501 líneas de código que proporcionan:
- Estructuras de datos eficientes (O(log n), O(1))
- Persistencia en disco
- Memoria manejada dinámicamente
- Pool de objetos para evitar GC
- Serialización extensible

En las capas siguientes (5+), todo usa estas estructuras como base.

---

*Documentación de la Capa 4 de VivaceGraph*
*Marzo 2026*
