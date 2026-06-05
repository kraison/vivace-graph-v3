# VivaceGraph - Capa 1: Infraestructura & Utilidades Base

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Propósito y Responsabilidades](#propósito-y-responsabilidades)
3. [Componentes de la Capa 1](#componentes-de-la-capa-1)
4. [Archivos Detallados](#archivos-detallados)
5. [Constantes Globales Clave](#constantes-globales-clave)
6. [Orden de Carga](#orden-de-carga)
7. [Patrones de Diseño](#patrones-de-diseño)

---

## Visión General

La **Capa 1** es la **RAÍZ** sobre la cual se construye todo VivaceGraph. Incluye:

- **Configuración del namespace** (paquete Lisp)
- **Variables globales** y constantes del sistema
- **Manejo de excepciones** personalizado
- **Funciones de utilidad** reutilizables en todo el código
- **Extensiones CLOS** (Meta-Object Protocol)
- **Generación de IDs únicos** (UUIDs)
- **Generador de números aleatorios** (Mersenne Twister)
- **Estadísticas del grafo** (métricas de rendimiento)
- **Clases base** (GRAPH, NODE)

### Características Clave

- ✓ Totalmente **compatible con múltiples Lisps** (SBCL, LispWorks, CCL)
- ✓ **Sin dependencias circulares** (esta capa no importa de otras capas)
- ✓ **Thread-safe** (cuando sea apropiado)
- ✓ **Extensible** mediante CLOS y metaclases

### Líneas de Código

```
Archivo                    Líneas
──────────────────────────────────
package.lisp                 188
globals.lisp                 133
conditions.lisp               83
utilities.lisp               483
clos.lisp                     88
uuid.lisp                    121
random.lisp                  254
stats.lisp                    77
graph-class.lisp              84
node-class.lisp              174
──────────────────────────────────
TOTAL                      1,685 líneas
```

---

## Propósito y Responsabilidades

### ¿Por qué existe la Capa 1?

VivaceGraph necesita un **fundamento sólido** antes de poder:
- Crear tablas hash persistentes (Capa 4)
- Gestionar transacciones (Capa 3)
- Sincronizar acceso a memoria (Capa 2)
- Indexar grafos (Capa 5)
- Modelar datos (Capa 6)

### Responsabilidades Específicas

| Responsabilidad | Archivo | Razón |
|-----------------|---------|-------|
| Definir paquete y exportaciones | `package.lisp` | Crear namespace público |
| Variables globales compartidas | `globals.lisp` | Estado del sistema |
| Excepciones personalizadas | `conditions.lisp` | Manejo de errores robusto |
| Helpers y utilitarios | `utilities.lisp` | Funciones reutilizables |
| Metaclases CLOS | `clos.lisp` | Extender sistema de clases |
| Identificadores únicos | `uuid.lisp` | IDs para nodos/aristas |
| Aleatoriedad | `random.lisp` | Si se necesita (Mersenne Twister) |
| Métricas del grafo | `stats.lisp` | Rendimiento y monitoreo |
| Estructura GRAPH | `graph-class.lisp` | Objeto raíz de la BD |
| Estructura NODE | `node-class.lisp` | Metaclase base para datos |

---

## Componentes de la Capa 1

### Mapa de Dependencias Internas

```
CAPA 1 - DEPENDENCIAS INTERNAS
================================

package.lisp
    ↓
globals.lisp
    ↓
conditions.lisp
    ↓
utilities.lisp
    ├─→ uuid.lisp
    ├─→ random.lisp
    └─→ clos.lisp
        ↓
    graph-class.lisp (define metaclase graph-class)
        ↓
    node-class.lisp (define metaclase node-class)
        ↓
    stats.lisp

NOTA: No hay dependencias circulares.
Cada archivo solo depende de anteriores.
```

### Estructura de Interfaces

```
PÚBLICAS (exportadas en package.lisp):
├─ make-graph, open-graph, close-graph, lookup-graph
├─ execute-tx, with-transaction, commit, rollback
├─ def-node-type, def-vertex, def-edge
├─ lookup-vertex, lookup-edge, lookup-node-type-by-name
├─ vertex, edge, generic-edge, generic-vertex
├─ id, string-id, type-id, revision, deleted-p
├─ def-view, map-view, map-reduced-view, invoke-graph-view
├─ def-global-prolog-functor, compile-body, unify, ?, ?-, cut
├─ make-rw-lock, with-read-lock, with-write-lock (SBCL-specific)
└─ ... (147 símbolos en total)

PRIVADAS (uso interno):
├─ *graphs*, *graph*, *schema-node-metadata*
├─ +null-key+, +max-key+, +key-bytes+, +value-bytes+
├─ +vertex-namespace+, +edge-namespace+
├─ Constantes de serialización (+positive-integer+, etc.)
├─ *trail*, *var-counter*, *functor* (Prolog)
└─ ... (muchas más)
```

---

## Archivos Detallados

### 1. `package.lisp` (188 líneas)

**Propósito:** Define el paquete `:graph-db` y exporta todos los símbolos públicos.

**Contenido:**

```lisp
(defpackage #:graph-db
  (:use #:cl
        #:bordeaux-threads
        #:local-time
        #+ccl #:closer-mop      ; metaclases en CCL
        #+lispworks #:clos      ; CLOS en LispWorks
        #+sbcl #:sb-mop         ; Meta-Object Protocol en SBCL
        #+sbcl #:sb-pcl)        ; Portable Common Loops
  ;; Importar símbolos específicos por Lisp
  ;; (shadow imports para compatibilidad)
  
  (:export #:make-graph #:open-graph #:close-graph
           #:lookup-graph #:graph-stats
           ;; ... 147 símbolos más
           ))
```

**Responsabilidades:**

| Tarea | Detalle |
|-------|---------|
| Crear namespace | Todos los símbolos de VivaceGraph viven en `:graph-db` |
| Usar dependencias | Incluye `:cl`, `:bordeaux-threads`, `:local-time` |
| Compatibilidad | Importa MOP según el Lisp (SBCL, LispWorks, CCL) |
| Shadow imports | Reemplaza algunos símbolos de `closer-mop` en CCL |
| Exportar API pública | 147 símbolos accesibles al usuario |

**Patrones Clave:**

1. **Compilación Condicional** (directivas `#+`)
   ```lisp
   #+sbcl #:sb-mop          ; Solo cargado en SBCL
   #+lispworks #:clos       ; Solo cargado en LispWorks
   ```

2. **Shadow Imports** (reemplazar símbolos)
   ```lisp
   (:shadowing-import-from "CLOSER-MOP" "STANDARD-METHOD")
   ```
   Esto es necesario porque diferentes Lisps implementan CLOS diferente.

**Cómo se usa:**

```lisp
(in-package :graph-db)  ; Todos los archivos comienzan así
(defun make-graph ...)  ; Ahora es graph-db:make-graph
```

---

### 2. `globals.lisp` (133 líneas)

**Propósito:** Define TODAS las variables globales y constantes del sistema.

**Secciones:**

#### 2.1 Variables Globales Dinámicas

```lisp
(defvar *cache-enabled* t)                    ; ¿Activar caché?
(defvar *graph* nil)                          ; Grafo actual en sesión
(defvar *schema-node-metadata* 
  (make-hash-table :test 'equal))             ; Metadatos de tipos
```

#### 2.2 Constantes de Configuración

```lisp
(defconstant +db-version+ 1)                  ; Versión del formato
(defconstant +storage-version+ #x01)          ; Versión de almacenamiento
(defconstant +main-table-file+ "main.dat")    ; Archivo principal
(defconstant +meta-file+ "meta.dat")          ; Metadatos
(defconstant +data-file+ "data.dat")          ; Datos
(defconstant +max-node-types+ 65536)          ; Máximo de tipos
```

#### 2.3 Magic Bytes (para serialización)

```lisp
(defconstant +data-magic-byte+     #x17)      ; Marca datos
(defconstant +lhash-magic-byte+    #x18)      ; Marca hash persistente
(defconstant +overflow-magic-byte+ #x19)      ; Marca overflow
(defconstant +config-magic-byte+   #x20)      ; Marca configuración
```

Estos **bytes mágicos** permiten que VivaceGraph identifique qué tipo de datos está leyendo del disco.

#### 2.4 Constantes de Clave (IDs)

```lisp
(defconstant +null-key+      ;; 16 bytes de cero
  (make-array '(16) :element-type '(unsigned-byte 8) 
              :initial-element 0))
(defconstant +max-key+       ;; 16 bytes de 255
  (make-array '(16) :element-type '(unsigned-byte 8) 
              :initial-element 255))
(defconstant +key-bytes+ 16)
(defconstant +value-bytes+ 8)
```

**Por qué 16 bytes?** UUIDs estándar tienen 128 bits = 16 bytes. Esto permite identificación única global.

#### 2.5 Namespaces de UUID

```lisp
(defvar *vertex-namespace* 
  (uuid:uuid-to-byte-array
   (uuid:make-uuid-from-string "2140DCE1-3208-4354-8696-5DF3076D1CEB")))
(defvar *edge-namespace* 
  (uuid:uuid-to-byte-array
   (uuid:make-uuid-from-string "0392C7B5-A38B-466F-92E5-5A7493C2775A")))
```

Estos **identifican el tipo de nodo**:
- UUIDs v5 de vértices usan el namespace de vértices
- UUIDs v5 de aristas usan el namespace de aristas

Esto garantiza que un UUID de vértice nunca colisione con uno de arista, **incluso si tienen el mismo contenido**.

#### 2.6 Constantes para Índices

```lisp
(defconstant +min-sentinel+ :gmin)   ; Sentinela mínimo
(defconstant +max-sentinel+ :gmax)   ; Sentinela máximo
(defconstant +reduce-master-key+ :gagg) ; Clave de agregación
```

Los **sentinelas** son valores especiales usados en skip lists para marcar inicio/fin.

#### 2.7 Tamaños de Índices Especializados

```lisp
(defconstant +index-list-bytes+ 17)
(defconstant +ve-key-bytes+ 18)      ; Vértice→Arista
(defconstant +vev-key-bytes+ 34)     ; Vértice→Vértice
```

**Explicación:**
- `+ve-key-bytes+` = 16 (vértice) + 2 (tipo de arista) = 18
- `+vev-key-bytes+` = 16 (vértice1) + 16 (vértice2) + 2 (tipo) = 34

#### 2.8 Tipos de Serialización

```lisp
(defconstant +unknown+ 0)
(defconstant +negative-integer+ 1)
(defconstant +positive-integer+ 2)
(defconstant +character+ 3)
(defconstant +symbol+ 4)
(defconstant +string+ 5)
(defconstant +list+ 6)
(defconstant +vector+ 7)
(defconstant +single-float+ 8)
(defconstant +double-float+ 9)
;; ... más tipos ...
(defconstant +vertex+ 18)
(defconstant +edge+ 19)
(defconstant +skip-list+ 20)
(defconstant +ve-index+ 21)
(defconstant +uuid+ 100)
(defconstant +timestamp+ 101)
```

Cuando VivaceGraph **serializa un objeto** a disco, escribe primero el tipo (1 byte), luego el contenido. Al deserializar, lee el tipo y sabe cómo interpretar los bytes.

#### 2.9 Variables Prolog (Motor de Consultas)

```lisp
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0)
(defvar *functor* nil)
(defvar *select-list* nil)
(defconstant +unbound+ :unbound)
(defconstant +no-bindings+ '((t . t)))
(defconstant +fail+ nil)
```

Estas variables **controlan el motor Prolog** (usado para consultas tipo Prolog).

#### 2.10 Thread Safety (por Lisp)

```lisp
#+sbcl
(defvar *prolog-global-functors* 
  (make-hash-table :synchronized t))   ; Thread-safe en SBCL

#+lispworks
(defvar *prolog-global-functors* 
  (make-hash-table :single-thread nil)) ; Multi-thread en LispWorks

#+ccl
(defvar *prolog-global-functors* 
  (make-hash-table :shared t))         ; Compartida en CCL
```

Cada Lisp tiene su propia forma de crear hash tables thread-safe. `globals.lisp` maneja esto automáticamente.

---

### 3. `conditions.lisp` (83 líneas)

**Propósito:** Define las excepciones personalizadas de VivaceGraph.

**Excepciones Definidas:**

| Excepción | Causa | Uso |
|-----------|-------|-----|
| `slave-auth-error` | Autenticación fallida en replicación | Replicación slave |
| `transaction-error` | Error durante transacción | Control ACID |
| `serialization-error` | Fallo al serializar objeto | Persistencia |
| `deserialization-error` | Fallo al deserializar | Persistencia |
| `stale-revision-error` | Intento de actualizar versión vieja | Control de concurrencia |
| `duplicate-key-error` | Clave duplicada en índice único | Índices |
| `nonexistent-key-error` | Clave no encontrada | Búsqueda |
| `node-already-deleted-error` | Nodo ya marcado como borrado | Operaciones en nodos |
| `vertex-already-deleted-error` | Vértice borrado | Hereda de node-already-deleted-error |
| `edge-already-deleted-error` | Arista borrada | Hereda de node-already-deleted-error |
| `invalid-view-error` | Vista no existe | Vistas |
| `view-lock-error` | Error al lockear vista | Sincronización de vistas |

**Ejemplo de Definición:**

```lisp
(define-condition serialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Serialization failed for ~a because of ~a."
                       instance reason)))))
```

**Cómo se usa:**

```lisp
(handler-case
    (serialize-node node)
  (serialization-error (e)
    (format t "Error: ~A~%" e)))
```

**Jerarquía de Excepciones:**

```
ERROR (standard Lisp)
├─ slave-auth-error
├─ transaction-error
├─ serialization-error
├─ deserialization-error
├─ stale-revision-error
├─ duplicate-key-error
├─ nonexistent-key-error
├─ invalid-view-error
├─ view-lock-error
└─ node-already-deleted-error
   ├─ vertex-already-deleted-error
   └─ edge-already-deleted-error
```

---

### 4. `utilities.lisp` (483 líneas)

**Propósito:** Funciones de utilidad reutilizables en todo VivaceGraph.

**Secciones:**

#### 4.1 Debug y Logging

```lisp
(defun dbg (fmt &rest args)
  "Debug output a la consola"
  (apply #'format t fmt args)
  (terpri))

(defun ignore-warning (condition)
  "Suprimir warnings"
  (muffle-warning))
```

#### 4.2 Operaciones de Bytes y Memoria

```lisp
(defun get-random-bytes (&optional (count 16))
  "Leer bytes aleatorios de /dev/urandom"
  (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
    ...))

(defun free-memory ()
  "Reportar memoria libre (Lisp-specific)"
  #+sbcl (- (sb-kernel::dynamic-space-size) 
            (sb-kernel:dynamic-usage))
  #+ccl  (ccl::%freebytes))
```

#### 4.3 Conversión de Tiempo

```lisp
(defvar *unix-epoch-difference* 
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  "Convertir tiempo de Lisp a Unix time"
  (- universal-time *unix-epoch-difference*))

(defun get-unix-time ()
  "Obtener tiempo actual en segundos Unix"
  (universal-to-unix-time (get-universal-time)))

(defun gettimeofday ()
  "Obtener tiempo con microsegundos (FFI)"
  #+sbcl (multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
           (+ sec (/ msec 1000000)))
  #+ccl  (ccl:rlet ((tv :timeval)) ...)
  #+lispworks (fli:with-dynamic-foreign-objects ...))
```

**Por qué:** VivaceGraph necesita timestamps precisos para transacciones y logs.

#### 4.4 Operaciones Funcionales

```lisp
(defun find-all (item sequence &key test test-not)
  "Encontrar todos los elementos que coincidan"
  ...)

(defun find-anywhere (item tree)
  "Buscar item en árbol anidado"
  ...)

(defun unique-find-anywhere-if (predicate tree)
  "Buscar elementos únicos que cumplan predicado"
  ...)

(defun flatten (x)
  "Aplanar lista anidada"
  ...)
```

#### 4.5 Operaciones con Símbolos

```lisp
(defun new-interned-symbol (&rest args)
  "Crear símbolo internado desde strings"
  (intern (format nil "~{~a~}" args)))

(defun length=1 (list)
  "¿Lista de exactamente 1 elemento?"
  (and (consp list) (null (cdr list))))
```

#### 4.6 UUID y Generación de IDs

```lisp
(defun gen-id ()
  "Generar UUID v4 aleatorio"
  (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))

(defun read-uuid-from-string (string)
  "Parsear UUID desde string (ej: 6ba7b810-9dad-11d1-80b4-00c04fd430c8)"
  ...)

(defun read-id-array-from-string (string)
  "Convertir string UUID a byte array"
  ...)
```

#### 4.7 Comparación Genérica (Clave para Índices)

```lisp
(defgeneric less-than (x y)
  (:documentation "Comparación genérica <")
  (:method ((x (eql +min-sentinel+)) y) nil)
  (:method ((x (eql +max-sentinel+)) y) t)
  (:method ((x symbol) (y symbol)) 
    (string< (symbol-name x) (symbol-name y)))
  (:method ((x number) (y number)) (< x y))
  ...)

(defgeneric greater-than (x y)
  (:documentation "Comparación genérica >")
  ;; Similar a less-than
  ...)
```

**Por qué necesitamos esto?** Los índices de VivaceGraph deben comparar valores de tipos diferentes:
- Números vs números
- Strings vs strings
- Símbolos vs símbolos
- **Y también "apples vs oranges"** (comparación cross-type)

El patrón es:
```
Tipo A vs Tipo B: tipo A siempre "gana" vs tipo B
Ejemplo: string > symbol > number > null > t
```

#### 4.8 Comparación de Vectores de Bytes

```lisp
(defun key-vector< (v1 v2)
  "Comparar vectores de bytes lexicográficamente"
  ...)

(defun key-vector<= (v1 v2)
  ...)

(defun key-vector> (v1 v2)
  ...)
```

#### 4.9 Locks y Sincronización

```lisp
(defmacro with-lock ((lock &key whostate timeout) &body body)
  "Macro portátil para acquiring locks"
  #+ccl
  `(do-with-lock ,lock ,whostate ,timeout (lambda () ,@body))
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sbcl
  `(sb-thread:with-recursive-lock (,lock) ,@body))

(defun make-semaphore ()
  "Crear semáforo (Lisp-specific)"
  #+sbcl (sb-thread:make-semaphore)
  #+lispworks (mp:make-semaphore)
  #+ccl (ccl:make-semaphore))

(defmacro with-read-lock ((lock) &body body)
  "Locks de lectura (solo CCL)"
  #+ccl `(ccl:with-read-lock (,lock) ,@body))

(defmacro with-write-lock ((lock) &body body)
  "Locks de escritura (solo CCL)"
  #+ccl `(ccl:with-write-lock (,lock) ,@body))
```

---

### 5. `clos.lisp` (88 líneas)

**Propósito:** Extensiones al sistema CLOS de Lisp para soportar persistencia.

**Contenido:**

```lisp
(defvar *meta-slots*
  '(id %type-id %revision %deleted-p ... %data %bytes))

(defclass graph-class (standard-class)
  ;; Metaclase personalizada para grafos
  ())

(defmethod validate-superclass ((class graph-class) (super standard-class))
  "Las clases graph-class pueden heredar de clases normales"
  t)

(defclass graph-slot-definition (standard-slot-definition)
  ;; Definición de slot personalizada
  ())

(defmethod slot-value-using-class :around 
    ((class graph-class) instance slot)
  ;; Interceptar acceso a slots
  ;; Si es meta-slot: acceso normal
  ;; Si es data-slot: buscar en data alist
  ...)

(defmethod (setf slot-value-using-class) :around 
    ((class graph-class) instance slot)
  ;; Interceptar asignación de slots
  ;; Marcar nodo como modificado
  ...)
```

**Por qué es necesario?**

VivaceGraph usa **persistencia flexible**: los nodos pueden tener diferentes esquemas, y los slots se almacenan como asociaciones (clave → valor) en lugar de como campos de estructura.

**Ejemplo:**

```lisp
(def-vertex my-vertex
  ((name :type string :persistent t)
   (age :type integer :persistent t)
   (email :type string :persistent t)))

;; Cuando haces:
(setf (slot-value v 'name) "Alice")

;; Lo que pasa internamente:
;; 1. slot-value-using-class es invocado
;; 2. Busca 'name en la alist (data v)
;; 3. Marca v como modificado
;; 4. Si hay transacción: añade v a la cola de actualización
;; 5. Si no: guarda v a disco
```

---

### 6. `uuid.lisp` (121 líneas)

**Propósito:** Operaciones con UUIDs (Universally Unique Identifiers).

**Contenido:**

#### 6.1 Predicados y Conversiones

```lisp
(defgeneric uuid? (thing)
  (:method ((thing uuid)) t)
  (:method (thing) nil))

(defgeneric uuid-eql (uuid1 uuid2)
  "Comparar si dos UUIDs son iguales"
  (:method ((uuid1 uuid) (uuid2 uuid))
    (equalp (uuid-to-byte-array uuid1) 
            (uuid-to-byte-array uuid2))))
```

#### 6.2 Conversión UUID ↔ Byte Array

```lisp
(defun uuid-to-byte-array (uuid &optional type-specifier)
  "Convertir UUID a array de 16 bytes (o 18 con type-specifier)"
  ;; Si type-specifier: devuelve [type-byte | len-byte | 16 bytes UUID]
  ;; Sin type-specifier: devuelve [16 bytes UUID]
  ...)

(defun mmap-array-to-uuid (mfp offset)
  "Convertir bytes en memory-mapped file a UUID"
  (make-instance 'uuid
    :time-low (mmap-array-to-bytes offset (+ 3 offset) mfp)
    :time-mid (mmap-array-to-bytes (+ 4 offset) (+ 5 offset) mfp)
    ...))
```

#### 6.3 Operaciones FFI con Memory-Mapped Files

```lisp
(defun set-byte (mfp offset byte)
  "Escribir byte en posición de memory-mapped file"
  (cffi:mem-aref mfp :unsigned-char offset) = byte)

(defun get-byte (mfp offset)
  "Leer byte desde memory-mapped file"
  (cffi:mem-aref mfp :unsigned-char offset))

(defmacro mmap-array-to-bytes (from to mfp)
  "Helper para convertir bytes a integer"
  `(loop for i from ,from to ,to
     with res = 0
     do (setf (ldb (byte 8 (* 8 (- ,to i))) res) 
              (get-byte ,mfp i))
     finally (return res)))
```

**Por qué FFI (Foreign Function Interface)?** Porque acceder a memory-mapped files desde Lisp requiere CFFI para lectura/escritura de bytes directos.

---

### 7. `random.lisp` (254 líneas)

**Propósito:** Generador de números aleatorios (Mersenne Twister).

**Contenido:**

```lisp
;; Implementación del algoritmo Mersenne Twister
;; Generador pseudo-aleatorio de Matsumoto & Nishimura

(defconstant *mt-n* 624)
(defconstant *mt-m* 397)

(defstruct (mt-random-state ...)
  mti    ; índice
  arr)   ; array de 624 números

(defun mt-make-random-state (&optional state)
  "Crear nuevo generador aleatorio"
  ...)

(defun mt-genrand ()
  "Generar siguiente número aleatorio (32-bit)"
  ...)

(defun mt-random (n &optional state)
  "Generar número aleatorio 0 ≤ x < n"
  (if (integerp n)
      (mod (... usar mt-genrand) n)  ; para enteros
      (* (mt-genrand) ...) ; para floats
```

**Por qué Mersenne Twister?**
- Período muy largo (2^19937 - 1)
- Distribuición uniforme en alta dimensionalidad
- Fast (solo operaciones bit-level)
- Estándar en simulaciones

**Nota:** Este archivo es bastante independiente. Se podría usar para cualquier propósito de aleatoriedad.

---

### 8. `stats.lisp` (77 líneas)

**Propósito:** Recolectar y reportar estadísticas del grafo.

**Contenido:**

```lisp
(defun graph-writes-report (graph)
  "Reportar escrituras por segundo (últimos N segundos)"
  (let ((report nil))
    (maphash (lambda (time writes)
               (push (cons time writes) report))
             (write-stats graph))
    (sort report '< :key 'car)))

(defun graph-writes-report-last-minute (graph)
  "Promedio de escrituras por segundo en el último minuto"
  ...)

(defun graph-reads-report (graph)
  "Reportar lecturas por segundo"
  ...)

(defun graph-rw-report (&key (graph *graph*))
  "Reporte combinado de lecturas + escrituras"
  ...)

(defun graph-stats (&key (graph *graph*) detail-p)
  "Reporte completo de estadísticas"
  (let ((report
          `((:free-memory . ,(free-memory))
            (:avg-writes-per-second . ,(graph-writes-report-last-minute graph))
            (:avg-reads-per-second . ,(graph-reads-report-last-minute graph))
            (:cache-size . ,(hash-table-count (cache graph)))
            (:vertex-count . ,(read-lhash-count (vertex-table graph)))
            (:edge-count . ,(read-lhash-count (edge-table graph)))
            (:buffer-pool . ,(dump-buffer-pool-stats)))))
    ...))

(defun record-graph-write ()
  "Registrar una escritura (llamado internamente)"
  (incf (gethash (get-universal-time) 
                 (write-stats *graph*) 0)))

(defun record-graph-read ()
  "Registrar una lectura (llamado internamente)"
  (incf (gethash (get-universal-time) 
                 (read-stats *graph*) 0)))
```

**Estructura de Datos:**

```
write-stats (hash table por segundo):
  tiempo-unix → número-de-escrituras
  
Ejemplo:
  1234567890 → 150
  1234567891 → 148
  1234567892 → 152

Cuando llamas graph-stats:
  ├─ free-memory: bytes disponibles
  ├─ avg-writes-per-second: promedio último minuto
  ├─ avg-reads-per-second: promedio último minuto
  ├─ cache-size: entradas en caché
  ├─ vertex-count: total de vértices
  ├─ edge-count: total de aristas
  └─ buffer-pool: estado del pool de buffers
```

---

### 9. `graph-class.lisp` (84 líneas)

**Propósito:** Define la clase raíz `GRAPH` y su metaclase.

**Contenido:**

```lisp
(defvar *graphs*
  #+sbcl (make-hash-table :test 'equal :synchronized t)
  #+lispworks (make-hash-table :test 'equal :single-thread nil)
  #+ccl (make-hash-table :test 'equal :shared t))

(defclass graph ()
  ((graph-name :accessor graph-name :initarg :graph-name)
   (graph-open-p :accessor graph-open-p :initform nil)
   (location :accessor location :initarg :location)
   (txn-log :accessor txn-log :initarg :txn-log)
   (txn-file :accessor txn-file :initarg :txn-file)
   (txn-lock :accessor txn-lock :initform (make-recursive-lock))
   (transaction-manager :accessor transaction-manager :initarg :transaction-manager)
   (replication-key :accessor replication-key :initarg :replication-key)
   (replication-port :accessor replication-port :initarg :replication-port)
   (vertex-table :accessor vertex-table :initarg :vertex-table)
   (edge-table :accessor edge-table :initarg :edge-table)
   (heap :accessor heap :initarg :heap)
   (indexes :accessor indexes :initarg :indexes)
   (schema :accessor schema :initarg :schema)
   (cache :accessor cache :initarg :cache)
   (ve-index-in :accessor ve-index-in :initarg :ve-index-in)
   (ve-index-out :accessor ve-index-out :initarg :ve-index-out)
   (vev-index :accessor vev-index :initarg :vev-index)
   (vertex-index :accessor vertex-index :initarg :vertex-index)
   (edge-index :accessor edge-index :initarg :edge-index)
   (views-lock :accessor views-lock :initform (make-recursive-lock))
   (views :accessor views :initarg :views)
   (write-stats :accessor write-stats :initform (...))
   (read-stats :accessor read-stats :initform (...))))

(defclass master-graph (graph)
  ((replication-mbox :accessor replication-mbox)
   (replication-listener :accessor replication-listener)
   (stop-replication-p :accessor stop-replication-p :initform nil)
   (slaves :accessor slaves :initform ())
   (slaves-lock :accessor slaves-lock :initform (make-recursive-lock))))

(defclass slave-graph (graph)
  ((master-host :accessor master-host)
   (slave-socket :accessor slave-socket)
   (stop-replication-p :accessor stop-replication-p :initform nil)
   (slave-thread :accessor slave-thread :initform nil)
   (master-txn-id :accessor master-txn-id)))
```

**Slots Principales:**

| Slot | Tipo | Propósito |
|------|------|----------|
| `graph-name` | string | Nombre único del grafo |
| `location` | path | Directorio donde se almacena |
| `vertex-table` | linear-hash | Tabla de vértices |
| `edge-table` | linear-hash | Tabla de aristas |
| `heap` | allocator | Memoria persistente |
| `indexes` | alist | Índices personalizados |
| `schema` | hash-table | Tipos de nodos definidos |
| `cache` | hash-table | Caché en RAM |
| `ve-index-in/out` | ve-index | Índices de aristas (entrada/salida) |
| `vev-index` | vev-index | Índices de conectividad vértice-vértice |
| `views` | hash-table | Vistas (map-reduce) |
| `write-stats` | hash-table | Estadísticas de escritura |
| `read-stats` | hash-table | Estadísticas de lectura |

**Jerarquía de Clases:**

```
GRAPH (clase base)
├─ MASTER-GRAPH (para replicación)
│  └ (puede tener múltiples slaves)
└─ SLAVE-GRAPH (para replicación)
   └ (conectado a un master)
```

**Métodos Genéricos Definidos:**

```lisp
(defgeneric graph-p (thing)
  (:method ((graph graph)) graph)
  (:method (thing) nil))

(defgeneric master-graph-p (thing) ...)
(defgeneric slave-graph-p (thing) ...)
(defgeneric init-schema (graph))
(defgeneric update-schema (graph-or-name))
(defgeneric snapshot (graph &key))
(defgeneric scan-for-unindexed-nodes (graph))
(defgeneric start-replication (graph &key package))
(defgeneric stop-replication (graph))

(defun lookup-graph (name)
  "Buscar grafo por nombre en *graphs* hash table"
  (gethash name *graphs*))
```

---

### 10. `node-class.lisp` (174 líneas)

**Propósito:** Define la metaclase `NODE-CLASS` que extiende CLOS para persistencia.

**Contenido:**

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass node-class (standard-class) nil)

(defmethod validate-superclass ((class node-class) (super standard-class))
  "Las clases node-class pueden heredar de clases estándar"
  t)

(defclass node-slot-definition (standard-slot-definition)
  ((persistent :accessor persistent-p :initarg :persistent :initform t)
   (indexed :accessor indexed-p :initarg :index :initform nil)
   (ephemeral :accessor ephemeral-p :initarg :ephemeral :initform nil)
   (meta :accessor meta-p :initarg :meta :initform nil)))
```

**Atributos de Slot:**

| Atributo | Significado | Persistido a Disco |
|----------|-------------|-------------------|
| `:persistent t` | Slot persistente | ✓ Sí |
| `:persistent nil` | Slot efímero | ✗ No |
| `:ephemeral t` | Slot solo en memoria | ✗ No |
| `:meta t` | Slot de metadatos | ✗ No (acceso especial) |
| `:index t` | Crear índice en este slot | ✓ Sí (automático) |

**Ejemplo:**

```lisp
(def-vertex person
  ((name :type string :persistent t)        ; Persistido
   (age :type integer :persistent t)        ; Persistido
   (cached-data :ephemeral t)               ; Solo RAM
   (id :meta t)                             ; Metadatos
   (email :persistent t :index t)))         ; Persistido + Indexado
```

**Métodos Clave:**

```lisp
(defmethod data-slots ((instance node-class))
  "Retorna lista de slots manejados (persistent + ephemeral)"
  (map 'list 'slot-definition-name
       (remove-if-not #'(lambda (i)
                          (or (persistent-p i) (ephemeral-p i)))
                      (class-slots instance))))

(defmethod meta-slot-names ((instance node-class))
  "Retorna lista de slots de metadatos (:meta t)"
  ...)

(defmethod persistent-slot-names ((instance node-class))
  "Retorna lista de slots persistentes"
  ...)

(defmethod ephemeral-slot-names ((instance node-class))
  "Retorna lista de slots efímeros"
  ...)

(defmethod find-all-subclasses ((class class))
  "Encuentra recursivamente todas las subclases"
  ...)

(defmethod find-ancestor-classes ((class node-class))
  "Encuentra clases padre relevantes (excluyendo STANDARD-OBJECT, T, etc.)"
  ...)

(defmethod find-graph-parent-classes ((class node-class))
  "Encuentra clases padre de grafo (excluyendo VERTEX, EDGE, PRIMITIVE-NODE)"
  ...)
```

**Clase BASE NODE:**

```lisp
(defclass node ()
  ((id :accessor id :initform +null-key+ :initarg :id :meta t)
   (type-id :accessor type-id :initform 1 :initarg :type-id :meta t)
   (revision :accessor revision :initform 0 :initarg :revision :meta t)
   (%revision-table :accessor %revision-table :initform (make-hash-table) :meta t)
   (heap-written-p :accessor heap-written-p :initform nil :meta t)
   (type-idx-written-p :accessor type-idx-written-p :initform nil :meta t)
   (ve-written-p :accessor ve-written-p :initform nil :meta t)
   (vev-written-p :accessor vev-written-p :initform nil :meta t)
   (views-written-p :accessor views-written-p :initform nil :meta t)
   (written-p :accessor written-p :initform nil :meta t)
   (data-pointer :accessor data-pointer :initform 0 :meta t)
   (deleted-p :accessor deleted-p :initform nil :meta t)
   (data :accessor data :initform nil :meta t)
   (bytes :accessor bytes :initform :init :meta t))
  (:metaclass node-class))
```

**Slots de NODE:**

| Slot | Propósito |
|------|----------|
| `id` | Identificador único (16 bytes) |
| `type-id` | Tipo de nodo (para polimorfismo) |
| `revision` | Versión del nodo |
| `%revision-table` | Tabla de revisiones para conflicto detection |
| `heap-written-p` | ¿Ya escrito en el heap? |
| `type-idx-written-p` | ¿Ya escrito en type-index? |
| `ve-written-p` | ¿Ya escrito en ve-index? |
| `vev-written-p` | ¿Ya escrito en vev-index? |
| `views-written-p` | ¿Ya escrito en vistas? |
| `written-p` | ¿Ya escrito en general? |
| `data-pointer` | Offset en el archivo de datos |
| `deleted-p` | ¿Marcado como borrado? |
| `data` | Alist de (key . value) personalizados |
| `bytes` | Buffer de bytes (para serialización) |

---

## Constantes Globales Clave

### Tamaños de Datos

```lisp
+key-bytes+          = 16    ; Tamaño de ID (UUID)
+value-bytes+        = 8     ; Tamaño de valor en hash
+bucket-size+        = 24    ; Tamaño de bucket
+data-extent-size+   = 100MB ; Tamaño de bloque de datos

+index-list-bytes+   = 17    ; Tamaño de key para índice de lista
+ve-key-bytes+       = 18    ; Tamaño de key para ve-index
+vev-key-bytes+      = 34    ; Tamaño de key para vev-index
```

### Magic Bytes (Identificadores de Tipo)

```lisp
+data-magic-byte+     = 0x17  ; Marca de datos
+lhash-magic-byte+    = 0x18  ; Marca de hash persistente
+overflow-magic-byte+ = 0x19  ; Marca de overflow
+config-magic-byte+   = 0x20  ; Marca de configuración
```

### Sentinelas para Índices

```lisp
+min-sentinel+  = :gmin      ; Valor mínimo en skip lists
+max-sentinel+  = :gmax      ; Valor máximo en skip lists
+reduce-master+ = :gagg      ; Clave agregada en vistas
```

### Tipos de Serialización

```lisp
+unknown+     = 0
+integer+     = 1-2     ; negativo/positivo
+character+   = 3
+symbol+      = 4
+string+      = 5
+list+        = 6
+vector+      = 7
+float+       = 8-9     ; simple/double
+uuid+        = 100
+timestamp+   = 101
; ... y muchos más
```

---

## Orden de Carga

**CRÍTICO:** El orden en `graph-db.asd` es exacto:

```
1. package.lisp .............. Crear paquete
2. globals.lisp .............. Variables globales y constantes
3. conditions.lisp ........... Excepciones
4. utilities.lisp ............ Funciones de utilidad
5. clos.lisp ................. Extensiones CLOS
6. uuid.lisp ................. Operaciones UUID
7. random.lisp ............... Mersenne Twister
8. stats.lisp ................ Estadísticas
9. graph-class.lisp .......... Clase GRAPH
10. node-class.lisp .......... Metaclase NODE-CLASS
```

**¿Por qué este orden?**

```
package.lisp PRIMERO
    ↓ (define el paquete :graph-db)
globals.lisp
    ↓ (constantes y vars que todo usa)
conditions.lisp
    ↓ (excepciones para signal/handle)
utilities.lisp
    ↓ (funciones básicas reutilizadas abajo)
clos.lisp
    ↓ (extiende CLOS)
uuid.lisp + random.lisp
    ↓ (utilitarios especializados)
stats.lisp
    ↓ (estadísticas)
graph-class.lisp + node-class.lisp
    ↓ (clases raíz que todo depende)
```

Si cambias el orden, obtendrás **errores de símbolo indefinido**.

---

## Patrones de Diseño

### 1. Compilación Condicional (Feature Flags)

```lisp
#+sbcl     ; Solo en SBCL
#+lispworks ; Solo en LispWorks
#+ccl      ; Solo en Clozure CL
#-sbcl     ; Excepto en SBCL
```

**Ejemplo:**

```lisp
(defun free-memory ()
  #+sbcl
  (- (sb-kernel::dynamic-space-size) (sb-kernel:dynamic-usage))
  #+ccl
  (ccl::%freebytes)
  #+lispworks
  (mp:get-heap-size))
```

### 2. Variables Thread-Safe

```lisp
#+sbcl
(defvar *hash* (make-hash-table :synchronized t))

#+lispworks
(defvar *hash* (make-hash-table :single-thread nil))

#+ccl
(defvar *hash* (make-hash-table :shared t))
```

### 3. Metaclases Personalizadas

```lisp
(defclass my-class ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2))
  (:metaclass node-class))  ; Usar metaclase personalizada
```

Las metaclases permiten **interceptar** creación de instancias, acceso a slots, etc.

### 4. Métodos Genéricos Multimétodo

```lisp
(defgeneric less-than (x y)
  (:method ((x number) (y number)) (< x y))
  (:method ((x string) (y string)) (string< x y))
  (:method ((x symbol) (y symbol)) 
    (string< (symbol-name x) (symbol-name y)))
  (:method ((x (eql +min-sentinel+)) y) nil)
  (:method ((x (eql +max-sentinel+)) y) t))
```

El método correcto se elige **basado en los tipos en runtime**.

---

## Resumen

La **Capa 1** proporciona:

1. ✓ **Paquete y namespace** (`package.lisp`)
2. ✓ **Estado global** (`globals.lisp`)
3. ✓ **Manejo de errores** (`conditions.lisp`)
4. ✓ **Utilitarios** (`utilities.lisp`)
5. ✓ **Extensiones CLOS** (`clos.lisp`)
6. ✓ **UUIDs e IDs** (`uuid.lisp`)
7. ✓ **Aleatoriedad** (`random.lisp`)
8. ✓ **Estadísticas** (`stats.lisp`)
9. ✓ **Clase GRAPH raíz** (`graph-class.lisp`)
10. ✓ **Metaclase NODE-CLASS** (`node-class.lisp`)

**Total:** ~1,685 líneas de código fundamental que **NO depende de nada interno**.

En las siguientes capas, todo dependerá de estos fundamentos.

---

*Documentación de la Capa 1 de VivaceGraph*
*Marzo 2026*
