# VivaceGraph - Capa 2: Memoria & Sincronización

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Propósito y Responsabilidades](#propósito-y-responsabilidades)
3. [Arquitectura de Memoria](#arquitectura-de-memoria)
4. [Componentes de la Capa 2](#componentes-de-la-capa-2)
5. [Archivos Detallados](#archivos-detallados)
6. [Sincronización y Concurrencia](#sincronización-y-concurrencia)
7. [Interoperabilidad entre Lisps](#interoperabilidad-entre-lisps)
8. [Orden de Carga](#orden-de-carga)

---

## Visión General

La **Capa 2** es el **segundo nivel fundamental** de VivaceGraph. Gestiona:

- **Memoria persistente** en archivos mapeados (memory-mapped files)
- **Estructuras de datos persistentes** (pcons, pmem)
- **Sincronización de acceso** (read-write locks)
- **Comunicación entre procesos** (colas, mailboxes)
- **Iteradores genéricos** (cursors)

### Características Clave

- ✓ **Memoria persistente:** Los datos sobreviven a crashes
- ✓ **Sincronización:** Acceso thread-safe y reader-writer locks
- ✓ **Serialización:** Conversión a/desde bytes en memory-mapped files
- ✓ **Manejo de segfaults:** Reintento automático en caso de acceso inválido
- ✓ **Compatible con múltiples SOs:** Linux y Darwin (macOS)

### Líneas de Código

```
Archivo                    Líneas
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
TOTAL                       702 líneas
```

### Ubicación en la Arquitectura

```
                    CAPA 3
               (Persistencia)
                     ↑
                     │
         ┌──────────────────────┐
         │   CAPA 2 (AQUÍ)      │
         │ Memoria & Sincro     │
         └──────────────────────┘
                     ↑
                     │
                    CAPA 1
             (Infraestructura)
```

---

## Propósito y Responsabilidades

### ¿Por qué existe la Capa 2?

VivaceGraph necesita almacenar datos **de forma persistente** en disco, pero accederlos con la **velocidad de la RAM**. Esto requiere:

1. **Mapeo de archivos en memoria** (memory-mapped files)
2. **Gestor de memoria personalizado** (stack/heap persistente)
3. **Sincronización de acceso** (locks para concurrencia)
4. **Manejo de fallos** (segfaults, reintentos)

### Responsabilidades Específicas

| Responsabilidad | Archivo | Razón |
|-----------------|---------|-------|
| Estructuras persistentes tipo cons | `pcons.lisp` | Base para listas persistentes |
| Modelo de memoria persistente | `pmem.lisp` | Gestionar stack/heap en disco |
| Estructuras persistentes custom | `pstruct.lisp` | Definir tipos de datos persistentes |
| Memory-mapped files | `mmap.lisp` | Acceso a disco como memoria |
| Synchronización reader-writer | `rw-lock.lisp` | Múltiples lectores, un escritor |
| Colas thread-safe | `queue.lisp` | Comunicación asíncrona |
| Mailboxes | `mailbox.lisp` | IPC (Inter-Process Communication) |
| Cursores/Iteradores | `cursors.lisp` | Interfaz genérica para recorrer |

---

## Arquitectura de Memoria

### Modelo General

```
DISCO DURO (Persistente)
├─ Archivo main.dat (tabla hash persistente)
├─ Archivo meta.dat (metadatos)
├─ Archivo data.dat (datos de objetos)
└─ Archivo ...

         ↓ mmap() ↓ (memory-mapped file)

MEMORIA RAM (Caché Rápida)
├─ Region 1: STACK (crece hacia arriba)
│  ├─ Variables locales
│  ├─ Frames de funciones
│  └─ Datos "hot"
│
├─ Region 2: HEAP (crece hacia abajo)
│  ├─ Objetos persistentes
│  ├─ Estructuras dinámicas
│  └─ Datos "cold"
│
└─ Puntos de Control
   ├─ Stack Pointer (SPP): dónde está el tope del stack
   └─ Heap Pointer (HPP): dónde está el tope del heap

         ↓ Acceso Normal ↓

APLICACIÓN LISP
├─ Lee/escribe en memoria como normal
├─ Los cambios se propagan a disco automáticamente
└─ Crash recovery vía journal/log
```

### Stack vs Heap Persistente

```
STACK (Ascendente)        HEAP (Descendente)
┌──────────────┐          ┌──────────────┐
│ Tope (SPP) ◄─┤          │ Tope (HPP) ►─┤
├──────────────┤          ├──────────────┤
│ Variable 3   │          │ Objeto 3     │
├──────────────┤          ├──────────────┤
│ Variable 2   │          │ Objeto 2     │
├──────────────┤          ├──────────────┤
│ Variable 1   │          │ Objeto 1     │
├──────────────┤          ├──────────────┤
│ Base         │          │ Base         │
└──────────────┘          └──────────────┘

Stack crece →    Heap crece ←
(hacia arriba)   (hacia abajo)

¿Colisión?
Si SPP >= HPP: OUT OF MEMORY
```

---

## Componentes de la Capa 2

### Diagrama de Dependencias Internas

```
CAPA 2 - DEPENDENCIAS INTERNAS
================================

(Desde CAPA 1)
    ↓
pcons.lisp ................... Persistent cons cells
    ↓
pmem.lisp .................... Memory model
    ├─→ pcons.lisp
    └─→ (Capa 1)
         ↓
    pstruct.lisp ............. Persistent structures
         ↓
    mmap.lisp ................ Memory-mapped files
         ├─→ pmem.lisp
         ├─→ (Capa 1: utilities, globals)
         └─→ (FFI: CFFI, osicat)
              ↓
    rw-lock.lisp ............. Reader-writer locks
         ├─→ queue.lisp
         └─→ (Bordeaux-threads)
              ↓
    queue.lisp ............... Colas
         ↓
    mailbox.lisp ............. Mailboxes
         ├─→ queue.lisp
         ├─→ rw-lock.lisp
         └─→ (Trivial-timeout)
              ↓
    cursors.lisp ............. Cursor interface
         └─→ (solo CLOS)
```

---

## Archivos Detallados

### 1. `pcons.lisp` (37 líneas)

**Propósito:** Definir **persistent cons cells** - celdas Lisp persistentes que pueden almacenarse en disco.

**Contenido:**

```lisp
(defstruct (pcons
             (:constructor %make-pcons)
             (:conc-name %pcons-)
             (:print-function
              (lambda (c s d)
                (format s "#P(~A ~A (deleted-p ~A))"
                        (%pcons-car c) (%pcons-cdr c) 
                        (%pcons-deleted-p c)))))
  car           ; Head (tipo: byte array = ID)
  cdr           ; Tail (tipo: uint64 = offset en heap)
  deleted-p)    ; ¿Marcado como borrado?
```

**Estructura en Memoria:**

```
PCONS (25 bytes totales):
┌─────────────────────────────┐
│ CAR (16 bytes)              │  UUID del objeto
├─────────────────────────────┤
│ CDR (8 bytes)               │  Offset en heap
├─────────────────────────────┤
│ DELETED-P (1 byte)          │  Flag de borrado
└─────────────────────────────┘
```

**Métodos:**

```lisp
(defmethod serialize-pcons ((pcons pcons) heap)
  "Serializar pcons a heap persistente"
  (let ((address (allocate heap 25)))
    ;; Escribir car (16 bytes)
    (dotimes (i 16)
      (set-byte heap (+ i address) (aref car i)))
    ;; Escribir cdr (8 bytes)
    (serialize-uint64 heap cdr (+ 16 address))
    ;; Escribir deleted-p (1 byte)
    (let ((flags 0))
      (when deleted-p
        (setq flags (dpb 1 (byte 1 0) flags)))
      (set-byte heap (+ 24 address) flags))
    address))

(defgeneric deserialize-pcons (index-list address &optional buffer)
  "Deserializar pcons desde heap")

(defmethod mark-pcons-deleted ((pcons pcons) heap address)
  "Marcar pcons como borrado"
  (setf (%pcons-deleted-p pcons) t)
  (let ((flags (dpb 1 (byte 1 0) 0)))
    (set-byte heap (+ 24 address) flags)))
```

**Casos de Uso:**

- Listas persistentes en índices
- Cadenas de objetos vinculados
- Estructuras tipo skip list

**Nota:** Este es un patrón muy bajo nivel. La mayoría del código no usa pcons directamente.

---

### 2. `pmem.lisp` (78 líneas)

**Propósito:** Definir el **Persistent Memory Model** - gestor de memoria persistente con stack ascendente y heap descendente.

**Contenido:**

#### 2.1 Estructura PMEM

```lisp
(defstruct (pmem
             (:conc-name %pmem-)
             (:constructor %make-pmem))
  memory          ; Memory-mapped file backing
  size            ; Tamaño total (ej: 16MB)
  offset          ; Offset en el archivo
  stack-pointer   ; Puntero del stack (crece ↑)
  heap-pointer    ; Puntero del heap (crece ↓)
  lock            ; Recursive lock para sincronización
  cache)          ; Hash table caché (weak references)
```

#### 2.2 Constantes

```lisp
(defconstant +pmem-magic-byte+ #x1A)         ; Byte mágico
(defconstant +stack-pointer-offset+ 1)       ; Offset de SPP
(defconstant +heap-pointer-offset+ 5)        ; Offset de HPP
(defconstant +stack-pointer-start-offset+ 9) ; SPP inicial
```

#### 2.3 Creación de PMEM

```lisp
(defun make-pmem (memory &key (size (expt 2 24)))
  "Crear nuevo modelo de memoria persistente"
  (when (> size (expt 2 32))
    (error "Cannot create pmem greater than 2^32 bytes"))
  
  (let ((offset (allocate memory size)))
    ;; Escribir magic byte
    (set-byte memory offset +pmem-magic-byte+)
    
    ;; Crear estructura
    (let ((pmem (%make-pmem 
                 :memory memory
                 :size size
                 :offset offset
                 :stack-pointer (+ offset +stack-pointer-offset+)
                 :heap-pointer (+ offset +heap-pointer-offset+)
                 :lock (make-recursive-lock)
                 :cache (make-hash-table :weakness :value))))
      
      ;; Inicializar punteros
      (setf (stack-pointer pmem) +stack-pointer-start-offset+)
      (setf (heap-pointer pmem) (+ offset size))
      
      pmem)))
```

#### 2.4 Asignación de Memoria

```lisp
(defmethod stack-allocate ((pmem pmem) (size integer))
  "Asignar desde el STACK (hacia arriba)"
  (with-recursive-lock-held ((%pmem-lock pmem))
    (let ((address (stack-pointer pmem)))
      (if (>= address (heap-pointer pmem))
          (error "Out of memory: stack reached heap")
          (progn
            (incf (stack-pointer pmem) size)
            address)))))

(defmethod heap-allocate ((pmem pmem) (size integer))
  "Asignar desde el HEAP (hacia abajo)"
  (with-recursive-lock-held ((%pmem-lock pmem))
    (let ((address (- (heap-pointer pmem) size)))
      (if (<= address (stack-pointer pmem))
          (error "Out of memory: heap reached stack")
          (progn
            (setf (heap-pointer pmem) address)
            address)))))
```

**Flujo de Asignación:**

```
Solicitud: stack-allocate 100 bytes
    ↓
¿SPP + 100 >= HPP?
    ├─ Sí: ERROR "Out of memory"
    └─ No: SPP_new = SPP + 100, return SPP_old

Solicitud: heap-allocate 50 bytes
    ↓
¿HPP - 50 <= SPP?
    ├─ Sí: ERROR "Out of memory"
    └─ No: HPP_new = HPP - 50, return HPP_new
```

**Seguridad de Threads:**

```lisp
(with-recursive-lock-held ((%pmem-lock pmem))
  ;; Código aquí ejecuta con exclusión mutua
  (incf (stack-pointer pmem) size))
```

---

### 3. `pstruct.lisp` (50 líneas)

**Propósito:** Macro DSL para definir **persistent structures** - tipos de datos personalizados con validación e indexación.

**Contenido (Incompleto en el código):**

```lisp
(defmacro def-doc (name attribs slots)
  ;; Macro para definir documentos persistentes
  )

;; Ejemplo proporcionado:
(def-doc customer ()
  ((name :type :string :validator 'valid-name-p :indexed-p t)
   (city :type :string :validator 'valid-city-p :indexed-p nil)
   (diseases :type :list :indexed-p nil :private-p t)))
```

**Lo que genera:**

```lisp
(defun create-customer (&key name city diseases id type revision deleted-p addr bytes)
  "Crear nueva instancia de customer"
  (let ((%%id (or id (gen-id)))
        (%%type (or type 1))
        (%%revision (or revision 0))
        (%%deleted-p deleted-p)
        (%%addr addr)
        (%%bytes (or bytes :init))
        (%name name)
        (%city city)
        (%diseases diseases))
    
    ;; Closure que actúa como objeto
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
  "Buscar customer por ID"
  (let* ((bytes (deserialize-customer (lookup-pointer id)))
         (type (deserialize-doc-type bytes)))
    (create-customer 
      :id (deserialize-doc-id bytes)
      :type type
      ...)))
```

**Características:**

- `:type` - Tipo de dato (string, integer, list, etc.)
- `:validator` - Función de validación
- `:indexed-p` - ¿Crear índice automático?
- `:private-p` - ¿Campo privado?

**Nota:** Este archivo está incompleto en VivaceGraph. Es un prototipo para un DSL de estructuras persistentes que podría mejorarse.

---

### 4. `mmap.lisp` (266 líneas) - **ARCHIVO CLAVE**

**Propósito:** Manejar **memory-mapped files** - mapear archivos en memoria física accesible como RAM.

**Contenido:**

#### 4.1 Estructura de Mapped File

```lisp
(defstruct (mapped-file
             (:conc-name m-)
             (:predicate mapped-file-p))
  path      ; Ruta del archivo
  pointer   ; Puntero CFFI a la región mapeada
  fd)       ; File descriptor
```

**Tipos Personalizados:**

```lisp
(cffi:defctype size :unsigned-int)
(deftype uint32 () '(integer 0 4294967295))
(deftype uint40 () '(integer 0 1099511627775))
(deftype uint64 () '(integer 0 18446744073709551615))
(deftype word () '(unsigned-byte 64))  ; LispWorks
```

#### 4.2 Creación de Mapped Files

```lisp
(defun mmap-file (file &key (create-p t) (size (* 4096 25600)))
  "Mapear archivo en memoria"
  
  ;; Si no existe y create-p, crear archivo
  (when (and (not create-p) (not (probe-file file)))
    (error "File ~A does not exist" file))
  
  ;; Abrir archivo (crear si es necesario)
  (let* ((fd (osicat-posix:open
              file
              (if create-p
                  (logior osicat-posix:O-CREAT osicat-posix:O-RDWR)
                  osicat-posix:O-RDWR))))
    
    ;; Si es nuevo, expandir archivo al tamaño deseado
    (when create-p
      (osicat-posix:lseek fd (1- size) osicat-posix:seek-set)
      (cffi:foreign-funcall "write" :int fd :pointer ... size 1))
    
    ;; Mapear en memoria
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

**¿Qué hace mmap()?**

```
Antes (sin mmap):
  Aplicación → read() → Kernel → Disco → Búfer → Aplicación
                                (lento, 3+ context switches)

Con mmap:
  Aplicación → [Memoria Mapeada] → Kernel ↔ Disco
                     (rápido, acceso directo)
                     (el kernel maneja sincronización
                      automática)
```

#### 4.3 Manejo de Segmentation Faults

```lisp
(defmethod set-byte :around (mf offset byte)
  "Envolver set-byte con manejo de segfault"
  (handler-case
      (call-next-method)
    #+sbcl
    (sb-kernel::memory-fault-error (c)
      (log:error "SEGV in ~A, retrying..." mf)
      (set-byte mf offset byte))  ; Reintentar automáticamente
    #+ccl
    (CCL::INVALID-MEMORY-ACCESS (c)
      (log:error "SEGV in ~A, retrying..." mf)
      (set-byte mf offset byte))))
```

**¿Por qué?** Los memory-mapped files pueden tener fallas si:
- El archivo se trunca
- El disco falla temporalmente
- La región se expande

VivaceGraph **reintenta automáticamente** en estos casos.

#### 4.4 Operaciones de Lectura/Escritura

```lisp
(defmethod set-byte ((mapped-file mapped-file) offset byte)
  "Escribir 1 byte en posición"
  (declare (type word offset))
  (declare (type (integer 0 255) byte))
  (setf (cffi:mem-aref (m-pointer mapped-file) 
                       :unsigned-char offset) 
        byte))

(defmethod get-byte ((mapped-file mapped-file) offset)
  "Leer 1 byte desde posición"
  (declare (type word offset))
  (cffi:mem-aref (m-pointer mapped-file) :unsigned-char offset))

(defmethod get-bytes ((mapped-file mapped-file) offset length)
  "Leer N bytes desde posición"
  (let ((vec (make-byte-vector length)))
    (dotimes (i length)
      (setf (aref vec i) (get-byte mapped-file (+ i offset))))
    vec))

(defmethod set-bytes ((mapped-file mapped-file) vec offset length)
  "Escribir N bytes en posición"
  (dotimes (i length)
    (set-byte mapped-file (+ i offset) (aref vec i)))
  vec)
```

#### 4.5 Sincronización con Disco

```lisp
(defmethod sync-region ((mapped-file mapped-file) 
                       &key addr length
                       (sync osicat-posix:ms-sync))
  "Forzar sincronización de región con disco"
  (osicat-posix:msync 
   (or addr (m-pointer mapped-file))
   (or length (mapped-file-length mapped-file))
   sync))

(defmethod munmap-file ((mapped-file mapped-file) &key (save-p nil) ...)
  "Desmapar archivo de memoria (con opcional sync)"
  (when save-p
    (osicat-posix:msync (m-pointer mapped-file) ...))
  (osicat-posix:munmap (m-pointer mapped-file) ...)
  (osicat-posix:close (m-fd mapped-file))
  nil)
```

#### 4.6 Extensión de Mapped Files (Linux vs Darwin)

```lisp
#+linux
(defmethod extend-mapped-file ((mapped-file mapped-file) (length integer))
  "Extender mapped file usando mremap() [LINUX]"
  (let ((ptr (osicat-posix:mremap 
              (m-pointer mapped-file)
              (mapped-file-length mapped-file)
              (+ length (mapped-file-length mapped-file))
              osicat-posix:MREMAP-MAYMOVE)))
    (setf (m-pointer mapped-file) ptr)
    mapped-file))

#+darwin
(defmethod extend-mapped-file ((mapped-file mapped-file) (length integer))
  "Extender mapped file re-mapeando [DARWIN/macOS]"
  (let ((len (mapped-file-length mapped-file)))
    (munmap-file mapped-file)
    ;; Volver a mapear región más grande
    (setf (m-pointer mapped-file)
          (osicat-posix:mmap ...))
    mapped-file))
```

**¿Por qué diferente?** Linux tiene `mremap()` que es más eficiente. macOS no, así que hay que desmapar y remapar.

#### 4.7 Serialización de Enteros

```lisp
;; Serializar/deserializar enteros en little-endian

(defmethod serialize-uint64 ((mf mapped-file) int offset)
  "Escribir uint64 en 8 bytes (little-endian)"
  (dotimes (i 8)
    (set-byte mf offset (ldb (byte 8 (* i 8)) int))
    (incf offset)))

(defmethod deserialize-uint64 ((mf mapped-file) offset)
  "Leer uint64 desde 8 bytes"
  (let ((int 0))
    (dotimes (i 8)
      (setq int (dpb (get-byte mf (+ i offset)) 
                     (byte 8 (* i 8)) int)))
    int))

;; Lo mismo para uint32 (4 bytes) y uint40 (5 bytes)
```

**Nota sobre Endianness:**

```
Little-endian (Intel x86, ARM):
  uint64 = 0x0102030405060708
  Bytes en memoria: [08] [07] [06] [05] [04] [03] [02] [01]
  
Lectura correcta:
  byte[0] = 08 (bits 0-7)
  byte[1] = 07 (bits 8-15)
  byte[2] = 06 (bits 16-23)
  ...
```

---

### 5. `rw-lock.lisp` (185 líneas) - **SINCRONIZACIÓN AVANZADA**

**Propósito:** Implementar **Read-Write Locks** - sincronización que permite múltiples lectores pero solo un escritor.

**Contenido:**

#### 5.1 Estructura RW-Lock

```lisp
(defstruct (rw-lock
             (:print-function print-rw-lock)
             (:predicate rw-lock-p))
  (lock (make-recursive-lock))        ; Mutex interno
  (readers 0 :type integer)           ; Contador de lectores
  (semaphore (make-semaphore))        ; Semáforo para writers
  (writer-queue (make-empty-queue))   ; Cola de writers
  (writer nil)                        ; Writer actual
  (waitqueue (make-waitqueue)))       ; Condition variable
```

#### 5.2 Política de Acceso

```
REGLAS:
════════════════════════════════════════════════════════

1. MÚLTIPLES LECTORES:
   Lector 1 ──┐
   Lector 2 ──├─ ACCESO CONCURRENTE
   Lector 3 ──┤
              └─ (simultáneamente)

2. UN SOLO ESCRITOR:
   Si Writer está activo:
   Otros Writers esperan en cola
   Lectores nuevos ¡NO! pueden entrar

3. EXCLUSIÓN MUTUA:
   Nunca: Lector + Writer simultáneamente
   Nunca: Writer + Writer simultáneamente

4. PRIORIDAD:
   Writers > Readers (para evitar starvation)
```

#### 5.3 Operaciones de Lectura

```lisp
(defun acquire-read-lock (rw-lock &key (max-tries 1000))
  "Adquirir lock de lectura"
  (loop
    (with-recursive-lock-held ((lock-lock rw-lock))
      (if (lock-writer rw-lock)
          ;; Hay writer: esperar su fin
          (condition-wait (lock-waitqueue rw-lock) 
                         (lock-lock rw-lock))
          ;; Ni writer: incrementar contador de lectores
          (progn
            (incf (lock-readers rw-lock))
            (return-from acquire-read-lock rw-lock))))))

(defun release-read-lock (rw-lock)
  "Liberar lock de lectura"
  (with-recursive-lock-held ((lock-lock rw-lock))
    (assert (not (eql 0 (lock-readers rw-lock))))
    (decf (lock-readers rw-lock))
    ;; Si último lector: despertar writers
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

**Flujo de Lectura:**

```
Thread A: acquire-read-lock
    ↓
¿Hay writer?
    ├─ Sí: esperar (condition-wait)
    └─ No: readers++, return
    
... hacer lectura ...

Thread A: release-read-lock
    ↓
readers--
¿readers == 0 y hay writer?
    ├─ Sí: signal-semaphore (despertar writer)
    └─ No: nothing
```

#### 5.4 Operaciones de Escritura

```lisp
(defun acquire-write-lock (rw-lock &key reading-p wait-p)
  "Adquirir lock de escritura"
  (with-recursive-lock-held ((lock-lock rw-lock))
    ;; Si es owned recursivamente: listo
    (cond ((and (next-in-queue-p rw-lock (current-thread))
                (eq (lock-writer rw-lock) (current-thread)))
           (return-from acquire-write-lock rw-lock))
          ;; Esperar o fallar
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
  
  ;; Loop esperando turno
  (loop
    (if (eq (lock-writer rw-lock) (current-thread))
        (return-from acquire-write-lock rw-lock)
        (let ((internal-wait-p nil))
          (with-recursive-lock-held ((lock-lock rw-lock))
            (if (and (null (lock-writer rw-lock))
                    (next-in-queue-p rw-lock (current-thread)))
                (progn
                  (setf (lock-writer rw-lock) (current-thread))
                  ;; Si es reader: liberar lectura
                  (when reading-p
                    (decf (lock-readers rw-lock)))
                  ;; Si aún hay lectores: esperar
                  (unless (eql 0 (lock-readers rw-lock))
                    (setf internal-wait-p t)))
                (condition-wait (lock-waitqueue rw-lock) 
                               (lock-lock rw-lock))))
          ;; Esperar a que lectores terminen
          (when internal-wait-p
            (wait-on-semaphore (lock-semaphore rw-lock)))))))

(defun release-write-lock (rw-lock &key reading-p)
  "Liberar lock de escritura"
  (with-recursive-lock-held ((lock-lock rw-lock))
    (if (next-in-queue-p rw-lock (current-thread))
        (dequeue (lock-writer-queue rw-lock))
        (error "Cannot release lock I don't own!"))
    (if (next-in-queue-p rw-lock (current-thread))
        nil  ; Recursión detectada
        (progn
          (setf (lock-writer rw-lock) nil)
          ;; Si es reader que vuelve a leer:
          (when reading-p
            (incf (lock-readers rw-lock)))
          ;; Despertar otros writers y readers
          (condition-broadcast (lock-waitqueue rw-lock))))))

(defmacro with-write-lock ((rw-lock &key reading-p) &body body)
  `(unwind-protect
     (progn
       (acquire-write-lock ,rw-lock :reading-p ,reading-p)
       ,@body)
     (release-write-lock ,rw-lock :reading-p ,reading-p)))
```

**Flujo de Escritura:**

```
Thread A: acquire-write-lock
    ↓
¿Soy el writer actual?
    ├─ Sí: return (propiedad recursiva)
    └─ No: enqueue en writer-queue
    
¿Hay lectores o writer?
    ├─ Sí: wait-on-semaphore
    └─ No: setf writer = self, return
    
... hacer escritura ...

Thread A: release-write-lock
    ↓
¿Próximo en cola = yo?
    ├─ Sí: nothing (recursión)
    └─ No: dequeue, writer = nil
    
Despertar waiting threads (writers + readers)
```

**Cambio de Modo (Reader → Writer):**

```lisp
;; Convertir read lock a write lock:
(with-read-lock (lock)
  ;; Lectura...
  (with-write-lock (lock :reading-p t)
    ;; Conversión segura:
    ;; 1. Decrementa readers
    ;; 2. Espera a que otros readers terminen
    ;; 3. Adquiere write lock
    ;; Escritura...
    ))  ; Auto-libera
```

---

### 6. `queue.lisp` (43 líneas)

**Propósito:** Implementar **colas thread-safe** para comunicación asíncrona.

**Contenido:**

```lisp
(defstruct (queue
             (:print-function
              (lambda (q stream depth)
                (format stream "<QUEUE: ~a>" (queue-elements q)))))
  (key #'identity)          ; Función de ordenamiento
  (last nil)                ; Última celda (para append rápido)
  (elements nil))           ; Elementos

(defun make-empty-queue ()
  (make-queue))

(defun empty-queue-p (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  "Ver el primer elemento (sin sacar)"
  (elt (queue-elements q) 0))

(defun dequeue (q)
  "Sacar el primer elemento"
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue-front (q &rest items)
  "Añadir ítems al FRENTE"
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-elements q) (nconc items (queue-elements q))
               (queue-last q) (last (queue-elements q))))
        (t (setf (queue-elements q) (nconc items (queue-elements q))))))

(defun enqueue (q &rest items)
  "Añadir ítems al FINAL"
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))
```

**Operaciones:**

```
empty-queue-p: O(1) - checar largo
queue-front:   O(1) - acceso directo
dequeue:       O(n) - pop es O(n) en listas
enqueue:       O(1) - amortizado (usando queue-last)
enqueue-front: O(1) - amortizado
queue-length:  O(n) - recorrer la lista
```

**Uso en rw-lock.lisp:**

```lisp
(lock-writer-queue rw-lock)  ; Cola de writers esperando
(enqueue queue thread)       ; Añadir al final
(dequeue queue)              ; Sacar del frente
(next-in-queue-p lock thread) ; ¿Es el siguiente?
```

---

### 7. `mailbox.lisp` (31 líneas)

**Propósito:** **Mailboxes** - buzones de mensajes para comunicación inter-thread e inter-proceso.

**Contenido:**

```lisp
(defstruct (mailbox
             (:conc-name mb-)
             (:constructor %make-mailbox))
  (lock (make-lock))        ; Proteger acceso
  (queue (make-queue)))     ; Cola de mensajes

(defun make-mailbox ()
  "Crear nuevo mailbox"
  #+sbcl (sb-concurrency:make-mailbox)
  #+lispworks (mp:make-mailbox)
  #+ccl (%make-mailbox))

(defun send-message (mailbox message)
  "Enviar mensaje a mailbox"
  #+sbcl (sb-concurrency:send-message mailbox message)
  #+lispworks (mp:mailbox-send mailbox message)
  #+ccl (with-lock ((mb-lock mailbox))
          (enqueue (mb-queue mailbox) message)))

(defun receive-message (mailbox &key (timeout 1))
  "Recibir mensaje de mailbox (con timeout)"
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

**Modelo de Comunicación:**

```
Thread A                    Thread B
    │                           │
    ├─ send-message("hello")   │
    │─────────────────────────→│
    │                           ├─ receive-message()
    │                           ├─ "hello" recibido
    │                           │
    │                      send-message("ack")
    │←─────────────────────────┤
    ├─ receive-message()
    ├─ "ack" recibido
    │
```

**Casos de Uso:**

- **Replicación:** Master ↔ Slave
- **Consultas async:** Client ↔ Server
- **Background tasks:** Worker threads

---

### 8. `cursors.lisp` (12 líneas)

**Propósito:** Interfaz genérica para **iteradores/cursores**.

**Contenido:**

```lisp
(in-package :graph-db)

(defclass cursor ()
  ;; Clase base para iteradores
  ())

(defgeneric cursor-next (cursor &optional eoc)
  (:documentation "Obtener siguiente elemento"))

(defgeneric cursor-prev (cursor &optional eoc)
  (:documentation "Obtener elemento anterior"))

(defgeneric make-cursor (index &key cursor-class &allow-other-keys)
  (:documentation "Crear cursor en índice"))

(defgeneric make-values-cursor (index &key &allow-other-keys)
  (:documentation "Cursor solo de valores"))

(defgeneric make-keys-cursor (index &key &allow-other-keys)
  (:documentation "Cursor solo de claves"))

(defgeneric make-range-cursor (index start end &key &allow-other-keys)
  (:documentation "Cursor en rango [start, end)"))
```

**Patrón de Uso:**

```lisp
;; Iterar sobre un índice
(let ((cursor (make-cursor my-index)))
  (loop for item = (cursor-next cursor :eoc :done)
        until (eq item :done)
        do (process item)))

;; Rango específico
(let ((cursor (make-range-cursor my-index start-key end-key)))
  (loop for key = (cursor-next cursor)
        while key
        do ...))
```

---

## Sincronización y Concurrencia

### Modelo de Concurrencia

```
CAPA 2 - SEGURIDAD DE THREADS
═══════════════════════════════════════════════════

1. PMEM (Persistent Memory):
   ├─ Cada pmem tiene su propio lock (recursive)
   ├─ stack-allocate: lockea pmem
   └─ heap-allocate: lockea pmem

2. MMAP (Memory-Mapped Files):
   ├─ Operaciones atómicas byte-level
   ├─ set-byte: seguro
   ├─ sync-region: thread-safe
   └─ Manejo de segfaults: reintenta

3. RW-LOCK:
   ├─ Readers: CONCURRENTES
   ├─ Writers: EXCLUSIVOS
   └─ Transición Reader→Writer: SEGURA

4. QUEUE:
   ├─ Operaciones básicas: O(1) amortizado
   ├─ No es thread-safe por sí sola
   └─ Envolver con lock si múltiples threads

5. MAILBOX:
   ├─ send-message: THREAD-SAFE
   ├─ receive-message: THREAD-SAFE + TIMEOUT
   └─ Diferentes implementaciones por Lisp
```

### Deadlock Prevention

```
EVITAR DEADLOCKS:
════════════════════════════════════════

1. LOCK ORDERING:
   Siempre adquirir locks en el MISMO orden
   
   Seguro:
   with-lock (lock1)
     with-lock (lock2)
       ...
   
   PELIGRO (deadlock posible):
   Thread A: with-lock (lock1) then lock2
   Thread B: with-lock (lock2) then lock1
             ↑ pueden esperar mutuamente

2. TIMEOUT:
   receive-message (mailbox :timeout 5)
   ↑ no espera forever
   
   with-lock (lock :timeout 1)
   ↑ fallar si no adquiere en 1s

3. UNWIND-PROTECT:
   (unwind-protect
     (acquire-lock)
     (release-lock))
   ↑ garantiza liberación incluso si error
```

---

## Interoperabilidad entre Lisps

### Diferencias Principales

| Característica | SBCL | LispWorks | CCL |
|---|---|---|---|
| **Threads** | sb-thread | mp | ccl |
| **Mutex** | sb-thread:mutex | mp:lock | ccl:lock |
| **Semáforo** | sb-thread:semaphore | mp:semaphore | manual |
| **Condition Var** | sb-thread:waitqueue | mp:condition-variable | ccl:condition-variable |
| **Read-Write Lock** | custom (Capa 2) | custom (Capa 2) | nativa (ccl:rw-lock) |

### Estrategia de Compatibilidad

```lisp
;; Usar directivas de compilación condicional
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

**Ejemplo en mmap.lisp:**

```lisp
;; Memory-mapped files: diferente en Linux vs Darwin
#+linux
(defmethod extend-mapped-file ...)  ; usa mremap()

#+darwin
(defmethod extend-mapped-file ...)  ; re-mapea
```

---

## Orden de Carga

**CRÍTICO:** Respetar el orden exacto.

```
ORDEN DE CARGA - CAPA 2
═══════════════════════════════════════════════════

Desde CAPA 1:
    ↓
pcons.lisp ...................... Cons persistentes
    ↓
pmem.lisp ....................... Memory model
    ↓
pstruct.lisp .................... Structures (menos usado)
    ↓
mmap.lisp ....................... Memory-mapped files
    ├─ Depende: CAPA 1 (utilities, globals)
    ├─ Depende: CFFI, osicat (FFI)
    └─ Define: set-byte, get-byte, serialize-uint*
         ↓
    rw-lock.lisp ................ RW Locks
         ├─ Depende: queue.lisp
         ├─ Depende: bordeaux-threads
         └─ Usa: condition-wait, signal-semaphore
              ↓
    queue.lisp .................. Colas
         ├─ Depende: CAPA 1
         └─ Usa: enqueue, dequeue, queue-front
              ↓
    mailbox.lisp ................ Mailboxes
         ├─ Depende: queue.lisp, rw-lock.lisp
         ├─ Depende: sb-concurrency (SBCL)
         ├─ Depende: mp (LispWorks)
         ├─ Depende: trivial-timeout
         └─ Proporciona: send/receive-message
              ↓
    cursors.lisp ................ Cursor interface
         └─ Depende: solo CLOS
```

**En graph-db.asd:**

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

---

## Resumen

La **Capa 2** proporciona:

1. ✓ **Estructuras persistentes** (pcons, pmem)
2. ✓ **Memory-mapped files** (acceso rápido a disco)
3. ✓ **Sincronización avanzada** (rw-locks)
4. ✓ **Colas thread-safe** (para comunicación)
5. ✓ **Mailboxes** (para IPC)
6. ✓ **Interfaz genérica** (cursores/iteradores)

**Total:** ~702 líneas de código que maneja:
- Persistencia en disco
- Acceso concurrente thread-safe
- Recuperación ante fallos
- Compatibilidad multi-Lisp

En las capas siguientes (3+), todo uso de memoria persistente y sincronización depende de la **Capa 2**.

---

*Documentación de la Capa 2 de VivaceGraph*
*Marzo 2026*
