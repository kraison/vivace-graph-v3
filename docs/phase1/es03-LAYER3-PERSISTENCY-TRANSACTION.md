# VivaceGraph - Capa 3: Persistencia & Transacciones

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Propósito y Responsabilidades](#propósito-y-responsabilidades)
3. [Modelo ACID](#modelo-acid)
4. [Componentes de la Capa 3](#componentes-de-la-capa-3)
5. [Archivos Detallados](#archivos-detallados)
6. [Flujo de Transacciones](#flujo-de-transacciones)
7. [Replicación Master-Slave](#replicación-master-slave)
8. [Recuperación ante Fallos](#recuperación-ante-fallos)
9. [Orden de Carga](#orden-de-carga)


## Visión General

La **Capa 3** es el **tercero y más crucial nivel** de VivaceGraph. Proporciona:

- **Transacciones ACID** - Garantías de Atomicidad, Consistencia, Aislamiento, Durabilidad
- **Persistencia en disco** - Snapshots y transaction logs
- **Replicación master-slave** - Redundancia y escalabilidad de lectura
- **Recuperación ante fallos** - Replay de transacciones desde logs
- **Garbage collection** - Limpieza de memoria no referenciada

### Características Clave

- ✓ **ACID-compliant:** Garantías fuertes de transacciones
- ✓ **Optimistic Locking:** Minimiza locks, maximiza concurrencia
- ✓ **Conflict Detection:** Detecta y resuelve conflictos
- ✓ **Replication Logs:** Registro exhaustivo de cambios
- ✓ **Crash Recovery:** Recuperación automática ante fallos
- ✓ **Garbage Collection:** Liberación automática de memoria

### Líneas de Código

```
Archivo                          Líneas
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
TOTAL                            2336 líneas
```

## Propósito y Responsabilidades

### ¿Por qué existe la Capa 3?

VivaceGraph necesita **garantizar** que los datos:
1. Se escriban **atomicamente** (todo o nada)
2. Permanezcan **consistentes** (validación)
3. Sean **aislados** (sin interferencia entre transacciones)
4. Sean **durables** (resistan crashes)

Además, necesita:
- **Escalabilidad horizontal:** Replicación a slaves
- **Recuperación:** Poder restaurar desde fallos
- **Limpieza:** Liberar memoria sin references

### Responsabilidades Específicas

| Responsabilidad | Archivo | Razón |
|-----------------|---------|-------|
| Control de transacciones | `transactions.lisp` | Gestionar read/write sets, validación |
| Recuperación de snapshots | `transaction-restore.lisp` | Recrear estado desde backup |
| Replicación streaming | `transaction-streaming.lisp` | Enviar cambios a slaves |
| Log streaming | `transaction-log-streaming.lisp` | Recuperación incremental |
| Log de transacciones | `txn-log.lisp` | Gestionar snapshots y replay |
| Backup/Snapshots | `backup.lisp` | Crear copias de seguridad puntuales |
| Replicación (stub) | `replication.lisp` | Placeholder para replicación |
| Garbage collection | `gc.lisp` | Limpiar memoria no referenciada |

## Modelo ACID

### Atomicidad (Atomic)

```
Garantía: Una transacción ocurre COMPLETAMENTE o NO ocurre
═══════════════════════════════════════════════════════════════

ANTES:
  Graph:
    Vértice A: value=10
    Vértice B: value=20

TRANSACCIÓN:
  update A.value = 15
  update B.value = 25
  
DURANTE (si falla a mitad):
  ↑ Nada cambia (rollback automático)
  
DESPUÉS (si completa):
  Vértice A: value=15
  Vértice B: value=25
  ↑ Ambos o ninguno (NUNCA solo uno)
```

**Implementación:**

```lisp
(defmacro with-transaction ((transaction-manager) &body body)
  ;; Ejecuta body dentro de una transacción
  ;; Si error: rollback automático
  ;; Si éxito: commit automático
  ...)

;; Ejemplo:
(with-transaction ((transaction-manager graph))
  (update-node node1 :value 15)
  (update-node node2 :value 25)
  ;; O ambos se aplican o ninguno
)
```

### Consistencia (Consistent)

```
Garantía: Los datos siempre satisfacen restricciones
═══════════════════════════════════════════════════════════════

Restricciones Posibles:
  - Foreign key: Edge.from/to siempre apuntan a vértices existentes
  - Type: Vertex.age siempre es un integer ≥ 0
  - Custom: suma de todos los values ≤ 100
  
VivaceGraph Valida:
  ✓ Revisiones (no hay race conditions)
  ✓ Type checking
  ✓ User-defined validators (si existen)
  
Si validación falla:
  → Excepción validation-conflict
  → Transacción reintenta (hasta N veces)
```

### Aislamiento (Isolated)

```
Garantía: Transacciones concurrentes no interfieren
═══════════════════════════════════════════════════════════════

ESCENARIO PELIGROSO:

Thread A: Lee Vértice X (value=10)
            ↓
Thread B:   Modifica Vértice X (value=20)
            Commit
            ↓
Thread A:   Usa valor viejo (value=10) ← ¡INCONSISTENCIA!

VivaceGraph Usa OPTIMISTIC LOCKING:

Thread A: Lee Vértice X (value=10, revision=5)
         Modifica localmente
         Intenta commit
         Valida: ¿revision aún 5?
            ├─ Sí: ÉXITO
            └─ No: CONFLICT, reintenta

Thread B: (en paralelo)
         Modifica Vértice X
         Valida: revision cambió → CONFLICT
         Reintenta
         
GARANTÍA: Si ambos leen el mismo, solo UNO puede escribir
```

### Durabilidad (Durable)

```
Garantía: Una vez committed, es permanente
═══════════════════════════════════════════════════════════════

COMMIT SEQUENCE:

1. Escribir cambios a TRANSACTION LOG (en disco)
2. Escribir cambios a REPLICATION LOG (enviar a slaves)
3. Aplicar cambios a memory-mapped file
4. Sync región del archivo (msync)
5. Confirmar commit al cliente

Si crash ANTES de paso 1:
  → Transacción se revierte (está en RAM)
  
Si crash DESPUÉS de paso 1:
  → Transacción se replayed en recovery
  
PROPIEDAD: Una vez que commit retorna, datos son PERMANENTES
```

## Componentes de la Capa 3

### Diagrama de Dependencias

```
CAPA 3 - DEPENDENCIAS INTERNAS
================================

Desde CAPA 2:
    ↓
transactions.lisp ........... Control de txn, object-sets
    ├─ Depende: CAPA 2 (locks, memory)
    ├─ Define: transaction, transaction-manager
    └─ Usa: read-set, write-set, validation
         ↓
    transaction-restore.lisp . Restaurar desde snapshot
         ├─ Depende: transactions.lisp
         └─ Crea: restore-transaction
              ↓
    transaction-streaming.lisp Replicación de txn
         ├─ Depende: transactions.lisp
         ├─ Depende: usocket (networking)
         └─ Protocolos: master-slave
              ↓
    transaction-log-streaming.lisp Log streaming
         ├─ Depende: transaction-streaming.lisp
         └─ Lee: replication-*.log
              ↓
    txn-log.lisp ............. Gestión de logs
         ├─ Depende: backup.lisp
         └─ Implementa: snapshot, replay
              ↓
    backup.lisp .............. Crear snapshots
         ├─ Depende: transactions.lisp
         └─ Serializa: vértices/aristas a disco
              ↓
    replication.lisp ......... (stub actual)
         └─ Placeholder
              ↓
    gc.lisp .................. Garbage collection
         ├─ Depende: todas (mapea allocations)
         └─ Libera: memoria no referenciada
```

## Archivos Detallados

### 1. `transactions.lisp` (1402 líneas) - **ARCHIVO CLAVE**

**Propósito:** Núcleo del sistema de transacciones - control ACID completo.

**Contenido:**

#### 1.1 Variables Globales

```lisp
(defvar *transaction* nil)  ; Transacción actual
(defvar *end-of-transaction-action* '%commit)  ; Acción al fin (commit/rollback)

(defparameter *maximum-transaction-attempts* 8
  "Número de reintentos antes de usar exclusive lock")

(defparameter *add-to-indexes-unless-present-p* nil
  "¿Verificar unicidad al recuperar?")
```

#### 1.2 Object Sets

Un **object-set** rastrea qué objetos se leyeron/escribieron.

```lisp
(defgeneric make-object-set (initial-contents)
  "Crear conjunto de objetos")

(defgeneric object-set-count (set)
  "¿Cuántos objetos?")

(defgeneric add-to-object-set (object set)
  "Añadir objeto al conjunto")

(defgeneric object-set-member-p (object set)
  "¿Es miembro?")

(defgeneric object-sets-intersect-p (set1 set2)
  "¿Hay overlap entre dos sets?")

(defclass object-set ()
  ((table :initform (make-id-table)
          :reader table)))
```

**Uso Interno:**

```lisp
;; Cada transacción mantiene:
(read-set txn)      ; ← Qué vértices/aristas se leyeron
(write-set txn)     ; ← Qué se modificó

;; Validación:
(if (object-sets-intersect-p 
     (read-set txn)
     (write-set other-committed-txn))
    (error 'validation-conflict)
    (commit txn))
```

#### 1.3 Excepciones de Transacción

```lisp
(define-condition validation-conflict (error)
  ((transaction :initarg :transaction 
                :reader validation-conflict-transaction))
  (:report "Los read/write sets conflictúan con otra txn"))

(define-condition no-transaction-in-progress (error)
  ()
  (:report "No hay transacción activa"))

(define-condition modifying-non-copy (error)
  ((node :initarg :node :reader modifying-non-copy-node))
  (:report "Modificar sin copiar primero"))
```

**Ejemplo de Validación:**

```
Thread A: read-set = {Vértice1}
          write-set = {Vértice1}
          intenta commit
          
Thread B: (ya committed)
          write-set = {Vértice1}
          
Validación en Thread A:
  ¿{Vértice1} (lectura) ∩ {Vértice1} (escritura B) ?
  → SÍ interseca → CONFLICT
  → Reintenta transacción
```

#### 1.4 Clase Transaction

```lisp
(defclass tx ()
  ((sequence-number      ; ID local en transacción-manager
    :accessor sequence-number)
   (state                ; :active, :committed, :aborted
    :accessor state)
   (start-tx-id          ; ID de txn cuando comenzó
    :accessor start-tx-id)
   (finish-tx-id         ; ID de txn cuando terminó
    :accessor finish-tx-id)
   (transaction-id       ; ID asignado al commit
    :accessor transaction-id)
   (read-set             ; Objetos leídos
    :accessor read-set
    :initform (make-object-set nil))
   (write-set            ; Objetos modificados
    :accessor write-set
    :initform (make-object-set nil))
   (delete-set           ; Objetos borrados
    :accessor delete-set
    :initform (make-object-set nil))
   (update-queue         ; Cola de actualizaciones
    :accessor update-queue
    :initform (make-empty-queue))
   (lock                 ; RW-lock para validación
    :accessor transaction-lock
    :initform (make-rw-lock))
   (transaction-manager  ; Referencia al manager
    :accessor transaction-manager)
   (graph                ; Referencia al grafo
    :accessor graph)
   (graph-cache          ; Caché del grafo
    :accessor graph-cache)))
```

#### 1.5 Transaction Manager

```lisp
(defclass transaction-manager ()
  ((sequence-number          ; Contador de txn locales
    :accessor sequence-number :initform 0)
   (tx-id-counter            ; Contador global de txn
    :accessor tx-id-counter)
   (transactions             ; Hash de txn activas
    :reader transactions
    :initform (make-hash-table))
   (lock                     ; Lock del manager
    :reader lock)
   (replication-log-file     ; Archivo de replicación
    :accessor replication-log-file)
   (replication-log          ; Stream abierto de replicación
    :accessor replication-log)
   (graph                    ; Referencia al grafo
    :reader graph)))
```

**Métodos Principales:**

```lisp
(defmethod create-transaction (transaction-manager)
  "Crear nueva transacción"
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
  "Ejecutar función dentro de una transacción"
  (let ((completed nil)
        (attempt-count 0))
    (loop
      ;; Si demasiados intentos: usar exclusive lock
      (when (<= *maximum-transaction-attempts* attempt-count)
        (with-transaction-manager-lock (transaction-manager)
          (return (call-transaction-fun))))
      
      ;; Intentar sin lock exclusivo
      (handler-case
          (return (call-transaction-fun))
        (validation-conflict ()
          (incf attempt-count))))))

(defmethod overlapping-transactions (transaction transaction-manager)
  "Encontrar txn que podrían conflictuar"
  (let ((start (start-tx-id transaction))
        (finish (finish-tx-id transaction))
        (result '()))
    (do-committed-transactions (tx transaction-manager)
      (when (<= start (transaction-id tx) finish)
        (push tx result)))
    result))
```

**Macros de Conveniencia:**

```lisp
(defmacro with-transaction ((transaction-manager) &body body)
  "Ejecutar body dentro de transacción"
  `(call-with-transaction (lambda () ,@body)
                         ,transaction-manager))

(defmacro do-transactions ((transaction transaction-manager) &body body)
  "Iterar sobre todas las txn (activas + cometidas)")

(defmacro do-committed-transactions ((tx tm) &body body)
  "Iterar solo sobre las cometidas (tienen transaction-id)")

(defmacro do-active-transactions ((tx tm) &body body)
  "Iterar solo sobre las activas (state = :active)")
```

#### 1.6 Interceptores para Copy-on-Write

```lisp
(defmethod lookup-node :around (graph node-id)
  "Automáticamente hacer copy-on-write si se modifica"
  (let ((node (call-next-method)))
    (when *transaction*
      (add-to-object-set node (read-set *transaction*)))
    node))

(defmethod update-node :around (node graph)
  "Rastrear escritura y validar transacción"
  (when *transaction*
    (add-to-object-set node (write-set *transaction*)))
  (call-next-method))

(defmethod delete-node :around (node graph)
  "Rastrear borrado en transacción"
  (ensure-transaction ((transaction-manager graph))
    (call-next-method)))
```

**Flujo de Lectura:**

```
lookup-node("Vértice1")
    ↓
¿Hay transacción activa?
    ├─ Sí: add-to-object-set(Vértice1, read-set)
    │       ↓ (rastreamos que se leyó)
    └─ No: nothing
    
Retorna: Vértice1
```

**Flujo de Escritura:**

```
update-node(Vértice1, :value 15)
    ↓
¿Hay transacción activa?
    ├─ Sí: add-to-object-set(Vértice1, write-set)
    │       ↓ (rastreamos que se escribió)
    └─ No: nothing
    
Modifica en RAM (cambios locales a la txn)
    ↓
Retorna: ok
```

#### 1.7 Validación y Commit

```lisp
;; PSEUDOCÓDIGO del flujo de commit:

(defgeneric apply-transaction (transaction graph)
  ;; 1. Validar (¿hay conflictos?)
  (unless (validate-transaction transaction graph)
    (signal 'validation-conflict))
  
  ;; 2. Asignar transaction-id
  (assign-transaction-id transaction 
                        (transaction-manager graph))
  
  ;; 3. Escribir write-set a disco
  (dolist (node (object-set-list (write-set transaction)))
    (save-node node))
  
  ;; 4. Registrar en transaction log
  (log-transaction transaction graph)
  
  ;; 5. Actualizar índices
  (update-indexes transaction graph)
  
  ;; 6. Sincronizar disco
  (sync-region (mmap graph))
  
  ;; 7. Marcar como cometida
  (setf (state transaction) :committed))
```

### 2. `transaction-restore.lisp` (71 líneas)

**Propósito:** Restaurar estado del grafo desde **snapshots** (copias de seguridad puntuales).

**Contenido:**

```lisp
(defvar *restore-objects-per-transaction* 10
  "Objetos a restaurar por transacción (para no sobrecargar)")

(defun restore-sharp-paren-reader (stream subcharacter arg)
  "Lector personalizado para byte-arrays en snapshots"
  (let ((contents (read-delimited-list #\\) stream)))
    (coerce contents '(simple-array (unsigned-byte 8) (*)))))

(defparameter *restore-readtable*
  "Readtable con soporte para timestamps y byte-arrays"
  (let ((*readtable* (copy-readtable)))
    (local-time:enable-read-macros)
    (set-dispatch-macro-character #\\# #\\( 'restore-sharp-paren-reader)
    *readtable*))

(defun read-n-sexps (stream n)
  "Leer N s-expressions de stream"
  (let ((sexps '()))
    (dotimes (i n (nreverse sexps))
      (let ((sexp (read stream nil stream)))
        (when (eq sexp stream)
          (return (values (nreverse sexps) :eof)))
        (push sexp sexps)))))

(defmacro do-snapshot-sexps ((var file &optional (count 10)) &body body)
  "Iterar sobre s-expressions en snapshot"
  `(call-for-snapshot-sexps (lambda (,var) ,@body)
                            ,file
                            ,count))

(defun recreate-graph (graph snapshot-file &key package-name)
  "Restaurar grafo desde snapshot"
  (let ((*package* (find-package package-name))
        (*readtable* *restore-readtable*)
        (*graph* graph)
        (count 0)
        (tx-id (load-highest-transaction-id graph)))
    
    ;; Leer snapshot en lotes
    (do-snapshot-sexps (plists snapshot-file *restore-objects-per-transaction*)
      
      ;; Crear transacción de restauración
      (let ((*transaction* 
             (make-instance 'restore-transaction
                           :transaction-id (incf tx-id))))
        
        ;; Procesar cada plist en el lote
        (dolist (plist plists)
          (incf count)
          (when (zerop (mod count 1000))
            (log:info "Restored ~A nodes" count))
          
          ;; Interpretar plist según tipo
          (ecase (car plist)
            (:v (apply 'make-vertex (rest plist)))  ; Vértice
            (:e (apply 'make-edge (rest plist)))    ; Arista
            (:last-txn-id)                          ; Metadata
            (otherwise (log:error "Unknown: ~S" plist))))
        
        ;; Aplicar transacción de este lote
        (apply-transaction *transaction* graph)))
    
    ;; Guardar highest transaction ID
    (persist-highest-transaction-id (incf tx-id) graph)
    
    (values graph :count count)))
```

**Flujo de Restauración:**

```
snapshot-file.txt:
  (:v user id1 revision1 :name "Alice" :age 30)
  (:v user id2 revision1 :name "Bob" :age 25)
  (:e friend id3 revision0 :from id1 :to id2 :weight 1.0)
  (:v user id4 revision1 :name "Charlie" :age 35)
  ...
  
recreate-graph(graph, snapshot-file)
    ↓
Leer 10 plists por vez
    ↓
Crear restore-transaction
    ↓
make-vertex para cada (:v ...)
make-edge para cada (:e ...)
    ↓
apply-transaction (aplica lote)
    ↓
Siguiente lote de 10
    ↓
Fin archivo
```

**Caso de Uso:**

```lisp
;; Recuperar grafo desde crash:
(let ((graph (make-graph :name "mydb" :location "~/graphdb")))
  (recreate-graph graph "~/graphdb/txn-log/snap-1234567890.123456"
                  :package-name :my-package)
  ;; Graph ahora está en estado del snapshot
  )
```

### 3. `transaction-streaming.lisp` (484 líneas)

**Propósito:** Replicación streaming de transacciones a slaves (redundancia en tiempo real).

**Contenido:**

#### 3.1 Protocolo de Replicación

```lisp
(defvar *replication-protocol-version* 2)

(deftype broken-pipe-error () '(satisfies broken-pipe-error-p))

(deftype unfancy-plist-element ()
  '(or number string keyword boolean))

(alexandria:define-constant +replication-buffer-size+ 4096)
```

#### 3.2 Lectura/Escritura de Paquetes

```lisp
(defun simple-socket-read (socket buffer length &key (eof-error-p t))
  "Leer LENGTH bytes de socket"
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
  "Leer un paquete completo de socket"
  (let* ((buffer (make-byte-vector +replication-buffer-size+)))
    ;; Leer header (8 bytes de tamaño)
    (simple-socket-read socket buffer 8 :eof-error-p eof-error-p)
    (let* ((packet-size (deserialize-uint64 buffer 0))
           (packet (make-byte-vector packet-size)))
      ;; Leer resto del paquete
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
  "Escribir paquete a socket"
  (write-sequence packet (usocket:socket-stream socket)
                  :start start :end end)
  (force-output (usocket:socket-stream socket)))
```

#### 3.3 Paquetes Plist (Autenticación y Metadata)

```lisp
(defun serialize-plist-packet (plist)
  "Serializar plist a paquete de transporte"
  ;; Header: [8 bytes tamaño][1 byte flags][1 byte type]
  ;; Payload: plist codificado
  (let* ((payload (serialize-packet-plist plist))
         (size (+ (length payload) 10))  ; 8+1+1
         (packet (make-byte-vector size)))
    (serialize-uint64 packet size 0)    ; Size
    (setf (aref packet 8) 0)             ; Flags
    (setf (aref packet 9) (char-code #\p))  ; Type: 'p' para plist
    (replace packet payload :start1 10)
    packet))

(defun deserialize-plist-packet (packet)
  "Deserializar plist desde paquete"
  (check-packet-type packet (char-code #\p))
  (deserialize-packet-plist packet :offset 10))
```

**Protocolo de Autenticación:**

```
Master ←→ Slave connection:

Master envía:
  {:protocol-version 2
   :graph-name "mydb"
   :schema-digest "abc123..."}

Slave responde:
  {:protocol-version 2
   :key "slave-key-secret"}

Si versión/schema no coinciden:
  → Conexión rechazada
  → Slave debe sincronizarse desde snapshot primero
```

#### 3.4 Master-Slave Communication

```lisp
;; Estructura de sesión replicación:

(defclass replication-session ()
  ((socket :accessor socket)
   (remote-address :accessor remote-address)
   (highest-transaction-id :accessor highest-transaction-id)
   (graph :accessor graph)
   ...))

;; En master: cuando txn se commit
;; → Escribir a replication-log
;; → Enviar a todos los slaves
;; → Esperar ACK de slaves (si synchronous)
;; → Retornar éxito al cliente
```

### 4. `transaction-log-streaming.lisp` (112 líneas)

**Propósito:** Recuperación incremental - stream logs de transacciones para sincronización.

**Contenido:**

```lisp
;; Los archivos de log de replicación se nombran así:
;; "replication-<hex-txn-id>.log"
;; Ejemplo: "replication-0000000000000010.log"
;;          (comienza con txn #16)

(defun all-replication-logs (graph)
  "Listar todos los logs de replicación"
  (directory (merge-pathnames "replication-*.log"
                             (persistent-transaction-directory graph))))

(defun parse-replication-log-name (replication-log)
  "Extraer txn-id inicial del nombre del archivo"
  ;; "replication-00000010.log" → #x10 = 16
  (let ((name (pathname-name replication-log)))
    (when (search "replication-" name)
      (let ((start (length "replication-")))
        (parse-integer name :start start :radix 16)))))

(defun replication-log-ranges (replication-logs)
  "Calcular rangos de txn para cada log"
  ;; Ejemplo:
  ;; log1.log comienza en txn 10, log2.log en txn 20
  ;; → log1 cubre [10, 20)
  ;; → log2 cubre [20, ∞)
  (let* ((starts (mapcar 'minimum-transaction-id replication-logs))
         (ends (append (mapcar '1- (rest starts))
                       (list most-positive-fixnum))))
    (mapcar 'cons starts ends)))

(defun applicable-replication-logs (transaction-id graph)
  "Encontrar logs a partir de transaction-id"
  ;; Ejemplo: si quiero txns desde 15 y tengo:
  ;; log1 [10, 20), log2 [20, 30), log3 [30, ∞)
  ;; → Retorna [log1, log2, log3] (todos aplicables)
  (let* ((logs (all-replication-logs graph))
         (ranges (replication-log-ranges logs))
         (target (cons transaction-id most-positive-fixnum)))
    (loop for log in logs
         for range in ranges
         when (ranges-overlap-p range target)
         collect log)))

(defun stream-replication-log (socket replication-log minimum-transaction-id)
  "Stream paquetes desde log, filtrando por txn-id mínimo"
  (with-open-file (stream replication-log 
                         :element-type '(unsigned-byte 8))
    ;; Buscar primer paquete con txn-id >= mínimo
    (loop
      (let ((packet (read-stream-packet stream)))
        (unless packet
          (return))
        (let* ((tx-header (deserialize-tx-header-vector packet)))
          (when (<= minimum-transaction-id (transaction-id tx-header))
            ;; Encontrado: stream todo lo demás
            (write-packet packet socket)
            (return))))
    ;; Stream resto del archivo sin filtros
    (stream-all-packets stream socket)))
```

**Flujo de Catch-Up:**

```
Slave conecta a Master:
  Dice: "Tengo txn hasta #19, dame el resto"

Master busca applicable-replication-logs(20):
  Logs [0-100), [100-200), [200-∞)
  → Retorna todos 3

Master streams:
  1. Primeros 100 paquetes del log 1 (desde txn 20)
  2. Todos los 100 paquetes del log 2
  3. Todos los paquetes del log 3 (hasta end of file)

Slave recibe y aplica paquetes en orden
  → Ahora sincronizado con Master
```

### 5. `txn-log.lisp` (51 líneas)

**Propósito:** Gestión de snapshots y replay de transacciones.

**Contenido:**

```lisp
(defmethod snapshot ((graph graph) 
                    &key include-deleted-p
                         (check-data-integrity-p t))
  "Crear snapshot del grafo"
  (with-recursive-lock-held ((txn-lock graph))
    
    ;; Verificar integridad antes de snapshot
    (when check-data-integrity-p
      (let ((problems (check-data-integrity graph 
                                            :include-deleted-p include-deleted-p)))
        (if problems
            (return-from snapshot 
              (values :data-integrity-issues problems)))))
    
    ;; Crear nombre de snapshot
    (multiple-value-bind (sec msec) (gettimeofday)
      (let ((snap-file (format nil "~A/txn-log/snap-~D.~6,'0D"
                               (location graph) sec msec)))
        ;; Realizar backup
        (backup graph snap-file :include-deleted-p include-deleted-p)))))

(defun find-newest-snapshot (dir)
  "Encontrar snapshot más reciente en directorio"
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
  "Replayear transacciones desde logs"
  ;; 1. Encontrar snapshot más reciente
  (let ((snapshot (find-newest-snapshot txn-dir)))
    (when snapshot
      (recreate-graph graph snapshot :package-name package-name)))
  
  ;; 2. Regenerar vistas
  (log:debug "Generating graph views.")
  (map nil
       (lambda (pair)
         (destructuring-bind (class-name . view-name) pair
           (regenerate-view graph class-name view-name)))
       (all-views graph))
  
  ;; 3. Validar integridad
  (log:debug "Checking data integrity.")
  (if check-integrity-p
      (check-data-integrity graph)
      graph))
```

**Escenario de Recuperación:**

```
Servidor crashea a las 15:30
  ↓
Encontrar snapshot más reciente: snap-1609459800.000000
  (creado hace 5 minutos)
  ↓
recreate-graph(graph, snapshot)
  → Restaura estado de las 15:25
  ↓
replay(graph, txn-dir)
  → Lee transaction logs de 15:25 → 15:30
  → Re-aplica todas las txn durante esos 5 minutos
  ↓
Graph ahora en estado EXACTO de 15:30
  (¡nada perdido!)
```

### 6. `backup.lisp` (80 líneas)

**Propósito:** Crear snapshots serializando todos los vértices/aristas.

**Contenido:**

```lisp
(defgeneric backup (object location &key include-deleted-p)
  (:documentation "Serializar objeto a archivo"))

(defmethod backup ((v vertex) (stream stream) &key include-deleted-p)
  "Serializar vértice como plist a stream"
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
  "Serializar arista como plist a stream"
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
  "Serializar todo el grafo a archivo"
  (ensure-directories-exist location)
  (let ((count 0))
    (with-open-file (out location :direction :output)
      ;; Backup de todos los vértices
      (map-vertices (lambda (v)
                      (maybe-init-node-data v :graph graph)
                      (incf count)
                      (backup v out))
                    graph :include-deleted-p include-deleted-p)
      
      ;; Backup de todas las aristas
      (map-edges (lambda (e)
                   (maybe-init-node-data e :graph graph)
                   (incf count)
                   (backup e out))
                 graph :include-deleted-p include-deleted-p)
      
      (values count location))))

(defmethod check-data-integrity ((graph graph) &key include-deleted-p)
  "Verificar que todos los datos sean deserializables"
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

**Formato de Snapshot:**

```
snapshot-1609459800.000000:

(:v user id1 (:name "Alice" :age 30) :id #(...) :revision 1 :deleted-p nil)
(:v user id2 (:name "Bob") :id #(...) :revision 0 :deleted-p nil)
(:e friend id3 id1 id2 1.0 nil :id #(...) :revision 0 :deleted-p nil)
(:v user id4 (:name "Charlie" :age 35) :id #(...) :revision 2 :deleted-p nil)
(:e friend id5 id2 id1 1.0 nil :id #(...) :revision 0 :deleted-p nil)
...

Cada línea = un objeto (vértice o arista)
Format: (:v type data-plist :id ... :revision ... :deleted-p ...)
        (:e type from to weight data-plist :id ... :revision ... :deleted-p ...)
```

### 7. `replication.lisp` (3 líneas)

**Propósito:** Placeholder - la replicación principal está en `transaction-streaming.lisp`.

```lisp
(in-package :graph-db)

;; (vacío en la versión actual)
```

### 8. `gc.lisp` (133 líneas)

**Propósito:** **Garbage Collection** - liberar memoria no referenciada.

**Contenido:**

```lisp
(defun map-all-nodes (fn graph)
  "Llamar FN para cada nodo (vértice o arista)"
  (map-vertices fn graph)
  (map-edges fn graph))

(defun map-node-addresses (fn graph)
  "Llamar FN con dirección de heap de cada nodo"
  (map-all-nodes (lambda (node)
                   (let ((data-pointer (data-pointer node)))
                     (unless (zerop data-pointer)
                       (funcall fn data-pointer))))
                 graph))

(defun map-index-list-addresses (fn index-list)
  "Llamar FN con dirección de cada pcons en índice"
  (let ((address (index-list-head index-list)))
    (loop
      (when (zerop address)
        (return))
      (funcall fn address)
      (let ((pcons (deserialize-pcons index-list address)))
        (setf address (%pcons-cdr pcons))))))

(defun map-edge-index-list-addresses (fn graph)
  "Mapear dirección de índices de aristas (ve-index, vev-index)"
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
  "Mapear dirección de type-index"
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
  "Mapear TODAS las direcciones de índices"
  (map-type-index-list-addresses fn graph)
  (map-edge-index-list-addresses fn graph))

(defun heap-allocation-table (graph)
  "Hash table de TODAS las allocaciones en heap"
  (let ((table (make-hash-table)))
    (map-all-heap-allocations (lambda (address)
                                (setf (gethash address table) t))
                              graph)
    table))

(defun graph-allocation-table (graph)
  "Hash table de allocaciones REFERENCIADAS por el grafo"
  (let ((table (make-hash-table)))
    (map-all-graph-allocations (lambda (address)
                                 (setf (gethash address table) t))
                               graph)
    table))

(defun gc-heap (graph)
  "Garbage-collect: liberar no-referenciadas"
  (log:debug "gc-ing heap.")
  
  ;; 1. Crear tabla de TODAS las allocaciones
  (let ((allocation-table (heap-allocation-table graph))
        (heap (heap graph)))
    
    ;; 2. Remover las que ESTÁN referenciadas
    (map-all-graph-allocations (lambda (pointer)
                                 (remhash pointer allocation-table))
                               graph)
    
    ;; 3. Liberar las que QUEDAN (no-referenciadas)
    (maphash (lambda (pointer _)
               (declare (ignore _))
               (log:debug "Freeing ~A" pointer)
               (free heap pointer))
             allocation-table))
  
  (log:debug "gc complete"))
```

**Algoritmo de GC Mark-and-Sweep:**

```
GC Mark-and-Sweep (simplificado):
═════════════════════════════════════════════════════════════

MARKED SET = { }
FREE LIST = { todas las allocaciones }

MARCAR:
  Para cada nodo en el grafo:
    Para cada allocación referenciada por el nodo:
      MARKED.add(allocacion)

SWEEP:
  Para cada allocacion en FREE LIST:
    Si NOT in MARKED:
      free(allocacion)

GARANTÍA:
  - Nada que sea alcanzable se libera
  - Todo no-alcanzable se libera
```


## Flujo de Transacciones

### Lectura Simple

```
Thread A: lookup-node("Vértice1")
    ↓
¿Hay *transaction*?
    ├─ Sí: add-to-object-set(Vértice1, read-set)
    │       Retorna: Vértice1 (del caché si posible)
    └─ No: Retorna: Vértice1

read-set(TxnA) = {Vértice1}
write-set(TxnA) = {}
```

### Modificación

```
Thread A: 
  (with-transaction ((tm graph))
    (let ((v (lookup-node graph "Vértice1")))  ; read
      (update-node v :value 15)))               ; write
    
    ↓
    
    read-set = {Vértice1}
    write-set = {Vértice1}
    
    Intenta commit
        ↓
    Valida: ¿write-set ∩ (write-sets de txn cometidas) ?
        ├─ No intersection: OK, commit
        └─ Intersection: CONFLICT, reintenta
```

### Conflicto

```
Thread A: read Vértice1 (revision 5)
Thread B: read Vértice1 (revision 5)
Thread B: modificar Vértice1
          commit exitoso → revision 6

Thread A: intenta commit
          Valida: ¿Vértice1 aún revision 5?
          NO (ahora es 6) → CONFLICT
          
          Reintenta:
          1. Vuelve a leer Vértice1 (revision 6)
          2. Aplica modificación sobre nuevo estado
          3. Intenta commit nuevamente
          4. Esta vez OK (sin conflict)
```


## Replicación Master-Slave

### Arquitectura

```
MASTER (writable)
├─ Transacciones → Transaction Log
├─ Transaction Log → Replication Log (replication-*.log)
├─ Replication Log → SLAVE #1
├─ Replication Log → SLAVE #2
└─ Replication Log → SLAVE #3

SLAVE #1 (read-only)
├─ Recibe Replication Log
├─ Aplica cambios localmente
└─ Envía ACK al master

SLAVE #2 (read-only)
├─ Recibe Replication Log
├─ Aplica cambios localmente
└─ Envía ACK al master

SLAVE #3 (read-only)
├─ Recibe Replication Log
├─ Aplica cambios localmente
└─ Envía ACK al master
```

### Sincronización

```
Slave conecta a Master (primera vez):

Master envía:
  1. Graph schema
  2. Metadata (highest txn id, etc.)

Slave responde:
  :hello {protocol-version 2, graph-name "mydb"}

Master:
  ¿Schema OK?
    ├─ No: :error
    └─ Sí: :ok, comienza streaming

Master envía:
  1. Snapshot más reciente (replication-log o snapshot file)
  2. Transacciones posteriores al snapshot

Slave:
  1. Restaura snapshot
  2. Aplica cada transacción
  3. Envía ACK de cada txn

Master continúa:
  Nuevas transacciones se replican en tiempo real
```

## Recuperación ante Fallos

### Escenario: Crash durante transacción

```
Estado inicial:
  Vértice A: value=10
  Vértice B: value=20

Transacción inicia:
  update A.value = 15
  update B.value = 25
  commit

Crash DURANTE commit (después de escribir A, antes de B)
  → En disco: A=15, B=20
  → INCONSISTENTE

Recuperación:
  1. Leer snapshot más reciente
  2. Ver estado último (A=10, B=20)
  3. Replayear todos los logs
  4. ¿Txn que crasheó está en log?
     ├─ Sí: replayed, aplicada completamente
     └─ No: ignorada, rollback implícito

GARANTÍA: Después de recovery, estado es CONSISTENTE
```

### Escenario: Crash después de commit

```
Transacción exitosa:
  update A=15, B=25
  commit registrado en log
  enviado a slaves
  retornado al cliente

Crash DESPUÉS de commit
  → En disco: A=15, B=25, tx-log OK

Recuperación:
  1. Leer snapshot
  2. Replayear todos los logs (incluyendo commit reciente)
  3. Estado restaurado = estado pre-crash

GARANTÍA: Transacción sobrevive al crash
         (por eso es "durabilidad")
```

## Orden de Carga

```
ORDEN DE CARGA - CAPA 3
═══════════════════════════════════════════════════════════════

Desde CAPA 2:
    ↓
transactions.lisp ............ Control ACID
    ├─ Depende: CAPA 2 (locks, memory, mmap)
    ├─ Depende: CAPA 1 (conditions, utilities)
    └─ Define: transaction, transaction-manager, object-sets
         ↓
    transaction-restore.lisp .. Restaurar snapshots
         ├─ Depende: transactions.lisp
         └─ Define: recreate-graph, restore-transaction
              ↓
    transaction-streaming.lisp  Replicación
         ├─ Depende: transactions.lisp
         ├─ Depende: usocket (networking)
         └─ Define: replication protocol, master-slave
              ↓
    transaction-log-streaming.lisp Log streaming
         ├─ Depende: transaction-streaming.lisp
         └─ Define: replication-log-ranges, applicable-logs
              ↓
    backup.lisp .............. Crear snapshots
         ├─ Depende: transactions.lisp
         └─ Define: backup, check-data-integrity
              ↓
    txn-log.lisp ............. Gestión de logs
         ├─ Depende: backup.lisp
         └─ Define: snapshot, replay
              ↓
    replication.lisp ......... (stub, placeholder)
         └─ Depende: nada especial
              ↓
    gc.lisp .................. Garbage collection
         ├─ Depende: todas (mapea allocations)
         └─ Define: gc-heap, mark-and-sweep
```

## Resumen

La **Capa 3** proporciona:

1. ✓ **Transacciones ACID** (transactions.lisp)
2. ✓ **Persistencia en disco** (backup.lisp, txn-log.lisp)
3. ✓ **Recuperación ante fallos** (transaction-restore.lisp, replay)
4. ✓ **Replicación master-slave** (transaction-streaming.lisp)
5. ✓ **Streaming incremental** (transaction-log-streaming.lisp)
6. ✓ **Garbage collection** (gc.lisp)

**Total:** ~2,336 líneas que garantizan:
- **Durabilidad:** Datos sobreviven crashes
- **Consistencia:** Estado siempre válido
- **Aislamiento:** Transacciones no interfieren
- **Atomicidad:** Todo o nada
- **Escalabilidad:** Replicación horizontal

En las capas siguientes (4+), todo depende de estas garantías ACID.

---

*Documentación de la Capa 3 de VivaceGraph*
*Marzo 2026*
