# VivaceGraph


[español](esReadme.md) | [english](readme.md)

<br><br>


## Documentación

**VivaceGraph** es una base de datos de grafos ACID escrita **100% en Common Lisp puro**, sin dependencias de sistemas externos (excepto librerías Lisp estándar). Implementa:

- **ACID Completo** - Transacciones con isolation, durabilidad
- **Persistencia** - Memory-mapped files con recuperación ante fallos
- **Índices** - Skip-lists, linear hashing, type-based indexing
- **Logic Programming** - Motor Prolog completo basado en PAIP
- **Tipos Dinámicos** - Sistema de tipos extensible en runtime
- **API REST** - Servidor HTTP con autenticación
- **Replicación** - Master-slave (prototype)
- **Vistas** - Vistas materializadas con map-reduce

**Tamaño:** ~12,200 líneas de código Core  
**Capas:** 7 niveles de arquitectura modular  
**Documentación:** 8,600+ líneas  

## Tabla de Contenidos Integrada

### Arquitectura General

1. **[Arquitectura](./esArchitecture.md)**
   - Visión general del sistema de 7 capas
   - Orden de carga y dependencias
   - Flujo de inicialización
   - Patrones arquitectónicos

### Capa 1: Infraestructura (1,685 líneas)

**Archivos Documentados:**
- `package.lisp` - Paquete y exportaciones
- `globals.lisp` - Constantes globales
- `conditions.lisp` - Sistema de excepciones
- `utilities.lisp` - Utilidades (UUID, tiempo, locks)
- `clos.lisp` - Metaclase node-class
- `uuid.lisp` - Operaciones UUID
- `random.lisp` - Mersenne Twister PRNG
- `stats.lisp` - Estadísticas de lectura/escritura
- `graph-class.lisp` - Clase Graph y subgrafos
- `node-class.lisp` - Metaclase para nodos

**Documentación:** 
[INFRAESTRUCTURA](./es01-LAYER1-INFRA.md)

**Responsabilidades:**
- Definir paquete y exportaciones
- Constantes de configuración
- Excepciones y manejo de errores
- Utilidades genéricas (UUID, tiempo, locks)
- Metaclases CLOS para nodes
- Clase Graph y variantes (master/slave)

### Capa 2: Memoria & Sincronización (702 líneas)

**Archivos Documentados:**
- `pcons.lisp` - Persistent cons cells
- `pmem.lisp` - Persistent memory model
- `pstruct.lisp` - DSL para estructuras persistentes
- `mmap.lisp` - Memory-mapped files
- `rw-lock.lisp` - Reader-writer locks
- `queue.lisp` - Colas FIFO
- `mailbox.lisp` - Buzones IPC
- `cursors.lisp` - Interfaz de iteradores

**Documentación:** 
[SINCRONIZACION](./es02-LAYER2-MEMORY-SYNCHRONIZATION.md)

**Responsabilidades:**
- Cons cells persistentes (pcons)
- Memory model con stack/heap
- Memory-mapped file I/O
- Reader-writer locks for concurrency
- FIFO queues
- Message passing (mailbox)
- Iterator interface


### Capa 3: Persistencia & Transacciones (2,336 líneas)

**Archivos Documentados:**
- `transactions.lisp` - Core ACID implementation
- `transaction-restore.lisp` - Snapshot restoration
- `transaction-streaming.lisp` - Master-slave replication
- `transaction-log-streaming.lisp` - Log management
- `txn-log.lisp` - Transaction logging
- `backup.lisp` - Snapshot creation
- `replication.lisp` - Replication (stub)
- `gc.lisp` - Mark-and-sweep garbage collection

**Documentación:** 
[PERSISTENCIA Y TRANSACCIONES](./es03-LAYER3-PERSISTENCY-TRANSACTION.md)

**Responsabilidades:**
- ACID transactions con optimistic locking
- Transaction lifecycle management
- Snapshot creation & restoration
- Replication protocol (master-slave)
- Log-based durability
- Garbage collection

### Capa 4: Estructuras de Datos (3,501 líneas)

**Archivos Documentados:**
- `skip-list.lisp` - Índices ordenados O(log n)
- `skip-list-cursors.lisp` - Iteradores skip-list
- `linear-hash.lisp` - Tabla hash dinámica
- `allocator.lisp` - Gestor memoria con bins
- `buffer-pool.lisp` - Pool de objetos reutilizable
- `serialize.lisp` - Serialización tipo-agnóstica
- `node-id.lisp` - Generación UUIDs v5
- `index.lisp` - Wrapper sobre skip-lists
- `index-list.lisp` - Listas persistentes
- `index-vector.lisp` - Vectores dinámicos

**Documentación:** 
[ESTRUCTURAS DE DATOS](./es04-LAYER4-DATA-STRUCTURES.md)

**Responsabilidades:**
- Skip lists para búsqueda O(log n)
- Linear hashing para crecimiento dinámico
- Memory allocator con fragmentación
- Buffer pool para evitar GC
- Serialización tipo-extensible
- UUID generation v5

### Capa 5: Indexación (1,996 líneas)

**Archivos Documentados:**
- `ve-index.lisp` - Índice vértice→aristas
- `vev-index.lisp` - Índice V1→arista→V2
- `type-index.lisp` - Índice por tipo
- `functor.lisp` - Predicados Prolog
- `views.lisp` - Vistas materializadas
- `prologc.lisp` - Motor Prolog completo

**Documentación:** 
[INDEXACIÓN](./es05-LAYER5-INDEXING.md)

**Responsabilidades:**
- Índices especializados para búsqueda rápida
- VE-Index: O(log n) aristas de vértice
- VEV-Index: O(log n) arista específica
- Type-Index: O(1) nodos por tipo
- Motor Prolog con unificación y backtracking
- Vistas materializadas map-reduce


### Capa 6: Modelo de Datos (1,351 líneas)

**Archivos Documentados:**
- `primitive-node.lisp` - Clase base Node
- `vertex.lisp` - Nodos sin aristas
- `edge.lisp` - Aristas dirigidas
- `schema.lisp` - Sistema dinámico de tipos

**Documentación:** 
[MODELO DE DATOS](./es06-LAYER6-DATA-MODEL.md)

**Responsabilidades:**
- Node primitivo con flags y serialización
- Vértices como nodos simples
- Aristas con from/to/weight
- Sistema de tipos extensible (def-vertex/def-edge)
- CRUD automático
- Lazy loading de datos

### Capa 7: API de Usuario (517 líneas)

**Archivos Documentados:**
- `interface.lisp` - Genéricos CRUD
- `traverse.lisp` - Traversal BFS
- `rest.lisp` - Servidor HTTP REST

**Documentación:** 
[API DE USUARIO](./es07-LAYER-USER-API.md)

**Responsabilidades:**
- Genéricos copy, save, mark-deleted
- Recorrido BFS del grafo
- Servidor REST HTTP con JSON
- Autenticación con htpasswd
- Introspección de schema

## Diagrama de Arquitectura

```
VIVACEGRAPH - 7 CAPAS
════════════════════════════════════════════════════════════

┌─────────────────────────────────────────────────────────┐
│ Capa 7: API DE USUARIO (517 líneas)                     │
│ └─ REST HTTP, Traversal BFS, Interface genéricos        │
├─────────────────────────────────────────────────────────┤
│ Capa 6: MODELO DE DATOS (1,351 líneas)                  │
│ └─ Node, Vertex, Edge, Schema dinámico                  │
├─────────────────────────────────────────────────────────┤
│ Capa 5: INDEXACIÓN (1,996 líneas)                       │
│ └─ VE/VEV/Type indexes, Prolog, Vistas                  │
├─────────────────────────────────────────────────────────┤
│ Capa 4: ESTRUCTURAS DE DATOS (3,501 líneas)             │
│ └─ Skip-lists, Linear hash, Allocator, Serialización    │
├─────────────────────────────────────────────────────────┤
│ Capa 3: PERSISTENCIA & TRANSACCIONES (2,336 líneas)     │
│ └─ ACID, Backup, Replicación, GC                        │
├─────────────────────────────────────────────────────────┤
│ Capa 2: MEMORIA & SINCRONIZACIÓN (702 líneas)           │
│ └─ Mmap, RW-locks, Pcons, Mailbox                       │
├─────────────────────────────────────────────────────────┤
│ Capa 1: INFRAESTRUCTURA (1,685 líneas)                  │
│ └─ Package, constantes, UUID, Prolog vars, stats        │
├─────────────────────────────────────────────────────────┤
│ TOTAL: 12,088 líneas de código CORE                     │
│        8,652 líneas de documentación                    │
│        100% Common Lisp puro                            │
└─────────────────────────────────────────────────────────┘
```

## Búsqueda Rápida por Tema

### Transacciones ACID
- **Archivo:** `transactions.lisp` (Capa 3)
- **Documentación:** [PERSISTENCIA & TRANSACCIONES](./es03-LAYER3-PERSISTENCY-TRANSACTION.md)
- **Conceptos clave:**
  - Optimistic locking con read-set/write-set
  - Serialization isolation
  - Durabilidad con logs
  - Recuperación ante fallos
  - Reintento automático (max 8 intentos)

### Índices y Búsqueda
- **Archivos:** `skip-list.lisp`, `ve-index.lisp`, `vev-index.lisp`, `type-index.lisp` (Capas 4-5)
- **Documentación:** [ESTRUCTURAS DE DATOS](./es04-LAYER4-DATA-STRUCTURES.md), [INDEXACIÓN](./es05-LAYER5-INDEXING.md)
- **Complejidades:**
  - Skip-list búsqueda: O(log n)
  - VE-index: O(log n)
  - VEV-index: O(log n)
  - Type-index: O(1)

### Logic Programming (Prolog)
- **Archivo:** `prologc.lisp`, `functor.lisp` (Capa 5)
- **Documentación:** [INDEXACIÓN](./es05-LAYER5-INDEXING.md)
- **Características:**
  - Unificación con variables
  - Backtracking con trail
  - Clausification y compilación
  - Predicados en grafo

### Sistema de Tipos
- **Archivos:** `schema.lisp`, `node-class.lisp` (Capa 6, Capa 1)
- **Documentación:** [PERSISTENCIA & TRANSACCIONES](./es03-LAYER3-PERSISTENCY-TRANSACTION.md)
- **Macros:** `def-vertex`, `def-edge`
- **Extensibilidad:** Runtime, sin recompilación

### Replicación Master-Slave
- **Archivos:** `transaction-streaming.lisp`, `replication.lisp` (Capa 3)
- **Documentación:** [PERSISTENCIA & TRANSACCIONES](./es03-LAYER3-PERSISTENCY-TRANSACTION.md)
- **Protocolo:** Paquetes binarios, autenticación, compresión

### REST API
- **Archivo:** `rest.lisp` (Capa 7)
- **Documentación:** [API DE USUARIO](./es07-LAYER-USER-API.md)
- **Endpoints:** CRUD completo, JSON encoding, autenticación


## Estadísticas del Proyecto

### Líneas de Código

```
Capa 1 (Infraestructura):              1,685 líneas
Capa 2 (Memoria & Sync):                 702 líneas
Capa 3 (Persistencia & TX):            2,336 líneas
Capa 4 (Estructuras de Datos):         3,501 líneas
Capa 5 (Indexación):                   1,996 líneas
Capa 6 (Modelo de Datos):              1,351 líneas
Capa 7 (API de Usuario):                 517 líneas
────────────────────────────────────────────────────
TOTAL:                                 12,088 líneas
```

### Documentación

```
Arquitectura general:                    442 líneas
Capa 1:                                1,156 líneas
Capa 2:                                  849 líneas
Capa 3:                                1,547 líneas
Capa 4:                                1,823 líneas
Capa 5:                                1,365 líneas
Capa 6:                                1,471 líneas
Capa 7:                                  948 líneas
────────────────────────────────────────────────────
TOTAL:                                 8,652 líneas
```

### Complejidades de Algoritmos

```
Operación               Peor Caso    Promedio     Espacio
────────────────────────────────────────────────────────
Búsqueda Vertex         O(n)         O(1)         O(1)
Búsqueda Arista (VEV)   O(n)         O(1)         O(1)
Skip-list búsqueda      O(n)         O(log n)     O(log n)
Skip-list insert        O(n)         O(log n)     O(log n)
Type lookup             O(1)         O(1)         O(1)
Traversal BFS           O(V+E)       O(V+E)       O(V)
Transacción commit      O(n)         O(log n)     O(n)
```

## 🚀 Cómo Comenzar

### Leer en este Orden

1. **Primero:** Lee [Arquitectura](./esArchitecture.md) para visión general
2. **Luego:** Capa 1-3 para entender infraestructura base
3. **Después:** Capa 4 para estructuras de datos eficientes
4. **Entonces:** Capa 5-6 para modelo de grafo
5. **Finalmente:** Capa 7 para API de usuario

### Por Interés Específico

**Si quieres entender ACID:**
- Lee [PERSISTENCIA & TRANSACCIONES](./es03-LAYER3-PERSISTENCY-TRANSACTION.md)
- Luego [SINCRONIZACION](./0es02-LAYER2-MEMORY-SYNCHRONIZATION.md) para locks

**Si quieres entender Prolog:**
- Lee [Capa 5](./05-CAPA5-INDEXACION.md)
- Específicamente sección "Motor Prolog"

**Si quieres usar el sistema:**
- Lee [Capa 7](./07-CAPA7-API-USUARIO.md)
- Luego [Capa 6](./06-CAPA6-MODELO-DATOS.md) para tipos

**Si quieres entender rendimiento:**
- Lee [Capa 4](./04-CAPA4-ESTRUCTURAS-DATOS.md) para datos
- Luego [Capa 5](./05-CAPA5-INDEXACION.md) para índices


## 🔗 Referencias Cruzadas

### Módulos por Funcionalidad

**Concurrencia:**
- `rw-lock.lisp` (Capa 2) - Locks
- `queue.lisp` (Capa 2) - Colas
- `mailbox.lisp` (Capa 2) - IPC

**Persistencia:**
- `mmap.lisp` (Capa 2) - Memory-mapped files
- `allocator.lisp` (Capa 4) - Gestor memoria
- `transactions.lisp` (Capa 3) - Transacciones

**Índices:**
- `skip-list.lisp` (Capa 4) - Skip lists
- `linear-hash.lisp` (Capa 4) - Hash dinámica
- `ve-index.lisp` (Capa 5) - Índice vértice-arista
- `vev-index.lisp` (Capa 5) - Índice vértice-arista-vértice
- `type-index.lisp` (Capa 5) - Índice por tipo

**Grafo:**
- `vertex.lisp` (Capa 6) - Vértices
- `edge.lisp` (Capa 6) - Aristas
- `schema.lisp` (Capa 6) - Tipos dinámicos

**API:**
- `traverse.lisp` (Capa 7) - Recorrido BFS
- `rest.lisp` (Capa 7) - Servidor HTTP
- `interface.lisp` (Capa 7) - Genéricos CRUD

## 💡 Conceptos Clave

### Skip Lists
Estructura probabilística que proporciona búsqueda O(log n) sin rebalanceo.
Usada para: Índices ordenados, resultados de queries.
**Leer:** [Capa 4 - Skip Lists](./04-CAPA4-ESTRUCTURAS-DATOS.md)

### Linear Hashing
Tabla hash que crece incrementalmente sin rehash completo.
Usada para: Tablas de vértices/aristas.
**Leer:** [Capa 4 - Linear Hashing](./04-CAPA4-ESTRUCTURAS-DATOS.md)

### MVCC (Multi-Version Concurrency Control)
Cada nodo tiene revisión, múltiples lectores ven versión consistente.
Usada para: Aislamiento de transacciones.
**Leer:** [Capa 3 - ACID](./03-CAPA3-PERSISTENCIA-TRANSACCIONES.md)

### Prolog Logic Programming
Lenguaje declarativo con unificación y backtracking.
Usada para: Queries complejas sobre grafos.
**Leer:** [INDEXACIÓN](./es05-LAYER5-INDEXING.md)

### Vistas Materializadas
Cachés de queries que se actualizan automáticamente.
Usada para: Optimizar queries frecuentes.
**Leer:** [INDEXACIÓN](./es05-LAYER5-INDEXING.md)

## 📝 Notas de Implementación

### Dependencias Externas

VivaceGraph depende SOLO de librerías Lisp estándar:

- **Bordeaux-threads** - Portabilidad threading (SBCL, CCL, LispWorks)
- **local-time** - Manejo de fechas
- **CFFI** - Interface con C (para mmap)
- **Ironclad** - SHA1 para UUIDs v5
- **Ningle** - Framework web (opcional, para REST)
- **Clack** - WSGI Lisp (opcional, para REST)

**NO usa:**
- PostgreSQL, MongoDB, Redis, etc.
- Python, Java, Go, etc.
- Compiladores externos
- Servidores separados

## 🎓 Aprendizajes Clave del Proyecto

1. **Arquitectura por Capas** - Cada capa independiente, responsabilidades claras
2. **Persistencia en Lisp** - Memory-mapped files + CLOS = base de datos
3. **Skip Lists vs B-Trees** - Trade-offs de cada estructura
4. **ACID Transactions** - Optimistic locking, read-set/write-set
5. **Prolog en Lisp** - Unificación, backtracking, trail management
6. **Índices Especializados** - VE, VEV, Type indexes para grafos
7. **Replicación Master-Slave** - Protocolo binario, recuperación
8. **REST en Lisp** - Ningle/Clack stack


**Última actualización:** Marzo 2026  
**Versión:** 1.0 Documentada  
**Tamaño total:** 12,088 líneas de código + 8,652 líneas de documentación
