# VivaceGraph - Arquitectura Modular y Estructura de la Librería

## Tabla de Contenidos

1. [Visión General](#visión-general)
2. [Arquitectura en 7 Capas](#arquitectura-en-7-capas)
3. [Dependencias Verticales](#dependencias-verticales)
4. [Archivos Especiales](#archivos-especiales)
5. [Flujo de Inicialización](#flujo-de-inicialización)
6. [Guía de Lectura Recomendada](#guía-de-lectura-recomendada)
7. [Puntos Clave sobre Modularización](#puntos-clave-sobre-modularización)

---

## Visión General

**VivaceGraph** es una base de datos de grafos ACID (Atomicidad, Consistencia, Aislamiento, Durabilidad) escrita en **Common Lisp** puro.

### Características Principales:
- ACID-compliant object graph model (modelo de objetos con garantías ACID)
- Índices personalizables por el usuario
- Vistas tipo map-reduce
- Replicación master/slave para redundancia y escalabilidad horizontal de lectura
- Interfaz mediante métodos Lisp y lenguaje tipo Prolog
- Compatible con: SBCL >= 1.045, LispWorks, Clozure CL

### Inspiración:
El diseño toma inspiración de CouchDB, Neo4j y AllegroGraph.

---

## Arquitectura en 7 Capas

```
VIVACEGRAPH - ARQUITECTURA MODULAR POR CAPAS
=============================================

┌─────────────────────────────────────────────────────────────────────┐
│                     CAPA 7: API & APLICACIONES                      │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  REST         PROLOG        TRAVERSE      INTERFACE      EXAMPLE    │
│  (HTTP API)   (Consultas)   (Grafo)       (Métodos)      (Demo)     │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│                    CAPA 6: SISTEMA DE DATOS                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  VERTEX         EDGE           SCHEMA         VIEWS                 │
│  (Nodos)        (Aristas)      (Definiciones) (Proyecciones)        │
│    │              │                │               │                 │
│    └──────────────┴────────────────┴───────────────┘                 │
│                     PRIMITIVE-NODE                                   │
│               (Clase base de datos)                                  │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│              CAPA 5: INDEXACIÓN & BÚSQUEDA                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  VE-INDEX      VEV-INDEX      TYPE-INDEX     FUNCTOR                │
│  (Vértice→     (Vértice→      (por Tipo)     (Predicados)           │
│   Arista)       Vértice→                                            │
│                 Arista)                                              │
│    │              │              │              │                    │
│    └──────────────┴──────────────┴──────────────┘                    │
│           INDEX-LIST (Índices de Valor)                              │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│           CAPA 4: ESTRUCTURA DE DATOS PERSISTENTE                   │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  SKIP-LIST       SKIP-LIST-CURSORS    LINEAR-HASH      ALLOCATOR   │
│  (Índices       (Iteración)           (Tabla Hash)     (Memoria)   │
│   ordenados)                                                         │
│                                                                       │
│        BUFFER-POOL      SERIALIZE         NODE-ID                   │
│        (Caché)          (Serialización)   (Identificadores)          │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│         CAPA 3: PERSISTENCIA & TRANSACCIONES                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  TRANSACTIONS      TRANSACTION-RESTORE    TRANSACTION-STREAMING     │
│  (Control Txn)     (Recuperación)         (Replicación Txn)         │
│                                                                       │
│         BACKUP          REPLICATION       TXN-LOG                   │
│         (Snapshots)     (Master/Slave)    (Log de Txn)              │
│                                                                       │
│  TRANSACTION-LOG-STREAMING    GC                                     │
│  (Stream para replicación)    (Garbage Collection)                   │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│        CAPA 2: MEMORIA & SINCRONIZACIÓN                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  PCONS         PMEM          PSTRUCT      MMAP                      │
│  (Persistent   (Memory       (Structs)    (Memory Mapped           │
│   Cons Cells)   Model)                     Files)                    │
│                                                                       │
│     RW-LOCK       QUEUE       MAILBOX      CURSORS                  │
│     (Mutex)       (Colas)     (IPC)        (Iteradores)             │
│                                                                       │
└────────┬────────────────────────────────────────────────────────────┘
         │
┌────────▼────────────────────────────────────────────────────────────┐
│       CAPA 1: INFRAESTRUCTURA & UTILIDADES BASE                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  PACKAGE      GLOBALS       CONDITIONS      UTILITIES                │
│  (Paquetes)   (Vars Glob)   (Excepciones)   (Helpers)               │
│                                                                       │
│         CLOS          UUID          RANDOM        STATS              │
│         (CLOS exts)   (Ids)         (RNG)         (Métricas)        │
│                                                                       │
│                   GRAPH-CLASS  NODE-CLASS                            │
│                   (Definiciones de las clases principales)           │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

### Descripción Detallada de Cada Capa

#### CAPA 1: Infraestructura & Utilidades Base

Esta es la **capa fundamental** sobre la que se construye todo lo demás.

**Componentes:**

| Archivo | Propósito | Responsabilidad |
|---------|----------|-----------------|
| `package.lisp` | Define el paquete `:graph-db` | Namespace y exportaciones |
| `globals.lisp` | Variables globales del sistema | Estado global compartido |
| `conditions.lisp` | Excepciones y condiciones | Manejo de errores personalizado |
| `utilities.lisp` | Funciones de utilidad comunes | Helpers reutilizables |
| `clos.lisp` | Extensiones CLOS personalizadas | Metaclases y MOP (Meta-Object Protocol) |
| `uuid.lisp` | Generación de identificadores únicos | Creación de UUIDs para nodos/aristas |
| `random.lisp` | Generador de números aleatorios | Aleatorización (si es necesaria) |
| `stats.lisp` | Métricas y estadísticas | Recolección de datos de rendimiento |
| `graph-class.lisp` | Definición de clase GRAPH | Estructura principal del grafo |
| `node-class.lisp` | Metaclase para nodos persistentes | Base CLOS para nodos |

**Punto de entrada:** `package.lisp` → `globals.lisp` → `conditions.lisp` → `utilities.lisp`

---

#### CAPA 2: Memoria & Sincronización

**Propósito:** Gestionar memoria persistente y sincronización entre procesos.

**Componentes:**

| Archivo | Propósito | Responsabilidad |
|---------|----------|-----------------|
| `pcons.lisp` | Persistent Cons Cells | Celdas Lisp persistentes en memoria |
| `pmem.lisp` | Persistent Memory Model | Modelo de memoria persistente |
| `pstruct.lisp` | Persistent Structures | Estructuras persistentes |
| `mmap.lisp` | Memory Mapped Files | Mapeo de archivos en memoria |
| `rw-lock.lisp` | Read-Write Locks | Sincronización reader-writer |
| `queue.lisp` | Colas thread-safe | Colas para comunicación asíncrona |
| `mailbox.lisp` | Message Passing | Buzones de mensajes para IPC |
| `cursors.lisp` | Iteradores generales | Abstracción para recorrer estructuras |

**Característica clave:** La memoria es **persistente** y **mapeada en archivos**, lo que permite que los datos sobrevivan entre sesiones.

---

#### CAPA 3: Persistencia & Transacciones

**Propósito:** Garantizar ACID-compliance y recuperabilidad de datos.

**Componentes:**

| Archivo | Propósito | Responsabilidad |
|---------|----------|-----------------|
| `transactions.lisp` | Control de transacciones | Inicio, commit, rollback |
| `transaction-restore.lisp` | Recuperación de txn | Restaurar desde logs de transacciones |
| `transaction-streaming.lisp` | Streaming de txn | Replicación de transacciones en tiempo real |
| `transaction-log-streaming.lisp` | Log streaming | Streaming del log de transacciones |
| `txn-log.lisp` | Transaction Log | Log persistente de todas las transacciones |
| `backup.lisp` | Snapshots | Copias de seguridad puntuales |
| `replication.lisp` | Master/Slave Replication | Replicación para redundancia |
| `gc.lisp` | Garbage Collection | Limpieza de datos no referenciados |

**Concepto clave:** Todas las operaciones se registran en un log para poder recuperar el estado en caso de fallo.

---

#### CAPA 4: Estructura de Datos Persistente

**Propósito:** Proporcionar estructuras de datos eficientes que soportan persistencia.

**Componentes:**

| Archivo | Propósito | Responsabilidad |
|---------|----------|-----------------|
| `skip-list.lisp` | Skip Lists | Estructuras ordenadas para índices |
| `skip-list-cursors.lisp` | Skip List Iterators | Iteración sobre skip lists |
| `linear-hash.lisp` | Hash Table Persistente | Tabla hash dinámica y persistente |
| `allocator.lisp` | Memory Allocator | Asignador de memoria persistente |
| `buffer-pool.lisp` | Buffer Pool | Caché de objetos en memoria |
| `serialize.lisp` | Serialización | Conversión de objetos a/desde bytes |
| `node-id.lisp` | Identificadores de Nodo | Gestión de IDs únicos para nodos |
| `index.lisp` | Índices Generales | Abstracción base para índices |
| `index-list.lisp` | Índices Basados en Listas | Índices sobre atributos de valor |
| `index-vector.lisp` | Índices Vectoriales | Índices para búsquedas vectoriales |

**Punto crítico:** Las skip lists permiten búsqueda O(log n) incluso en datos persistentes.

---

#### CAPA 5: Indexación & Búsqueda

**Propósito:** Proporcionar búsqueda eficiente en grafos mediante índices especializados.

**Componentes:**

| Archivo | Propósito | Responsabilidad |
|---------|----------|-----------------|
| `ve-index.lisp` | Vertex→Edge Index | Índice de qué aristas salen/entran en cada vértice |
| `vev-index.lisp` | Vertex→Edge→Vertex Index | Índice de conectividad directa entre vértices |
| `type-index.lisp` | Type-based Index | Índice de vértices/aristas por tipo |
| `functor.lisp` | Functores | Predicados para consultas |
| `prolog-functors.lisp` | Prolog Predicates | Predicados en estilo Prolog |
| `prologc.lisp` | Prolog Compiler | Compilador del motor Prolog |

**Índices especializados:**
- **VE-Index:** `(vértice-id) → lista de arista-ids` (entrada y salida)
- **VEV-Index:** `(vértice-id-1, vértice-id-2) → arista-id` (acceso directo)
- **Type-Index:** `(tipo) → lista de nodo-ids` (filtrado por tipo)

---

#### CAPA 6: Sistema de Datos

**Propósito:** Definir el modelo de objetos (vértices y aristas) que forman el grafo.

**Componentes:**

| Archivo | Propósito | Responsabilidad |
|---------|----------|-----------------|
| `vertex.lisp` | Clase Vertex | Definición de nodos/vértices |
| `edge.lisp` | Clase Edge | Definición de aristas/relaciones |
| `primitive-node.lisp` | Node Base Class | Clase base persistente para todos los nodos |
| `schema.lisp` | Schema Definition | Definición de tipos y propiedades |
| `views.lisp` | Map-Reduce Views | Proyecciones y vistas del grafo |

**Estructura:**
```
PRIMITIVE-NODE (clase base persistente)
    ↓
    ├─→ VERTEX (nodos del grafo)
    └─→ EDGE (aristas del grafo)
```

**Propiedades de un Vertex:**
- `id`: Identificador único (UUID)
- `type-id`: Tipo de nodo (para polimorfismo)
- `revision`: Número de versión
- `deleted-p`: Flag de borrado lógico
- Datos personalizados según el esquema

**Propiedades de un Edge:**
- Todas las de Vertex, más:
- `from`: ID del vértice origen
- `to`: ID del vértice destino
- `weight`: Peso numérico (float)

---

#### CAPA 7: API & Aplicaciones

**Propósito:** Proporcionar interfaces de acceso a los datos.

**Componentes:**

| Archivo | Propósito | Responsabilidad |
|---------|----------|-----------------|
| `interface.lisp` | API de métodos Lisp | Funciones CLOS para manipular grafo |
| `traverse.lisp` | Graph Traversal | Algoritmos de recorrido (BFS, DFS) |
| `rest.lisp` | REST API | Servidor HTTP con Hunchentoot |
| `prologc.lisp` + `prolog-functors.lisp` | Prolog Interface | Lenguaje tipo Prolog para consultas |
| `example.lisp` | Ejemplos de uso | Demostraciones |

**Formas de usar VivaceGraph:**
1. **Métodos Lisp:** `(add-vertex graph :type "User" :name "Alice")`
2. **API REST:** `POST /api/vertices { "type": "User", "name": "Alice" }`
3. **Prolog:** `?- vertex(X), type(X, 'User')`

---

## Dependencias Verticales

Las dependencias siempre van **hacia arriba** (de capas bajas a capas altas).

```
FLUJO DE DEPENDENCIAS
=====================

Capa 7 (API)     ← Depende de: Capa 6, 5, 4
                   Necesita: métodos para acceder, índices para buscar, 
                             estructuras para almacenar

Capa 6 (Datos)   ← Depende de: Capa 5, 4, 3, 1
                   Necesita: índices para buscar vértices/aristas,
                             transacciones, infraestructura

Capa 5 (Índices) ← Depende de: Capa 4, 1
                   Necesita: estructuras de datos, utilidades base

Capa 4 (DS)      ← Depende de: Capa 3, 2, 1
                   Necesita: persistencia, memoria, infraestructura

Capa 3 (Txn)     ← Depende de: Capa 2, 1
                   Necesita: sincronización, utilidades

Capa 2 (Mem)     ← Depende de: Capa 1
                   Necesita: utilidades, infraestructura

Capa 1 (Base)    ← No depende de nada interno
                   Es la RAÍZ del árbol de dependencias
```

**Regla fundamental:** Nunca hay dependencias circulares.

---

## Archivos Especiales

Algunos archivos están **fuera del flujo principal** o tienen roles transversales:

### Fuera del flujo:

| Archivo | Rol |
|---------|-----|
| `chart.lisp` | Visualización de grafos (sin dependencias internas) |
| `example.lisp` | Ejemplos de uso |
| `test.lisp` | Tests unitarios |
| `test-lhash.lisp` | Tests para linear-hash |
| `xach-test.lisp` | Tests adicionales |

### Directorio de demostración (`/demo/`):

```
social-shopping/ (proyecto demostrativo)
├─ social-shopping.asd
├─ package.lisp
├─ schema.lisp
├─ collections.lisp
├─ customer.lisp
├─ want-list.lisp
└─ graph-views.lisp
```

Este es un ejemplo completo de cómo usar VivaceGraph para una aplicación de e-commerce social.

---

## Flujo de Inicialización

Cuando cargas VivaceGraph en SBCL/Clozure, el sistema ASDF carga los archivos en este **orden exacto**:

```
ORDEN DE CARGA (definido en graph-db.asd)
==========================================

1. package.lisp ...................... Paquete :graph-db
2. globals.lisp ...................... Variables globales
3. conditions.lisp ................... Excepciones
4. utilities.lisp .................... Funciones de utilidad
5. queue.lisp ........................ Colas
6. mailbox.lisp ...................... Buzones de mensajes
7. rw-lock.lisp ...................... Locks (si SBCL/LispWorks)
8. mmap.lisp ......................... Memory-mapped files
9. pcons.lisp ........................ Persistent cons cells
10. node-id.lisp ..................... Generación de IDs
11. buffer-pool.lisp ................. Buffer pool/caché
12. serialize.lisp ................... Serialización
13. linear-hash.lisp ................. Hash table persistente
14. allocator.lisp ................... Asignador de memoria
15. graph-class.lisp ................. Clase GRAPH
16. cursors.lisp ..................... Iteradores
17. skip-list.lisp ................... Skip lists para índices
18. skip-list-cursors.lisp ........... Iteradores de skip lists
19. index-list.lisp .................. Índices por lista
20. ve-index.lisp .................... Índices Vertex→Edge
21. vev-index.lisp ................... Índices Vertex→Edge→Vertex
22. type-index.lisp .................. Índices por tipo
23. graph.lisp ....................... Clase GRAPH completa con métodos
24. stats.lisp ....................... Estadísticas
25. schema.lisp ...................... Definición de esquema
26. node-class.lisp .................. Metaclase para nodos
27. views.lisp ....................... Vistas map-reduce
28. primitive-node.lisp .............. Clase base de nodos persistentes
29. vertex.lisp ...................... Clase VERTEX
30. edge.lisp ........................ Clase EDGE
31. gc.lisp .......................... Garbage collection
32. transactions.lisp ................ Control de transacciones
33. transaction-restore.lisp ......... Recuperación de txn
34. transaction-log-streaming.lisp .. Log streaming
35. transaction-streaming.lisp ....... Replicación de txn
36. backup.lisp ...................... Snapshots
37. replication.lisp ................. Master/slave replication
38. txn-log.lisp ..................... Transaction log
39. functor.lisp ..................... Functores para consultas
40. prologc.lisp ..................... Compilador Prolog
41. prolog-functors.lisp ............. Predicados Prolog
42. interface.lisp ................... API de métodos Lisp
43. traverse.lisp .................... Graph traversal algorithms
44. rest.lisp ........................ REST API

DEMOSTRACIÓN:
social-shopping.asd (proyecto separado que depende de graph-db)
```

**Nota importante:** Este orden es **crítico**. Si cambias el orden, puede haber errores de dependencia no resueltos.

---

## Guía de Lectura Recomendada

Antes de documentar o modificar VivaceGraph, lee en este orden:

### Fase 1: Fundamentos (2-3 horas)
1. `package.lisp` - Entiende el namespace
2. `globals.lisp` - Variables compartidas
3. `utilities.lisp` - Helpers comunes
4. `conditions.lisp` - Manejo de errores

**Objetivo:** Entender la infraestructura base.

### Fase 2: Memoria (2-3 horas)
5. `node-id.lisp` - Sistema de IDs
6. `pcons.lisp` - Estructuras persistentes
7. `mmap.lisp` - Memory mapping
8. `rw-lock.lisp` - Sincronización

**Objetivo:** Cómo se almacenan y sincronizan los datos.

### Fase 3: Persistencia (3-4 horas)
9. `serialize.lisp` - Conversión a/desde disco
10. `allocator.lisp` - Gestión de memoria
11. `buffer-pool.lisp` - Caché en RAM
12. `transactions.lisp` - Control ACID

**Objetivo:** Cómo se garantiza durabilidad.

### Fase 4: Estructuras de Datos (3-4 horas)
13. `linear-hash.lisp` - Hash tables
14. `skip-list.lisp` - Listas ordenadas
15. `index.lisp` - Abstracción de índices
16. `index-list.lisp` - Índices por valor

**Objetivo:** Qué estructuras usa internamente para ser rápida.

### Fase 5: Índices de Grafo (2-3 horas)
17. `ve-index.lisp` - Aristas por vértice
18. `vev-index.lisp` - Conectividad vértice-vértice
19. `type-index.lisp` - Búsqueda por tipo

**Objetivo:** Cómo se optimiza la búsqueda en grafos.

### Fase 6: Modelo de Datos (2-3 horas)
20. `node-class.lisp` - Metaclase base
21. `vertex.lisp` - Nodos
22. `edge.lisp` - Aristas
23. `schema.lisp` - Tipos de nodos

**Objetivo:** Qué puedes almacenar.

### Fase 7: Interfaz de Usuario (2-3 horas)
24. `interface.lisp` - Métodos Lisp
25. `traverse.lisp` - Recorridos
26. `prologc.lisp` - Lenguaje Prolog
27. `rest.lisp` - API REST

**Objetivo:** Cómo se accede a los datos.

**Tiempo total recomendado:** 20-25 horas de lectura atenta.

---

## Puntos Clave sobre Modularización

### ✓ Fortalezas

1. **Arquitectura en capas bien definida**
   - Cada capa tiene responsabilidades claras
   - Las dependencias son unidireccionales
   - No hay ciclos de dependencia

2. **Separación de conceptos**
   - Memoria ↔ Persistencia ↔ Índices ↔ Datos ↔ API
   - Cada parte puede estudiarse aisladamente

3. **Orden de carga explícito**
   - El archivo `graph-db.asd` define perfectamente las dependencias
   - ASDF garantiza que se cargan en orden correcto

4. **Reutilización**
   - Las capas bajas (índices, estructuras) se reutilizan en múltiples contextos
   - El diseño favorece composición

### ✗ Áreas de Mejora

1. **Falta de subdirectorios**
   - 52 archivos `.lisp` en la raíz
   - Sería más claro: `mem/`, `persist/`, `index/`, `data/`, `api/`

2. **Nomenclatura inconsistente**
   - Falta prefijo común en nombres relacionados
   - Por ejemplo: `buffer-pool.lisp` vs `skip-list.lisp` vs `ve-index.lisp`
   - Mejor sería: `mem-buffer-pool.lisp`, `ds-skip-list.lisp`, `idx-ve-index.lisp`

3. **Algunos archivos con múltiples responsabilidades**
   - `graph.lisp` contiene métodos principales de GRAPH
   - `transactions.lisp` mezcla control de txn con recuperación
   - Podrían dividirse en más de un archivo

4. **Documentación inexistente**
   - Muy pocos comentarios de arquitectura
   - Falta docstrings en muchas funciones
   - README es minimal

### Recomendación para Documentación

**No reorganices el código.** En cambio:

1. **Crea una estructura de documentación** que refleje las capas:
   ```
   docs/
   ├─ 01-FUNDAMENTALS.md       (Capa 1)
   ├─ 02-MEMORY.md              (Capa 2)
   ├─ 03-TRANSACTIONS.md        (Capa 3)
   ├─ 04-DATA-STRUCTURES.md     (Capa 4)
   ├─ 05-INDEXING.md            (Capa 5)
   ├─ 06-DATA-MODEL.md          (Capa 6)
   ├─ 07-API.md                 (Capa 7)
   ├─ ARCHITECTURE.md           (este documento)
   ├─ GETTING-STARTED.md
   └─ API-REFERENCE.md
   ```

2. **En cada documento**, mapea qué archivos pertenecen a qué concepto

3. **Crea un índice cruzado** archivo → concepto

4. **Añade diagramas** específicos para cada subsistema

---

## Estructura de Archivos Completa

```
VivaceGraph/ (raíz)
│
├─ graph-db.asd ..................... Definición del sistema ASDF
├─ README.md ........................ Descripción general
├─ example.lisp ..................... Ejemplo de uso
│
├─ CAPA 1: Infraestructura
│  ├─ package.lisp
│  ├─ globals.lisp
│  ├─ conditions.lisp
│  ├─ utilities.lisp
│  ├─ clos.lisp
│  ├─ uuid.lisp
│  ├─ random.lisp
│  ├─ stats.lisp
│  ├─ graph-class.lisp
│  └─ node-class.lisp
│
├─ CAPA 2: Memoria & Sincronización
│  ├─ pcons.lisp
│  ├─ pmem.lisp
│  ├─ pstruct.lisp
│  ├─ mmap.lisp
│  ├─ rw-lock.lisp
│  ├─ queue.lisp
│  ├─ mailbox.lisp
│  └─ cursors.lisp
│
├─ CAPA 3: Persistencia & Transacciones
│  ├─ transactions.lisp
│  ├─ transaction-restore.lisp
│  ├─ transaction-streaming.lisp
│  ├─ transaction-log-streaming.lisp
│  ├─ txn-log.lisp
│  ├─ backup.lisp
│  ├─ replication.lisp
│  └─ gc.lisp
│
├─ CAPA 4: Estructuras de Datos Persistentes
│  ├─ skip-list.lisp
│  ├─ skip-list-cursors.lisp
│  ├─ linear-hash.lisp
│  ├─ allocator.lisp
│  ├─ buffer-pool.lisp
│  ├─ serialize.lisp
│  ├─ node-id.lisp
│  ├─ index.lisp
│  ├─ index-list.lisp
│  └─ index-vector.lisp
│
├─ CAPA 5: Indexación & Búsqueda
│  ├─ ve-index.lisp
│  ├─ vev-index.lisp
│  ├─ type-index.lisp
│  ├─ functor.lisp
│  ├─ prolog-functors.lisp
│  └─ prologc.lisp
│
├─ CAPA 6: Sistema de Datos
│  ├─ vertex.lisp
│  ├─ edge.lisp
│  ├─ primitive-node.lisp
│  ├─ schema.lisp
│  └─ views.lisp
│
├─ CAPA 7: API & Aplicaciones
│  ├─ interface.lisp
│  ├─ traverse.lisp
│  └─ rest.lisp
│
├─ Utilidades & Tests
│  ├─ chart.lisp ..................... Visualización
│  ├─ test.lisp ...................... Tests generales
│  ├─ test-lhash.lisp ................ Tests de hash
│  └─ xach-test.lisp ................. Tests adicionales
│
└─ demo/ ............................. Proyecto demostrativo
   ├─ social-shopping.asd
   ├─ package.lisp
   ├─ schema.lisp
   ├─ collections.lisp
   ├─ customer.lisp
   ├─ want-list.lisp
   └─ graph-views.lisp
```

---

## Resumen Ejecutivo

**VivaceGraph** es una base de datos de grafos ACID escrita en Common Lisp, con una arquitectura bien estructurada en 7 capas:

1. **Infraestructura base** - Namespace, variables, excepciones, utilidades
2. **Memoria y sincronización** - Persistencia, locks, colas
3. **Transacciones** - Garantías ACID, logs, recuperación
4. **Estructuras de datos** - Hash tables, skip lists, serialización
5. **Indexación especializada** - Índices de grafo (VE, VEV, por tipo)
6. **Modelo de datos** - Vértices, aristas, esquemas
7. **APIs de acceso** - Métodos Lisp, Prolog, REST

La aparente "falta de modularización" (52 archivos en raíz) es **cosmética**. La arquitectura es en realidad **muy bien estructurada** con dependencias claras y unidireccionales.

**Para documentar:** Lee siguiendo las 7 fases recomendadas (20-25 horas de lectura atenta) y organiza la documentación por capas.

---

*Documento generado para facilitar la documentación de VivaceGraph*
*Arquitectura analizada: marzo 2026*
