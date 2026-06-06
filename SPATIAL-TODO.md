# Spatial Extension — TODO

Status of the geohash-backed spatial extension (geometry type, index, write-path
maintenance, queries). Merged into `experiment`; full suite green on SBCL
(1371/0) and ECL (1366/0, 1 pre-existing skip). See manual Chapter 13.

## Done

- `geometry.lisp` — geometry value type (point/linestring/polygon/multipolygon),
  WGS84 `(lon lat)` doubles, serialization (type tag 102), `geometry-bbox`.
- `geohash.lisp` — encode/decode/bbox/cell-size/covering/prefix-range.
- `geometry-ops.lisp` — haversine distance, point-in-polygon (+holes),
  multipolygon containment, bbox overlap.
- `spatial-index.lisp` — heap-backed geohash skip-list index, fixed precision
  (default 7), insert/remove/query-bbox/query-radius, persisted via a
  `spatial-index.root` sidecar.
- Graph lifecycle — `make-graph`/`open-graph`/`close-graph` create/reopen the index.
- Write-path hook (`transactions.lisp`) — auto-maintains the index on
  create/update/delete via the `node-geometry` protocol.
- Pure Prolog predicates — `geo-distance/5`, `geo-near/5`, `geo-within/3`.
- Index-backed queries — `find-nodes-within` / `find-nodes-near` (Lisp);
  `find-within/2`, `find-near/4` (Prolog, yield nodes).
- Public API exported from the `graph-db` package.
- Manual Chapter 13 + README (see "Doc fixes" below for two corrections).

## Known limitations (current design choices)

- **`find-within` on extended geometries is approximate WITHOUT GEOS** — matches
  by representative point (the point itself for `:point`; bbox centre otherwise).
  With the optional `graph-db/geos` add-on it is **exact** (GEOS containment); the
  centroid path is only the dependency-free fallback.
- **polygon↔polygon ops need GEOS** — intersects + exact containment + validity
  repair (`make-valid`) live in the optional `graph-db/geos` add-on; core has only
  point-in-polygon + a coarse bbox-overlap fallback for intersects.
- **Distance is haversine** (~0.5% vs ellipsoid); `geometry-distance-exact` (GEOS)
  is PLANAR in coordinate units (degrees for lon/lat), not metres — use it for
  ordering, not real distance.
- **Single grid precision per graph** — set with `make-graph :spatial-precision`
  (default 7, ~150 m cells); geohash (Z-order), not Hilbert.
- **No regenerate/rebuild** of the index (views have `regenerate-view`). *(Done —
  see `rebuild-spatial-index`.)*

## TODO

### P1 — Functional gaps for the platform
- [x] **Subset-replication filter** — DONE (prototype): a `replication-filter`
      predicate slot on `slave-graph` (set via `make-graph :replication-filter`);
      `apply-transaction` runs `filter-writes` on a slave so it applies only its
      subset (txn id still advances). `make-spatial-replication-filter` builds an
      area-of-operations predicate (accepts non-spatial nodes + spatial nodes in
      the area). Unit-tested, and validated end-to-end in the `tests/replication/`
      harness (in-AO replicated/indexed, out-of-AO filtered). **Remaining:**
      AO-boundary-crossing *updates* (a node moving in/out of the AO) are not yet
      reconciled.
- [x] **`find-intersects` query** — DONE (`graph-db/geos`): `find-nodes-intersecting`
      (Lisp) + `find-intersects/2` (Prolog), index bbox candidates refined by the
      `geometry-intersects-p` seam (exact with GEOS, coarse bbox fallback without).
- [x] **`rebuild-spatial-index`** — DONE (`spatial-query.lisp`): drop + recreate the
      index, re-index every live node with a `node-geometry`; optional `:precision`
      change; returns the count. Mirrors `regenerate-view`.
- [x] **Exact extended-geometry containment** — DONE (`graph-db/geos`):
      `find-nodes-within` routes non-point candidates through the exact
      `geometry-contains-geometry-p` (GEOS) seam, dropping the centroid
      approximation; without the add-on it falls back to the old centroid path.

### P2 — Accuracy & robustness
- [x] **GEOS integration** — DONE: evaluated `cl-geos` (unmaintained since 2018;
      no buffer/makeValid/distance; unsafe shared global context; won't load on
      macOS arm64) and instead built an OPTIONAL in-house CFFI binding
      `graph-db/geos` to libgeos_c's reentrant `_r` API. Provides exact
      `geometry-intersects-p` / `geometry-contains-geometry-p` / `geometry-make-valid`
      / `geometry-valid-p` / `geometry-distance-exact` behind a generic-function
      refine seam (dependency-free fallbacks in core; core stays libgeos-free).
      Threads share a borrow/return **context pool** (`with-geos-context`) — never
      two threads per context. Green on SBCL+ECL incl. a concurrency storm and a
      shapely oracle cross-check. *(Remaining: union/buffer + geodesic polygon
      distance not yet bound; centroid fallback still used when the add-on is absent.)*
- [x] **Geohash neighbors/adjacent** — DONE (`geohash.lisp`): `geohash-neighbor`
      (step one cell in lon/lat, wraps the antimeridian, NIL off a pole) and
      `geohash-neighbors` (the 8 surrounding same-precision cells). Covered by
      `geohash-suite`.
- [x] **kNN (`find-nearest-k`)** — DONE (`spatial-query.lisp`): the K nearest
      nodes to a point, nearest-first, via radius-doubling over `find-nodes-near`
      (correct because everything inside radius r is nearer than anything outside
      it). Bounded by `:max-radius` (default 25 km) since the fixed-precision
      geohash index enumerates cells per window — unbounded kNN is not supported.
      Prolog `find-nearest/4`. Covered by `spatial-query-suite`.
- [x] **Snapshot → restore → spatial-query test** — DONE (backup-suite): replay
      into a fresh graph re-applies nodes through the write-path hook, repopulating
      the spatial index (verified queryable; empty before replay).
- [x] **Replicated-index test** — DONE (tests/replication harness): the slave
      maintains its spatial index on replicated apply (catch-up + live).  The same
      run also covers end-to-end subset filtering (in-AO places replicated/indexed,
      out-of-AO filtered).
- [x] Point-in-polygon **boundary semantics** — DONE: documented the half-open
      PNPOLY rule (a point on an edge shared by two polygons lands in exactly one
      of them — no double-count, no gap; which side wins is not part of the
      contract) in `geometry-ops.lisp`, and pinned it with `geometry-ops-suite`
      tests (edge tiling XOR, determinism, interior/exterior unambiguous).

### P3 — Performance & scale
- [ ] **Load the real ~458k-image / 440-find dataset** onto a spatial graph;
      measure index insert + query at scale (no spatial perf coverage yet).
- [ ] **Compact geometry serialization** (flat double-array vs nested-list); tag,
      struct, and API stay stable.
- [ ] **Hilbert-curve migration** behind the same query API if geohash range
      fragmentation hurts at scale.
- [x] Concurrency/stress coverage for the index — DONE
      (`tests/concurrency/spatial-tests.lisp`, `concurrent-spatial-suite`): N-thread
      concurrent inserts, interleaved insert+query, cell-moving updates, and
      concurrent deletes, all asserting index consistency. Green on SBCL + ECL.

### P4 — Polish, docs, cross-impl
- [ ] **CCL** verification on Linux (only SBCL + ECL run so far).
- [ ] **H3 density helper + aggregation view** (contamination-heatmap building
      block); geohash was used for the index instead.
- [x] Wire the **`:index` slot flag** so a geometry slot opts into indexing
      declaratively (vs hand-written `node-geometry`) — DONE: `node-geometry`'s
      default scans the node's `:index`-marked slots and returns the first whose
      *value* is a geometry (robust to the app package of the `:type geometry`
      symbol; `(slot :type geometry :index t)` is now enough). An explicit
      `node-geometry` method still takes precedence. Covered by
      `index-slot-flag-auto-wires-node-geometry`.
- [x] **`example.lisp`** — DONE: MERCHANT gains a `(location :type geometry
      :index t)` slot (auto-indexed), and the script ends with a spatial
      walkthrough — `find-nodes-near`, `find-nearest-k`, `find-nodes-within`, and
      a Prolog `find-near` composed with `is-a`. Runs clean end-to-end.
- [ ] Cleanup — delete the merged `spatial-index` branch + `spatial-index-prerebase`
      tag.

### Done (this pass)
- [x] `make-graph :spatial-precision` keyword (persisted; read back by `open-graph`).
- [x] Chapter 13 caveat fixes: corrected the "prefix range scans" description
      (queries do same-precision exact-cell lookups) and the precision-config claim
      (now true via `:spatial-precision`).
