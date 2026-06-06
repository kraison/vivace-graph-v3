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

- **`find-within` on extended geometries is approximate** — matches by
  representative point (the point itself for `:point`; bbox centre otherwise).
  Exact for the point-in-area case; a polygon node is judged by its centroid.
- **No polygon↔polygon ops** — no intersects/union/buffer/validity-repair; only
  point-in-polygon refine. Self-intersecting polygons can't be repaired in-engine.
- **Distance is haversine** (~0.5% vs ellipsoid).
- **Single grid precision per graph** — set with `make-graph :spatial-precision`
  (default 7, ~150 m cells); geohash (Z-order), not Hilbert.
- **`node-geometry` must be hand-specialized** per type (the `:index` slot flag
  is not wired to auto-register a geometry slot).
- **No regenerate/rebuild** of the index (views have `regenerate-view`).

## TODO

### P1 — Functional gaps for the platform
- [ ] **Subset-replication filter** — `apply-tx-write` on `slave-graph` filtered by
      area-of-operations, so a field device replicates only its AO. Does not need
      snapshot-isolation reads.
- [ ] **`find-intersects` query** — nodes whose geometry *intersects* an area
      (needs polygon ops → GEOS).
- [ ] **`rebuild-spatial-index`** — regenerate from all nodes (recovery, precision
      change, adopting the index on a pre-existing graph). Mirror `regenerate-view`.
- [ ] **Exact extended-geometry containment** — replace the centroid approximation
      in `find-within` (depends on GEOS).

### P2 — Accuracy & robustness
- [ ] **Evaluate/harden `cl-geos`** for intersects/union/`buffer(0)` repair, with
      the hand-rolled predicates as fallback (watch per-thread GEOS contexts +
      finalizers under VG concurrency).
- [ ] **Geohash neighbors/adjacent** — enables true kNN and cell-boundary-spanning
      proximity.
- [ ] **Snapshot → restore → spatial-query test** — confirm the index rebuilds via
      replay + the write-path hook (untested).
- [ ] **Replicated-index test** — confirm a slave's index populates via the hook
      on apply.
- [ ] Point-in-polygon **boundary semantics** — currently consistent but not
      flagged; document/standardize.

### P3 — Performance & scale
- [ ] **Load the real ~458k-image / 440-find dataset** onto a spatial graph;
      measure index insert + query at scale (no spatial perf coverage yet).
- [ ] **Compact geometry serialization** (flat double-array vs nested-list); tag,
      struct, and API stay stable.
- [ ] **Hilbert-curve migration** behind the same query API if geohash range
      fragmentation hurts at scale.
- [ ] Concurrency/stress coverage for the index (the storm suites don't touch it).

### P4 — Polish, docs, cross-impl
- [ ] **CCL** verification on Linux (only SBCL + ECL run so far).
- [ ] **H3 density helper + aggregation view** (contamination-heatmap building
      block); geohash was used for the index instead.
- [ ] Wire the **`:index` slot flag** so a geometry slot opts into indexing
      declaratively (vs hand-written `node-geometry`).
- [ ] **`example.lisp`** — add a spatial walkthrough.
- [ ] Cleanup — delete the merged `spatial-index` branch + `spatial-index-prerebase`
      tag.

### Done (this pass)
- [x] `make-graph :spatial-precision` keyword (persisted; read back by `open-graph`).
- [x] Chapter 13 caveat fixes: corrected the "prefix range scans" description
      (queries do same-precision exact-cell lookups) and the precision-config claim
      (now true via `:spatial-precision`).
