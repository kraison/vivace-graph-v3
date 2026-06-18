# Changelog

All notable changes to VivaceGraph are recorded here.

## Unreleased

### Added
- **Prolog control-flow core (issue #45, Phase 0).** `not`/`\+`, `if` (the
  two- and three-argument `Cond -> Then [; Else]` soft cut), `once`, and `forall`
  are now first-class compiler constructs: they expand through `compile-body`, so
  they thread bindings and compose with conjunction and cut instead of routing
  through the runtime `call/1` functors.  Each opaque construct (`not`, `once`,
  the condition of `if`) is a proper cut barrier, while a cut in a `Then`/`Else`
  branch or in the tail after the construct still cuts the enclosing clause.
  A non-static (meta-call variable) sub-goal, e.g. `(not ?G)`, transparently
  falls back to the runtime functor, so existing behavior is preserved.
- **Compiled `call/N` and a runtime meta-call solver (issue #45, Phase 0.2).**
  `(call Goal Extra...)` now appends the extra arguments to `Goal` (call/N).
  When `Goal` is a static template the call compiles inline through
  `compile-body`, so it composes with cut and the control constructs (e.g.
  `(call (or ...))`, `(call (g-knows ?a) ?b)`).  When `Goal` is a variable, the
  new `%solve` runtime solver proves it -- handling conjunction, disjunction,
  call/N and atomic/compound goals.  `call/1` and the control runtime functors
  (`not`/`if`/`once`/`forall`) route through `%solve`.
- **All-solutions aggregation (issue #45, Phase 0.3).** New `findall/3` (collects
  every template instance in order, always succeeds, `[]` on no solutions).
  `bagof/3` and `setof/3` now **group by the goal's free (witness) variables** --
  the variables in the goal but not in the template and not existentially
  quantified -- yielding one solution per witness binding (and still failing when
  the goal has no solutions).  The `^` operator (`(^ Var Goal)`, nestable, accepts
  a single var or a list) marks variables as existential so they are not treated
  as witnesses.  `setof` sorts each group by the standard order of terms and
  removes duplicates.
- **Query resource bounds (issue #45, Phase 0.4).** Queries can now be bounded by
  a maximum inference count (`:max-inferences` select option / `*inference-budget*`
  / `*default-inference-budget*`) and a wall-clock timeout (`:timeout` seconds /
  `*default-query-timeout*`).  Exceeding either aborts the query with a catchable
  `prolog-resource-error`, so a runaway, non-terminating, or cyclic-recursive
  query fails cleanly instead of hanging or overflowing the Lisp control stack.
  Both default to nil (unlimited): trusted queries are unchanged, untrusted ones
  (e.g. the planned #44 web surface) opt in.  Solution count remains bounded by
  the existing `:limit`.
- **Effect partitioning / query effect policy (issue #45, Phase 1).** The
  side-effecting Prolog functors are now tagged by effect -- `:write` (graph
  mutation: `retract`), `:eval` (arbitrary Lisp: `lisp`/`lispp`/`is`/`trigger`),
  `:io` (`read`/`write`/`nl`) -- and check the per-query policy before acting.
  The `:effects` select option (or `*allowed-effects*` / `*default-allowed-effects*`)
  is `t` for all (the default) or a list of permitted tags; a disallowed effect
  aborts with a catchable `prolog-permission-error`.  Reads and pure logic are
  always allowed, so `:effects nil` is a safe read-only query mode (the basis for
  exposing queries to untrusted callers).  The check is transitive -- an effect
  reached through a user rule or meta-call is gated the same way.
- **Snapshot query mode (issue #45, Phase 1).** `select` accepts `:snapshot t`,
  which runs the query under a single consistent MVCC read snapshot: every read
  resolves at one epoch, so the result is stable against concurrent writers (a
  vertex committed after the query started is invisible to it).  Implemented as
  a lightweight read transaction (`with-read-snapshot` / `call-with-read-snapshot`)
  that registers active for the query's extent -- holding the reaper's retention
  floor -- and is discarded without commit or validation.  It inherits an
  enclosing transaction if one is already active.  Together with the resource
  bounds, the effect policy, and `:limit`/`:skip`, this gives a query surface
  safe to expose to untrusted callers.

### Changed
- **Unknown Prolog predicates are now noisy.** A goal naming an undefined
  predicate signals a `prolog-error` -- on both the compiled query path and the
  dynamic meta-call path -- instead of silently yielding no answers (the
  compiled path) or aborting with an opaque message (the old `call/1`).  This
  surfaces mistyped predicate names instead of letting them masquerade as empty
  results.  (A future `catch/3` + ISO `existence_error` will make this
  recoverable; see #45.)

- **`select-count` / `select` `:count`.** `select-count` (already exported but
  never implemented) now returns the integer number of solutions to a query
  without projecting or consing any per-solution bindings; the underlying
  `select` `:count t` option does the same and composes with `:limit` and
  `:skip` (so a capped or offset count counts the rows `select` would return).

- **`def-query` -- named parameterized queries over the web (issue #44).** A new
  `def-query` registers a server-authored, read-only graph query as a REST
  endpoint at `POST /graph/:graph/query/<name>`.  The author declares typed,
  named parameters (`:string`/`:integer`/`:number`/`:boolean`/`:keyword`),
  result variables, and the query goals; the client supplies only the
  parameters.  Each query runs through `select` with safe defaults the author
  may override -- read-only (`:effects nil`), and a result limit, inference
  budget, and wall-clock timeout.  A read-only query runs under a lightweight
  MVCC read snapshot; a query whose `:effects` permit side effects instead runs
  inside a `with-transaction`, so its writes flatten into one transaction that
  provides the same snapshot and commits on success (or rolls back if a bound or
  permission error aborts it).  Responses are a JSON array of objects keyed by the camelCase result
  names; a missing/malformed parameter is a 400, a resource-bound breach a 400,
  a forbidden effect a 403, and an unknown query a 404.  Parameter values are
  injected through a new pure `param/2` functor (and `*query-params*`), so
  injection works under the read-only policy.

### Fixed
- **REST procedures were broken on ECL.** `*rest-procedures*` had `#+sbcl`/
  `#+ccl`/`#+lispworks` initforms but no `#+ecl` branch, so on ECL the variable
  was declared special but left unbound; `def-rest-procedure` and
  `call-rest-procedure` then failed with an `unbound-variable` error.  Added the
  `#+ecl` branch so REST procedures work on ECL like the other implementations.
- **`node-slot-value/3` swallowed downstream query errors.** Its `handler-case`
  wrapped `(funcall cont)` -- the continuation -- so an error raised by any goal
  *after* a `node-slot-value` (e.g. a `prolog-permission-error` from a denied
  write, or a `prolog-resource-error`) was caught and silently turned into a
  non-match.  The guard now wraps only the slot read; the continuation runs
  outside it so downstream errors propagate.
- **Prolog `if/3` else-semantics (issue #45).** `(if Test Then Else)` now runs
  `Else` only when `Test` has no solution; previously it also ran `Else` when
  `Test` succeeded but `Then` failed.  The runtime `if/3` functor (the meta-call
  path) was corrected to match.
- **Prolog `or` binding propagation.** A variable first bound inside a disjunct
  (e.g. `(or (= ?x 1) (= ?x 2))`) was lost across the disjunction's shared
  continuation because `=` had been optimized to a compile-time alias.  The `or`
  compiler macro now seeds its fresh variables so they bind on the trail at
  runtime and are visible to the continuation.
- **ECL spatial-index concurrency (issue #42).** The skip list guarded every
  operation -- reads included -- with one recursive lock on ECL, so concurrent
  spatial queries ran sequentially and timed out under high parallelism.  Replaced
  it with a per-skip-list reader/writer lock: shared read lock for readers (find,
  cursor scans, map, count), exclusive write lock for mutators.  Writers never run
  concurrently with readers (torn-read safety preserved); concurrent readers now
  run in parallel.  No-op on non-ECL (those keep the lock-free design).

## 2.0.0

A major release: MVCC versioned nodes, a geohash spatial extension, a full
cross-implementation port (SBCL/CCL/ECL) with a comprehensive automated test
suite, and an ACID-compliance audit.

### Added
- **MVCC — immutable, versioned nodes (issue #19).** Each update now retains the
  prior version of a node in a heap-backed version chain instead of freeing it in
  place. Old versions are reclaimed by a lazy, epoch-gated reaper once no active
  reader or transaction can still observe them. Configurable retention via
  `:keep-revisions` (per node type, with a graph-level default), and
  snapshot-isolation reads for transactional lookups. As a bonus this dissolves
  the long-standing node-data read-after-free race at its source.
- **Spatial extension.** A geohash-backed, heap-resident spatial index answers
  proximity and area queries over nodes that carry a `geometry`. Declarative
  opt-in via a `:index t` geometry slot (`node-geometry` auto-wiring), bounding-box
  and radius queries, k-nearest-neighbour search, and geohash neighbour
  enumeration. Optional `graph-db/geos` integration adds exact topology, validity
  repair, and accurate distance via libgeos. (See Chapter 13 of the manual.)
- **ECL support.** The full `:graph-db` system — including the REST layer — builds
  and runs on ECL 26.5.5; the entire test suite is green on ECL (macOS arm64 and
  Linux x86_64), matching SBCL.
- **Automated test suite (FiveAM).** New `graph-db/test`, `graph-db/concurrency-test`,
  `graph-db/acid-test`, `graph-db/stress-test`, `graph-db/concurrent-stress-test`,
  and a `graph-db/perf-test` performance-benchmark system. Replaces the previous
  ad-hoc REPL-driven tests.
- **ACID-compliance audit** with dedicated regression tests (atomicity,
  consistency, isolation, durability) and broad concurrency coverage across
  SBCL/CCL/ECL.
- `migrate-graph` for upgrading a pre-MVCC (v1) on-disk graph to the v2 format via
  a logical snapshot + replay.
- This `CHANGELOG.md`.

### Changed
- **On-disk storage format bumped to v2.** The node head grows from 15 to 31 bytes
  (append-only: `commit-epoch` + `prev-pointer`). v1 graphs cannot be opened
  directly by v2 code — use `migrate-graph` (logical snapshot + replay). New
  graphs are stamped v2 automatically.
- Stable-address memory mapping: `extend-mapped-file` remaps in place
  (`MAP_FIXED`) so the base pointer never moves, enabling lock-free reads across
  SBCL/CCL/ECL.
- Project version 1.0 → 2.0.

### Fixed
- **Persistent-slot `slot-boundp` / `slot-makunbound` (issue #41).** These MOP
  generic functions were never specialized for the node metaclass, so they
  inspected the always-unbound backing CLOS slot; `slot-boundp` on a persistent
  slot was always NIL and `slot-makunbound` was a no-op on the value. Both now
  consult the data alist.
- **ECL concurrency regression from the #41 fix.** ECL's `change-class` invokes
  `slot-makunbound-using-class` on alist-backed persistent slots during node
  construction, which cleared a freshly-created node's data and triggered racy
  lazy re-materialization of the shared cached node (transient NIL slot reads). A
  dynamic guard (`*initializing-node*` / `change-node-class`) suppresses the
  destructive alist edit during (re)initialization; explicit `slot-makunbound` is
  unaffected.
- Numerous concurrency-correctness fixes surfaced by the new suites: rw-lock
  wakeup herd / FIFO behaviour, skip-list torn-read SIGSEGV on ECL (now
  serialized), insert lost-update ordering, schema class-lock rebuild on
  `open-graph` (issue #32), and replication socket/threading fixes.

### Known issues
- **ECL only**, under the high thread-count parallelism of many-core Linux hosts:
  `CONCURRENT-SPATIAL-INSERT-AND-QUERY` can deadlock (issue #42) and
  `FULL-SYSTEM-STORM` can flakily time out (issue #43). Both are timeouts, not data
  corruption; SBCL and CCL are unaffected, and ECL is green at lower parallelism.

### Compatibility
- ECL **26.5.5** is required (earlier releases such as 21.2.1 are no longer
  supported).
- CCL is supported on Linux x86_64 only; there is no usable CCL on Apple-Silicon
  macOS.
- LispWorks support is currently **untested** (no license access; the free
  Personal Edition's heap is too small to compile VivaceGraph).
