# Plan: fix the mmap remap-vs-reader race

## Problem

When the allocator grows a memory-mapped file, `extend-mapped-file`
(`mmap.lisp`) remaps it: on Linux via `mremap MREMAP_MAYMOVE` (the mapping can
move, invalidating the old address), on Darwin via `munmap` + `mmap` (the
`mapped-file` pointer goes transiently `NIL`). `munmap-file` likewise nils the
pointer. None of this is synchronized against `get-byte`/`set-byte`, which read
and write through `m-pointer` with no lock. So a reader thread that dereferences
the pointer during a concurrent remap takes a **SIGSEGV**.

The only guard today is a per-implementation SEGV-retry in the `:around`
methods on `get-byte`/`set-byte`/`get-bytes`/`set-bytes`: catch the fault,
retry the access, and hope the remap has finished. There is a **commented-out
`remap-lock` slot** in the `mapped-file` struct (`mmap.lisp:14`) — the proper
fix, abandoned in favour of the cheaper retry.

### Evidence (2026-06-03, odm.chatsubo.net, 64 threads, all storm caps raised)

| Lisp | Result | Notes |
| --- | --- | --- |
| SBCL 2.1.11 | 9/9, 0 SEGVs | tolerates the race via retry (one separate intermittent 3197/3200 lost-insert) |
| CCL 1.13 | 1/11, 9 SEGV storms | reads escape as `SIMPLE-ERROR`/`SIMPLE-FILE-ERROR`; fd limit 32768 (not the cause) |
| ECL 26.5.5 | bogus exit 0 | worker threads SEGV'd into the per-thread debugger; no real FiveAM summary |

The `:POINTER NIL` we logged is both the Darwin window and a teardown effect
(a thread stuck in the retry loop never finishes → `run-threads` 60s timeout →
graph closed → `munmap` nils the pointer → the stuck thread faults on NIL).

## Phase 0 — Reproduce & instrument

1. A focused regression test under `tests/concurrent-stress/` that forces
   repeated heap/lhash growth while many threads read — fails on CCL/ECL today.
2. A retry counter (`*mmap-segv-retries*`) bumped in each `:around` handler, so
   "no faults after the fix" becomes an assertable invariant.

## Phase 1 — Correctness via `remap-lock` (rw-lock)

Self-contained at the mmap layer; the `mapped-file` owns its lock because one
mapping is shared by heap / lhash / type-index, each with different owner locks.

1. Uncomment `remap-lock` (default `(make-rw-lock)`); accessor `m-remap-lock`.
   `make-rw-lock` is available — `rw-lock`/`utilities` load before `mmap` on
   every impl.
2. Split locked-outer / raw-inner: raw `%get-byte`/`%set-byte` (current bodies,
   no lock); public `get-byte`/`set-byte` wrap the raw call in
   `with-read-lock`. Bulk ops (`get-bytes`/`set-bytes`/`serialize-uint64`/
   `deserialize-uint64`) take the read lock **once** and loop over the raw
   inner accessor. **Never** nest a locked accessor inside another (the rw-lock
   read path deadlocks against a queued writer if a thread re-acquires read).
3. `extend-mapped-file` (both platforms) and `munmap-file` wrap their pointer
   mutation in `with-write-lock`. Write-lock is reentrant, so the Darwin
   `extend → munmap-file` nesting is fine.
4. Keep the SEGV-retry `:around` as a backstop; assert it never fires post-fix.

### Lock ordering (safety argument)

`allocate` holds `memory-lock(write)` then, via `grow-memory → extend`, takes
`remap-lock(write)` — order is always `memory-lock → remap-lock`. Byte
accessors take **only** `remap-lock(read)` and hold nothing else across the
dereference, calling nothing that re-takes `memory-lock` or `remap-lock(write)`.
No inversion, no cycle.

### Known cost

`acquire-read-lock` itself grabs a mutex (`with-recursive-lock-held` on the
lock's internal mutex), so a per-file remap read-lock **serializes all readers
of that file**. Correct, but it caps read scalability. Acceptable for
correctness; quantified in Phase 2.

## Phase 2 — Benchmark

Use the planned Stage 5 perf suite to compare read throughput (single- and
multi-threaded) before/after. If the read-lock cost is acceptable, stop here.

## Phase 3 (conditional) — Stable address, lock-free reads

LMDB-style: reserve a large virtual range once (`mmap(NULL, MAX, PROT_NONE)`),
map the file in with `MAP_FIXED`, and grow with in-place `mremap` (no
`MAYMOVE`). `m-pointer` never changes, so readers need no lock at all; only
`memory-size` grows (monotonic). Handle reservation exhaustion (re-reserve +
relocate under the write lock — rare). One osicat code path covers all impls.

## Out of scope (track separately)

- Teardown-during-stuck-reader (`:POINTER NIL`) — mostly covered by the
  remap-lock plus join-before-close; see buffer-pool shutdown work.
- Intermittent SBCL 3197/3200 lost-insert — recheck after the fix.

## Sequencing

Phase 0 → Phase 1 → full matrix at 64 on SBCL/CCL/ECL → Phase 2 → maybe Phase 3.
