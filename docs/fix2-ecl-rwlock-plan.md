# Plan — Fix #2: modernize the ECL rw-lock path

Part of the CCL/ECL concurrency-convoy work (see
`docs/concurrency-scaling-investigation.md`). Branch: `experiment`.

## Goal
Replace the `#+ecl (sleep 0.001)` busy-poll in `rw-lock.lisp` with real
condition-variable blocking (the path SBCL/LispWorks already use), **gated on
ECL ≥ 23.9.9** so ECL 21.2.1 keeps the safe poll fallback. This removes the
~1 ms-per-handoff floor that pins ECL@64 at ~100 % CPU (1 core of 32), the worst
part of the ECL convoy. It does NOT fix the read-read serialization or the
broadcast thundering-herd — that is fix #3.

## Step 1 — Version feature gate (this step)
In `globals.lisp` (loads before `rw-lock.lisp` per the ASD), wrapped in
`(eval-when (:compile-toplevel :load-toplevel :execute) …)` so the feature is set
before `rw-lock.lisp` is *read/compiled*: push `:graph-db-ecl-modern-mp` onto
`*features*` iff the ECL version is ≥ 23.9.9. Default-to-safe: if the version
can't be parsed or is older, the feature is absent → poll fallback. (ECL 21.2.1
is no longer installed anywhere we can test, so the safe default must carry it.)

## Step 2 — Convert the four rw-lock functions to a 3-way conditional
Each becomes **blocking** for `#+(or sbcl lispworks graph-db-ecl-modern-mp)`
(per-impl primitive) vs **poll** for `#+(and ecl (not graph-db-ecl-modern-mp))`.
ECL blocking calls use the struct's existing `mp:make-condition-variable`
`waitqueue` and `mp:make-semaphore` `semaphore`:

| function | SBCL | modern-ECL | old-ECL fallback |
|---|---|---|---|
| acquire-read-lock (writer present) | sb-thread:condition-wait | mp:condition-variable-wait | (sleep 0.001) |
| release-read-lock (wake writer) | sb-thread:signal-semaphore | mp:signal-semaphore | nothing (poll) |
| acquire-write-lock (drain readers) | wait-on-semaphore + condition-wait | mp:wait-on-semaphore + mp:condition-variable-wait | poll lock-readers + sleep |
| release-write-lock (wake waiters) | sb-thread:condition-broadcast | mp:condition-variable-broadcast | nothing |

`acquire-write-lock` is the intricate one (semaphore reader-drain + queue
condition-wait); modern-ECL mirrors the SBCL structure exactly.

## Risks
1. **Recursive-lock × condition-variable-wait.** The wait runs inside
   `bt:with-recursive-lock-held` on the struct's non-recursive `mp:make-lock`.
   SBCL does the identical thing and works, so the pattern is sound, but ECL's
   `mp:condition-variable-wait` must atomically release/reacquire that lock —
   **prove on ECL 26.5.5** (failure = deadlock/timeout, not corruption).
2. Wrong version gate → reintroduces 21.2.1 hangs. Mitigated by default-to-safe
   + conservative `≥ 23.9.9` + verifying the feature flips on on 26.5.5.
3. Use raw `mp:` consistently for the ECL branch (matching the struct), not `bt:`.
4. Broadcast thundering-herd remains — deferred to fix #3.

## Validation (gate before committing)
1. Confirm `:graph-db-ecl-modern-mp` ∈ `*features*` on ECL 26.5.5 (and absent /
   compiles fine on SBCL + CCL).
2. Full `acid` + `concurrency` + `concurrent-stress` on SBCL, CCL, ECL 26.5.5 at
   shipped config (`*stress-thread-count*` 16). ECL is the real test (no new
   hangs; skip-list/rw-lock concurrency tests pass).
3. Re-measure ECL@64 with CPU sampling: expect median CPU well above the ~100 %
   floor and pass rate up vs baseline (ECL 9/15). Update the doc's ceiling.

## Sequencing
On `experiment`: Step 1 → Step 2 → validate → one commit with gate results +
ECL@64 numbers. Fix #3 (read-read fast-path + targeted wakeup) follows.
