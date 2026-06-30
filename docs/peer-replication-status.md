# Peer Replication — Session Handoff / Status

**Purpose:** let a fresh Claude Code session resume the peer-replication work exactly where the
previous one stopped, and use the newly-attached **Lisp MCP** for verification.
**Branch:** `peer-replication` (off `experiment`), pushed to `origin`.
**Last updated:** 2026-06-30.

Read these first, in order: this file → `docs/peer-replication-design.md` (v2, the authoritative
design + §12 pressure-test invariants) → `docs/peer-replication-branch-a-plan.md` (WP breakdown,
peer-protocol-v1 wire layout, milestones). The auto-loaded memory `peer-replication.md` summarizes
the same state.

---

## 1. Where we are

**Milestone M1 (foundation) COMPLETE; M2 (pull MVP) STARTED.** All committed + pushed on
`peer-replication`:

| commit | what |
|---|---|
| `240c733` | docs: design v2 + approved proposal |
| `4c51351` | docs: Branch A implementation plan |
| `b0fb61f` | **WP-1** peer-graph class (sibling of master/slave) + make/open-graph plumbing |
| `60949e9` | **WP-2** device journaling (`journals-own-feed-p`) + **WP-3** applied-op-id index |
| `5bab412` | **WP-0** peer-meta envelope + op-id/lamport minting at authoring |
| `2f4112d` | **WP-5** authority-scoped closed-subgraph export (`replication.lisp`) |

Net: a `peer-graph` constructs/lifecycles; a device journals its own commits into a push feed with
a per-transaction peer-meta envelope (origin-id + op-id + lamport + op-class); a durable op-id index
dedups; and `scope-node-set`/`reconcile-manifest`/`filtered-backup` compute the closed
authority-scoped subgraph a device should hold. Master/slave/plain graphs are untouched throughout.
Every commit was verified with a standalone SBCL smoke test.

**Working tree:** `docs/peer-replication-status.md` (this file) is new/uncommitted. There are also
**pre-existing unrelated working-tree edits** (edge.lisp, index-list.lisp, rest.lisp, skip-list*.lisp,
vertex.lisp, vev-index.lisp, views.lisp) that predate this work — **do not stage or commit them**;
they are not part of peer-replication. Stage only the specific files you change.

---

## 2. How to verify (use the Lisp MCP; standalone SBCL is the fallback)

A Lisp MCP is now attached. Discover it with `ToolSearch` (keywords: `lisp`, `eval`, `swank`,
`slynk`, `sbcl`, `repl`) and load its eval tool. Use it to keep a **warm graph-db image** so you can
build a hub + device and watch a sync over loopback in one process (the big win for WP-4).

Bring the system up in the image once:
```lisp
(ql:quickload :graph-db)   ; the repo is symlinked into ~/quicklisp/local-projects, so this
(in-package :graph-db)     ; picks up your edits; re-quickload after editing a file to recompile
```

**Fallback (no MCP / quick one-shot):** the previous session used `sbcl --non-interactive
--disable-debugger --load <script>` via the Bash tool. Pattern that works:
```
sbcl --non-interactive --disable-debugger --load /path/to/smoke.lisp 2>/dev/null | grep -E '<markers>'
```
with `(load "~/quicklisp/setup.lisp")` + `(ql:quickload :graph-db)` at the top. The smoke scripts
from last session lived in that session's scratchpad (now gone) — recreate them in YOUR scratchpad,
or better, start the FiveAM `graph-db/peer-test` system (see plan §4). Verified facts to rely on:
`uuid:uuid-to-byte-array (uuid:make-v4-uuid)` makes a 16-byte id; `def-vertex NAME (supers) (slots)
GRAPH-NAME` (keyword graph names) generates `make-NAME`; graphs need `*graph*` bound for
transactions; `close-graph g :snapshot-p nil` to skip the snapshot.

Quick live sanity check once the image is up:
```lisp
(peer-graph-p (make-instance 'peer-graph))            ; => T
(journals-own-feed-p (make-instance 'peer-graph))     ; => T   (master T, slave/plain NIL)
;; build a hub with an export-predicate, create the 2-hop chain, then
;; (scope-node-set hub (list site-id) "hma")          ; => closed disclosable subgraph
```

---

## 3. Immediate next task: WP-4 (transport) — the rest of M2

Goal: a hub exports an authority-scoped closed subgraph to a device, which applies it (read-only),
plus an ongoing cursor-resumed pull. This is the shippable read-only field app (app Phase 2).

New file **`peer-streaming.lisp`** in the FULL `graph-db` system (NOT `graph-db/core`) — add it to
`graph-db.asd` right after `transaction-streaming.lisp` (the full-system component list, ~line 115),
since it needs usocket. Model on, but keep DISTINCT from, the master/slave transport in
`transaction-streaming.lisp` (`make-slave-session-handler` :260, `slave-loop` :413,
`server-accept-loop` :295, `stream-transaction-to-disk` :349) and the streaming in
`transaction-log-streaming.lisp`.

Reuse verbatim: `read-packet`/`write-packet`/`read-plist-packet`/`write-plist-packet`, the 8-byte
size framing, and `apply-transaction` for applying. The peer feed entries are
`[peer-meta packet][tx-header packet][write packets…]` — read the peer-meta first with
`deserialize-peer-meta` (transactions.lisp), then the standard tx packets.

Sub-steps (each independently testable in the warm image):
1. **`start-replication`/`stop-replication` methods specialized on `peer-graph`** (they're currently
   inherited no-ops). Hub role → an accept loop on `replication-port`; device role → a connect loop
   to `peer-host`:`replication-port`. (Reusing the existing generics means make/open/close-graph
   already call them — no lifecycle changes needed.)
2. **Handshake** — reuse the plist handshake; device sends `origin-id`, `pull-cursor`, `push-ack`,
   `schema-version`; hub authenticates against its `device-registry`, loads the device's scope +
   `apply-cursor` + manifest. **WP-6**: make the schema check a *compatibility* (same-major) check,
   not digest-equality (design §14 / PT-6).
3. **Pull phase** — manifest reconciliation (`reconcile-manifest`, WP-5, done) emitting
   membership-creates + purges, then the cursor-resumed in-scope authored-op stream. Manifest
   advances **only on device ack** (PT-4, disclosure-critical).
4. **WP-8** — the device receive thread ENQUEUES received ops to a single writer (don't apply inline
   on the socket thread); required for the single-writer funnel on ECL (PT-5).
5. **Push phase** — wire it (device streams authored ops after `apply-cursor`) but the device is
   read-only in Phase 2; the hub-side re-home apply is **WP-7 / M3**.

Then **M3**: WP-7 re-home apply (hub applies a device op as a fresh hub-origin transaction,
preserving op-id/origin/lamport, re-journaling; `apply-peer-op` generic, simple last-applied-wins —
merge is Branch B).

---

## 4. Hard invariants (from §12 pressure-tests — do not regress)

- **PT-3:** the applied-op-id index update must be atomic with the apply (same committed txn).
- **PT-4:** the per-device manifest advances ONLY on device ack — a dropped purge must re-emit
  (fail-closed disclosure). Over-emitting a purge is safe (idempotent); under-emitting is a breach.
- **PT-5:** device apply must run on the single writer, not the socket thread.
- **PT-6:** schema gate is compatibility (same-major), not equality; push must accept an
  older-but-compatible device so field safety data is never stranded.
- **§3 four identities** kept distinct: node-UUID / op-id (dedup) / per-origin feed-seq (cursor) /
  lamport (order).
- **§4 two op classes:** authored (op-id-deduped, bidirectional) vs state-sync (node-UUID-keyed,
  hub→device only, never deduped/merged/propagated — purges + membership-creates).
- **§7 closed-subgraph, fail-closed:** vertex iff disclosable, edge iff BOTH endpoints disclosable;
  `disclosable-p` must be downward-closed along disclosure-determining edges.
- Re-homing: only the hub re-journals; a device never re-journals ops it pulled (prevents loops).

---

## 5. Open items / app-side asks (do NOT block WP-4)

Engine TODO (deferred): Lamport counter persistence (PT-8, Branch B); applied-op-id index pruning;
manifest reconciliation cost model; origin-id persistence across reopen (currently passed in).
App-side (parallel): `disclosable-p` signature + multi-tasking/retask fail-closed rule (PT-7.3);
`find-of-type` modeling fork (lean: vertex reference slot, PT-7.1); edge-removal bucket; IMAS
`work-stage` value set + danger rank; device-registry schema; `schema-version` major/minor scheme.

---

## 6. Conventions (carry these)

- Engine work on branch `peer-replication`. **Push is explicit-only.**
- **Before every `git commit`**: show the COMPLETE staged diff in the assistant message in a fenced
  ```diff block under a `## 📋 DIFF FOR REVIEW` header (Kevin's standing rule). Stage only files you
  changed; never the pre-existing unrelated working-tree edits (§1).
- **Lisp = spaces only, never tabs** (tab-width 8 if converting). Match surrounding style.
- Commit messages end with `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- Use your own session scratchpad for temp/test files, not the old session's paths.

---

## 7. Ready-to-paste continuation prompt for the new session

> Resume the VivaceGraph peer-replication work on branch `peer-replication`. Read
> `docs/peer-replication-status.md`, then `docs/peer-replication-design.md` (v2) and
> `docs/peer-replication-branch-a-plan.md`. M1 (WP-0/1/2/3) and WP-5 are done, committed, and
> pushed. Next is **WP-4** (the `peer-streaming.lisp` transport: peer-graph start/stop-replication,
> handshake with schema-compat = WP-6, pull phase via `reconcile-manifest` + cursor-resumed op
> stream, device apply enqueued to a single writer = WP-8). Use the attached **Lisp MCP** to keep a
> warm `graph-db` image and test a hub+device sync over loopback (fall back to standalone
> `sbcl --load` if needed). Honor the diff-before-commit rule and don't touch the pre-existing
> unrelated working-tree edits.
