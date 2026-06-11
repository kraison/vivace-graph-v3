# Two-process replication test

Master/slave replication is verified with **two separate OS processes** talking
over a loopback socket. It deliberately is **not** a FiveAM suite: running a
master and a slave in one Lisp image makes them share the process-global
`*buffer-pool*`, the `*graphs*` registry and other global state, which produces
false results (e.g. apparent double-applies). Two processes are the only
faithful setup.

## Run

```sh
tests/replication/run-replication-test.sh
```

Prints each check and `RESULT: PASS` / `RESULT: FAIL`; exits 0 on success.

By default it uses SBCL. To run under another implementation, override how a
Lisp file is loaded/run:

```sh
REPL_LISP_CMD="ecl --load" tests/replication/run-replication-test.sh
REPL_LISP_CMD="ccl --load" tests/replication/run-replication-test.sh
```

`REPL_PORT` overrides the (random high) listener port.

## What it covers

- **Catch-up**: the master commits 2 vertices + 1 edge *before* the slave
  connects; the slave replays them from the log, with exactly 2 vertices and 1
  edge (guards against double-apply).
- **Live**: after the slave connects, the master commits an update and a delete;
  the slave converges to exactly 1 live vertex named `Alice2` with the edge gone
  (deleting an endpoint removes the incident edge).

## Files

- `schema.lisp` — shared `r-person` / `r-knows` schema (loaded identically by
  both processes so their schema-digests match for the handshake).
- `master.lisp` — master process: commits the scenario, coordinates via flag
  files under a work dir.
- `slave.lisp` — slave process: connects, verifies both phases, exits non-zero
  on any failed check.
- `run-replication-test.sh` — orchestrator (launches both, reports PASS/FAIL).
