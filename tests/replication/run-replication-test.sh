#!/usr/bin/env bash
#
# Two-process master/slave replication test for VivaceGraph.
#
# Replication CANNOT be tested with master and slave in one Lisp image: they
# would share the process-global *buffer-pool*, the *graphs* registry and other
# global state, which produces false failures (e.g. apparent double-applies).
# This harness runs the master and the slave as TWO SEPARATE OS processes
# talking over a loopback socket -- the only faithful setup.
#
# Usage:
#   tests/replication/run-replication-test.sh
#
# Environment overrides:
#   REPL_LISP_CMD   how to load+run a Lisp file (default: "sbcl --non-interactive --load")
#                   e.g. for ECL:  REPL_LISP_CMD="ecl --load"
#                        for CCL:  REPL_LISP_CMD="ccl --load"
#   REPL_PORT       TCP port for the master listener (default: random high port)
#
# Exits 0 if the slave converges to the master's state on every check, else 1.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
LISP_CMD="${REPL_LISP_CMD:-sbcl --non-interactive --load}"
PORT="${REPL_PORT:-$(( 17000 + RANDOM % 2000 ))}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/vg-repl-XXXXXX")"

export REPL_WORK="$WORK"
export REPL_MASTER_DIR="$WORK/master/"
export REPL_SLAVE_DIR="$WORK/slave/"
export REPL_PORT="$PORT"
mkdir -p "$REPL_MASTER_DIR" "$REPL_SLAVE_DIR"

echo "Replication test: lisp='$LISP_CMD' port=$PORT work=$WORK"

# Master runs in the background; it commits TX1, waits for the slave to connect,
# then streams TX2/TX3 live, and finally exits when the slave signals done.
$LISP_CMD "$HERE/master.lisp" > "$WORK/master.out" 2>&1 &
MPID=$!

# Slave runs in the foreground; it waits for the master's "ready" flag itself.
$LISP_CMD "$HERE/slave.lisp" > "$WORK/slave.out" 2>&1
SLAVE_CODE=$?

wait "$MPID" 2>/dev/null
MASTER_CODE=$?
kill "$MPID" 2>/dev/null   # belt and suspenders

echo "--- master output ---"
grep -E 'MASTER' "$WORK/master.out" 2>/dev/null || cat "$WORK/master.out"
echo "--- slave output ---"
grep -E '  ok|  FAIL|SLAVE|ERROR' "$WORK/slave.out" 2>/dev/null || cat "$WORK/slave.out"

if [ "$SLAVE_CODE" -eq 0 ]; then
  echo "RESULT: PASS"
  rm -rf "$WORK"
  exit 0
else
  echo "RESULT: FAIL (slave exit $SLAVE_CODE, master exit $MASTER_CODE)"
  echo "logs kept in: $WORK"
  exit 1
fi
