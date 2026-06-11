#!/usr/bin/env python3
"""Independent spatial-predicate oracle for the graph-db/geos test suite.

Reads a file of "WKT_A | WKT_B" lines and prints, per line, three booleans:

    <A intersects B> <A contains B> <A is_valid>

as "1"/"0" separated by spaces.  shapely uses its own bundled GEOS, so agreement
with VivaceGraph's libgeos binding cross-checks our WKT bridge (axis order, ring
closure, precision) and predicate wiring against a separate implementation.

Usage:  python3 oracle.py INPUT_FILE
"""
import sys
from shapely import wkt


def main():
    if len(sys.argv) != 2:
        sys.stderr.write("usage: oracle.py INPUT_FILE\n")
        return 2
    with open(sys.argv[1]) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            a_wkt, b_wkt = line.split("|")
            a = wkt.loads(a_wkt.strip())
            b = wkt.loads(b_wkt.strip())
            inter = 1 if a.intersects(b) else 0
            cont = 1 if a.contains(b) else 0
            valid = 1 if a.is_valid else 0
            print(f"{inter} {cont} {valid}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
