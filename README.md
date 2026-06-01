VivaceGraph
===============

VivaceGraph is an open source graph database written in pure Common Lisp.

VG takes design inspiration from CouchDB, neo4j and AllegroGraph.  It implements an ACID-compliant object graph model with user-defined indexes and map-reduce views.  It also implements a master / slave replication scheme for redundancy and horizontal read scaling.  Querying the graph is accomplished via a number of Lisp methods or via a Prolog-like query language.

VivaceGraph runs on SBCL (>= 1.045), ECL (>= 21.2.1), Clozure CL (CCL), and LispWorks. The full test suite passes on SBCL (macOS arm64 and Linux x86_64) and on ECL (macOS arm64 with ECL 26.5.5 and Linux with ECL 21.2.1).

A note on CCL: it works on Linux x86_64, but **not on Apple-Silicon macOS** — the Clozure ARM64 port has been stalled for several years and macOS support for Intel (x86_64) binaries is nearly gone, so there is no usable CCL on M-series Macs. On Apple Silicon, use SBCL or ECL.

To get started, please see example.lisp.

### Documentation

A comprehensive developer's manual lives in [`docs/vivace-graph-v3-doc.org`](docs/vivace-graph-v3-doc.org), covering getting started, the storage engine and object model, transactions, the Prolog query language, views, the REST API, replication, and backup/recovery, plus an API reference.

This manual was written by [Gwang-Jin Kim (@gwangjinkim)](https://github.com/gwangjinkim) — the project's first thorough documentation, and a great piece of work. Many thanks to him. It has been adopted here and is maintained alongside the code.

### Announcement, 2024-10-15

The author is still volunteering in Ukraine, and is looking for help maintaining this codebase while he is away (and after he returns).  There are currently 11 outstanding issues and very little documentation.  Please let kevin@chatsubolabs.com know if you are interested in helping out.

### Announcement, 2016-12-12

Folks, I recently checked in a few breaking changes to the VG3 repo that
you should be aware of.  In particular, the hashing scheme used for
vertices and edges was not distributing keys very well and graphs would
slow down terribly as they grew very large (1,000,000+ nodes).  I have
updated the UUID generation code as well as the hash functions for
vertices, edges, ve indices and vev indices.  My performance tests show
an improvement in loading a 10,000,000 node snapshot from 2.5 days to
about 2 hours.  Hash key distribution is largely responsible for this
change, but so is an optimization made to the hashing function.  As of
commit 58f87d60e767d868cf30b8e6f1ec0bfc9d6d0b1e , existing graphs will
not work.  I suggest that you take a snapshot of your graph(s) and
reload them using the REPLAY function.  Please let me know if you have
any questions or concerns.
