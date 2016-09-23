VivaceGraph
===============

VivaceGraph is an open source graph database written in pure Common Lisp.

VG takes design inspiration from CouchDB, neo4j and AllegroGraph.  It implements an ACID-compliant object graph model with user-defined indexes and map-reduce views.  It also implements a master / slave replication scheme for redundancy and horizontal read scaling.  Querying the graph is accomplished via a number of Lisp methods or via a Prolog-like query language.

It currently only works with SBCL versions >= 1.045, though it would not take much work to port it to other Common Lisp implementations.

To get started, please see example.lisp.
