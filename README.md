VivaceGraph
===============

VivaceGraph is an open source graph database written in pure Common Lisp.

VG takes design inspiration from CouchDB, neo4j and AllegroGraph.  It implements an ACID-compliant object graph model with user-defined indexes and map-reduce views.  It also implements a master / slave replication scheme for redundancy and horizontal read scaling.  Querying the graph is accomplished via a number of Lisp methods or via a Prolog-like query language.

It currently only works with SBCL versions >= 1.045 and Clozure CL, though it would not take much work to port it to other Common Lisp implementations. A port to ECL has been started and can be found in the ecl-port branch.

To get started, please see example.lisp.

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
