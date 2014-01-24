vivace-graph-v3
===============

VivaceGraph version 3

This is another redesign of VivaceGraph;  this version uses memory mapped files
and has dropped the RDF semantics of previous versions.  VG is now more akin to 
neo4j, as it implements a property graph model.  The Prolog interface has been 
greatly expanded, though it still suffers from a few minor issues (all of which 
are more issues of interface convenience as opposed to functional deficiencies).
CLOS integration was a goal with this version and works quite well at this 
stage.  Multi-node transactions are next on the agenda.

Documentation is forthcoming;  please stay tuned.
