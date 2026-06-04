;;;; Shared schema for the two-process replication test.
;;;;
;;;; Loaded IDENTICALLY by both the master and slave processes so their
;;;; schema-digests match (the master<->slave handshake requires it).  Keep the
;;;; def-vertex / def-edge forms byte-for-byte identical on both sides.

(in-package :graph-db)

(def-vertex r-person ()
  ((name :type string)
   (age  :type integer))
  :repl-test-app)

(def-edge r-knows ()
  ((since))
  :repl-test-app)
