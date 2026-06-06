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

;; A geometry-bearing vertex for the replicated spatial-index / subset checks.
;; The NODE-GEOMETRY method must be defined on BOTH processes (here) so the
;; slave maintains its spatial index as it applies replicated writes.
(def-vertex r-place ()
  ((label    :type string)
   (location :type geometry))
  :repl-test-app)

(defmethod node-geometry ((p r-place))
  (slot-value p 'location))
