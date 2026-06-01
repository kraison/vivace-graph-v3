;;;; ECL support for serializing DEFSTRUCT instances with cl-store.
;;;;
;;;; Stock cl-store has no structure-object serialization on ECL: its
;;;; SERIALIZABLE-SLOTS-USING-CLASS method for STRUCTURE-CLASS is gated
;;;; #+(or sbcl cmu openmcl allegro), and its own test suite excludes the
;;;; structure-object tests on ECL (#+(or sbcl cmu lispworks openmcl)).  As a
;;;; result (cl-store:store <any-struct>) signals "Cannot store objects of
;;;; type ... with backend cl-store" on ECL.
;;;;
;;;; graph-db relies on cl-store to persist a handful of struct types (LHASH,
;;;; SKIP-LIST, TYPE-INDEX, ...) for graph reopen, so we add the missing
;;;; pieces here.  We reuse cl-store's STANDARD-OBJECT type code: its restorer
;;;; (RESTORE-TYPE-OBJECT) reconstructs via FIND-CLASS + ALLOCATE-INSTANCE +
;;;; (setf SLOT-VALUE), all of which work on ECL structure objects, so no
;;;; separate restore method is needed.  ECL structs are STRUCTURE-OBJECTs
;;;; (metaclass STRUCTURE-CLASS) and are NOT STANDARD-OBJECTs, hence the stock
;;;; (obj standard-object) store method does not catch them.
;;;;
;;;; #+standard-object-code+ and store-type-object are internal to cl-store;
;;;; we reference them with :: deliberately.  The whole file is a no-op on
;;;; every other implementation.

#+ecl
(in-package :cl-store)

#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Enumerate a structure's slots the same way the supported impls do.
  (defmethod serializable-slots-using-class ((object t) (class structure-class))
    (class-slots class))

  ;; cl-store's SERIALIZABLE-SLOTS has no applicable method for a structure
  ;; object on ECL; route it through SERIALIZABLE-SLOTS-USING-CLASS as on the
  ;; other implementations.
  (defmethod serializable-slots ((object structure-object))
    (serializable-slots-using-class object (class-of object)))

  ;; Store structs under the STANDARD-OBJECT type code; RESTORE-TYPE-OBJECT
  ;; handles the read side for both.
  (defstore-cl-store (obj structure-object stream)
    (output-type-code +standard-object-code+ stream)
    (store-type-object obj stream)))
