(in-package :graph-db)

(defvar *cache-enabled* t)

(alexandria:define-constant +db-version+ 1)

(defvar *graph* nil)
(alexandria:define-constant +main-table-file+ "main.dat" :test 'equal)
(alexandria:define-constant +meta-file+ "meta.dat" :test 'equal)
(alexandria:define-constant +data-file+ "data.dat" :test 'equal)

(defvar *schema-node-metadata* (make-hash-table :test 'equal))
(alexandria:define-constant +max-node-types+ 65536)

(alexandria:define-constant +storage-version+     #x01)
(alexandria:define-constant +fixed-integer-64+    #x01)
(alexandria:define-constant +data-magic-byte+     #x17)
(alexandria:define-constant +lhash-magic-byte+    #x18)
(alexandria:define-constant +overflow-magic-byte+ #x19)
(alexandria:define-constant +config-magic-byte+   #x20)
(alexandria:define-constant +null-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 0)
   :test 'equalp)
(alexandria:define-constant +max-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 255)
   :test 'equalp)
(alexandria:define-constant +key-bytes+ 16)
(alexandria:define-constant +value-bytes+ 8)
(alexandria:define-constant +bucket-size+ 24)
(alexandria:define-constant +data-extent-size+ (* 1024 1024 100))

;; Key namespaces
(defvar *vertex-namespace* (uuid:make-uuid-from-string "2140DCE1-3208-4354-8696-5DF3076D1CEB"))
(defvar *edge-namespace* (uuid:make-uuid-from-string "0392C7B5-A38B-466F-92E5-5A7493C2775A"))

;; Sentinel values for skip lists
(alexandria:define-constant +min-sentinel+ :gmin)
(alexandria:define-constant +max-sentinel+ :gmax)
;; For views, aggregrate key symbol
(alexandria:define-constant +reduce-master-key+ :gagg)

;; index-lists
(alexandria:define-constant +index-list-bytes+ 17)

;; ve-key / ve-index
(alexandria:define-constant +ve-key-bytes+ 18)
(alexandria:define-constant +null-ve-key+
    (make-array +ve-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp)
(alexandria:define-constant +max-ve-key+
    (make-array +ve-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
  :test 'equalp)

;; vev-key / vev-index
(alexandria:define-constant +vev-key-bytes+ 34)
(alexandria:define-constant +null-vev-key+
    (make-array +vev-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp)
(alexandria:define-constant +max-vev-key+
    (make-array +vev-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
   :test 'equalp)

;; Type bytes for serialization
(alexandria:define-constant +needs-lookup+ :needs-lookup)
(alexandria:define-constant +unknown+ 0)
(alexandria:define-constant +negative-integer+ 1)
(alexandria:define-constant +positive-integer+ 2)
(alexandria:define-constant +character+ 3)
(alexandria:define-constant +symbol+ 4)
(alexandria:define-constant +string+ 5)
(alexandria:define-constant +list+ 6)
(alexandria:define-constant +vector+ 7)
(alexandria:define-constant +single-float+ 8)
(alexandria:define-constant +double-float+ 9)
(alexandria:define-constant +ratio+ 10)
(alexandria:define-constant +t+ 11)
(alexandria:define-constant +null+ 12)
(alexandria:define-constant +blob+ 13) ;; Uninterpreted octets
(alexandria:define-constant +dotted-list+ 14)
(alexandria:define-constant +keyword+ 15)
(alexandria:define-constant +slot-key+ 16)
(alexandria:define-constant +id+ 17)
(alexandria:define-constant +vertex+ 18)
(alexandria:define-constant +edge+ 19)
(alexandria:define-constant +skip-list+ 20)
(alexandria:define-constant +ve-index+ 21)
(alexandria:define-constant +type-index+ 22)
(alexandria:define-constant +pcons+ 23)
(alexandria:define-constant +pqueue+ 24)
(alexandria:define-constant +mpointer+ 25)
(alexandria:define-constant +pcell+ 26)
(alexandria:define-constant +index-list+ 27)
(alexandria:define-constant +vev-index+ 28)
(alexandria:define-constant +bit-vector+ 29)
(alexandria:define-constant +bignum+ 30)
;; User-defined type identifiers for serializing. Start at 100
(alexandria:define-constant +uuid+ 100)
(alexandria:define-constant +timestamp+ 101)
(alexandria:define-constant +guid+ 102)

(defparameter *initial-extents* 10)
(defparameter *max-locks* 10000)

(defvar *graph-hash* nil)

;; Prolog specials
(defparameter *occurs-check* t)
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0 "Counter for generating variable names.")
(defvar *functor* nil "The Prolog functor currently being compiled.")
(defvar *select-list* nil "Accumulator for prolog selects.")
(defvar *cont* nil "Continuation container for step-wise queries.")

#+sbcl
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
#+sbcl
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql))

#+ccl
(defvar *prolog-global-functors* (make-hash-table :shared t))
#+ccl
(defvar *user-functors* (make-hash-table :shared t :test 'eql))

(defparameter *prolog-trace* nil)
(alexandria:define-constant +unbound+ :unbound)
(alexandria:define-constant +no-bindings+ '((t . t)) :test 'equalp)
(alexandria:define-constant +fail+ nil)
