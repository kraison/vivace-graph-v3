(in-package :graph-db)

(defvar *cache-enabled* t)

(defconstant +db-version+ 1)

(defvar *graph* nil)
(defconstant +main-table-file+ "main.dat")
(defconstant +meta-file+ "meta.dat")
(defconstant +data-file+ "data.dat")

(defvar *pending-schema-updates* (make-hash-table :test 'equal))
(defconstant +max-node-types+ 65536)

(defconstant +storage-version+     #x01)
(defconstant +fixed-integer-64+    #x01)
(defconstant +data-magic-byte+     #x17)
(defconstant +lhash-magic-byte+    #x18)
(defconstant +overflow-magic-byte+ #x19)
(defconstant +config-magic-byte+   #x20)
(defconstant +null-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 0))
(defconstant +max-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 255))
(defconstant +key-bytes+ 16)
(defconstant +value-bytes+ 8)
(defconstant +bucket-size+ 24)
(defconstant +data-extent-size+ (* 1024 1024 100))

;; Key namespaces
(defvar *vertex-namespace* (uuid:make-uuid-from-string "2140DCE1-3208-4354-8696-5DF3076D1CEB"))
(defvar *edge-namespace* (uuid:make-uuid-from-string "0392C7B5-A38B-466F-92E5-5A7493C2775A"))

;; Sentinel values for skip lists
(defconstant +min-sentinel+ :gmin)
(defconstant +max-sentinel+ :gmax)
;; For views, aggregrate key symbol
(defconstant +reduce-master-key+ :gagg)

;; index-lists
(defconstant +index-list-bytes+ 17)

;; ve-key / ve-index
(defconstant +ve-key-bytes+ 18)
(defconstant +null-ve-key+ (make-array +ve-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8)))
(defconstant +max-ve-key+ (make-array +ve-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8)))

;; vev-key / vev-index
(defconstant +vev-key-bytes+ 34)
(defconstant +null-vev-key+ (make-array +vev-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8)))
(defconstant +max-vev-key+ (make-array +vev-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8)))

;; Type bytes for serialization
(defconstant +needs-lookup+ :needs-lookup)
(defconstant +unknown+ 0)
(defconstant +negative-integer+ 1)
(defconstant +positive-integer+ 2)
(defconstant +character+ 3)
(defconstant +symbol+ 4)
(defconstant +string+ 5)
(defconstant +list+ 6)
(defconstant +vector+ 7)
(defconstant +single-float+ 8)
(defconstant +double-float+ 9)
(defconstant +ratio+ 10)
(defconstant +t+ 11)
(defconstant +null+ 12)
(defconstant +blob+ 13) ;; Uninterpreted octets
(defconstant +dotted-list+ 14)
(defconstant +keyword+ 15)
(defconstant +slot-key+ 16)
(defconstant +id+ 17)
(defconstant +vertex+ 18)
(defconstant +edge+ 19)
(defconstant +skip-list+ 20)
(defconstant +ve-index+ 21)
(defconstant +type-index+ 22)
(defconstant +pcons+ 23)
(defconstant +pqueue+ 24)
(defconstant +mpointer+ 25)
(defconstant +pcell+ 26)
(defconstant +index-list+ 27)
(defconstant +vev-index+ 28)
;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+ 100)
(defconstant +timestamp+ 101)
(defconstant +guid+ 102)

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
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql))
(defparameter *prolog-trace* nil)
(defconstant +unbound+ :unbound)
(defconstant +no-bindings+ '((t . t)))
(defconstant +fail+ nil)

