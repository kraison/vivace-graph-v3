;;;; -*- Mode: Lisp; Package: :vg-tests -*-
;;;; tests/layer1/test-globals.lisp
;;;;
;;;; Comprehensive test suite for src/globals.lisp
;;;; Tests constants, variables, sentinel values, and type codes

(in-package :vg-tests)

(def-suite l1.globals
  :description "Layer 1: Global constants and variables tests"
  :in nil)

(in-suite l1.globals)

;;;; ============================================================================
;;;; CACHE CONTROL TESTS
;;;; ============================================================================

(test test-l1.globals.cache-enabled-defined
  "Verify *cache-enabled* is defined."
  (is (boundp 'graph-db:*cache-enabled*)))

(test test-l1.globals.cache-enabled-default-true
  "Verify *cache-enabled* defaults to T."
  (is (eq graph-db:*cache-enabled* t)))

;;;; ============================================================================
;;;; DATABASE VERSION AND PERSISTENCE TESTS
;;;; ============================================================================

(test test-l1.globals.db-version-defined
  "Verify +db-version+ is defined."
  (is (boundp 'graph-db:+db-version+)))

(test test-l1.globals.db-version-value
  "Verify +db-version+ is 1."
  (is (= graph-db:+db-version+ 1)))

(test test-l1.globals.graph-variable-defined
  "Verify *graph* is defined."
  (is (boundp 'graph-db:*graph*)))

(test test-l1.globals.graph-default-nil
  "Verify *graph* defaults to NIL."
  (is (null graph-db:*graph*)))

(test test-l1.globals.main-table-file-defined
  "Verify +main-table-file+ is defined."
  (is (boundp 'graph-db:+main-table-file+)))

(test test-l1.globals.main-table-file-value
  "Verify +main-table-file+ is \"main.dat\"."
  (is (equal graph-db:+main-table-file+ "main.dat")))

(test test-l1.globals.meta-file-defined
  "Verify +meta-file+ is defined."
  (is (boundp 'graph-db:+meta-file+)))

(test test-l1.globals.meta-file-value
  "Verify +meta-file+ is \"meta.dat\"."
  (is (equal graph-db:+meta-file+ "meta.dat")))

(test test-l1.globals.data-file-defined
  "Verify +data-file+ is defined."
  (is (boundp 'graph-db:+data-file+)))

(test test-l1.globals.data-file-value
  "Verify +data-file+ is \"data.dat\"."
  (is (equal graph-db:+data-file+ "data.dat")))

;;;; ============================================================================
;;;; SCHEMA METADATA TESTS
;;;; ============================================================================

(test test-l1.globals.schema-node-metadata-defined
  "Verify *schema-node-metadata* is defined."
  (is (boundp 'graph-db:*schema-node-metadata*)))

(test test-l1.globals.schema-node-metadata-hash-table
  "Verify *schema-node-metadata* is a hash table."
  (is (hash-table-p graph-db:*schema-node-metadata*)))

(test test-l1.globals.max-node-types-defined
  "Verify +max-node-types+ is defined."
  (is (boundp 'graph-db:+max-node-types+)))

(test test-l1.globals.max-node-types-value
  "Verify +max-node-types+ is 65536 (2^16)."
  (is (= graph-db:+max-node-types+ 65536)))

(test test-l1.globals.max-node-types-is-power-of-two
  "Verify +max-node-types+ is a power of 2."
  (is (= (logcount graph-db:+max-node-types+) 1)))

;;;; ============================================================================
;;;; STORAGE FORMAT TESTS
;;;; ============================================================================

(test test-l1.globals.storage-version-defined
  "Verify +storage-version+ is defined."
  (is (boundp 'graph-db:+storage-version+)))

(test test-l1.globals.storage-version-value
  "Verify +storage-version+ is #x01."
  (is (= graph-db:+storage-version+ #x01)))

(test test-l1.globals.fixed-integer-64-defined
  "Verify +fixed-integer-64+ is defined."
  (is (boundp 'graph-db:+fixed-integer-64+)))

(test test-l1.globals.magic-bytes-defined
  "Verify all magic bytes are defined."
  (is (boundp 'graph-db:+data-magic-byte+))
  (is (boundp 'graph-db:+lhash-magic-byte+))
  (is (boundp 'graph-db:+overflow-magic-byte+))
  (is (boundp 'graph-db:+config-magic-byte+)))

(test test-l1.globals.magic-bytes-distinct
  "Verify magic bytes are all different."
  (let ((values (list graph-db:+data-magic-byte+
                      graph-db:+lhash-magic-byte+
                      graph-db:+overflow-magic-byte+
                      graph-db:+config-magic-byte+)))
    (is (= (length values) (length (remove-duplicates values))))))

(test test-l1.globals.null-key-defined
  "Verify +null-key+ is defined."
  (is (boundp 'graph-db:+null-key+)))

(test test-l1.globals.null-key-is-16-byte-array
  "Verify +null-key+ is 16-byte array."
  (is (vectorp graph-db:+null-key+))
  (is (= (length graph-db:+null-key+) 16)))

(test test-l1.globals.null-key-all-zeros
  "Verify +null-key+ contains all zeros."
  (is (every (lambda (b) (zerop b)) graph-db:+null-key+)))

(test test-l1.globals.max-key-defined
  "Verify +max-key+ is defined."
  (is (boundp 'graph-db:+max-key+)))

(test test-l1.globals.max-key-is-16-byte-array
  "Verify +max-key+ is 16-byte array."
  (is (vectorp graph-db:+max-key+))
  (is (= (length graph-db:+max-key+) 16)))

(test test-l1.globals.max-key-all-255
  "Verify +max-key+ contains all 0xFF."
  (is (every (lambda (b) (= b 255)) graph-db:+max-key+)))

(test test-l1.globals.null-key-less-than-max-key
  "Verify +null-key+ < +max-key+ lexicographically."
  (is (not (equalp graph-db:+null-key+ graph-db:+max-key+)))
  (is (every (lambda (a b) (< a b)) graph-db:+null-key+ graph-db:+max-key+)))

(test test-l1.globals.key-bytes-defined
  "Verify +key-bytes+ is defined."
  (is (boundp 'graph-db:+key-bytes+)))

(test test-l1.globals.key-bytes-value
  "Verify +key-bytes+ is 16."
  (is (= graph-db:+key-bytes+ 16)))

(test test-l1.globals.value-bytes-defined
  "Verify +value-bytes+ is defined."
  (is (boundp 'graph-db:+value-bytes+)))

(test test-l1.globals.value-bytes-value
  "Verify +value-bytes+ is 8."
  (is (= graph-db:+value-bytes+ 8)))

(test test-l1.globals.bucket-size-defined
  "Verify +bucket-size+ is defined."
  (is (boundp 'graph-db:+bucket-size+)))

(test test-l1.globals.bucket-size-value
  "Verify +bucket-size+ equals +key-bytes+ + +value-bytes+."
  (is (= graph-db:+bucket-size+ 
         (+ graph-db:+key-bytes+ graph-db:+value-bytes+))))

(test test-l1.globals.data-extent-size-defined
  "Verify +data-extent-size+ is defined."
  (is (boundp 'graph-db:+data-extent-size+)))

(test test-l1.globals.data-extent-size-value
  "Verify +data-extent-size+ is 100 MB."
  (is (= graph-db:+data-extent-size+ (* 1024 1024 100)))
  (is (= graph-db:+data-extent-size+ 104857600)))

;;;; ============================================================================
;;;; NAMESPACE TESTS
;;;; ============================================================================

(test test-l1.globals.vertex-namespace-defined
  "Verify *vertex-namespace* is defined."
  (is (boundp 'graph-db:*vertex-namespace*)))

(test test-l1.globals.vertex-namespace-is-16-byte-array
  "Verify *vertex-namespace* is 16-byte array."
  (is (vectorp graph-db:*vertex-namespace*))
  (is (= (length graph-db:*vertex-namespace*) 16)))

(test test-l1.globals.edge-namespace-defined
  "Verify *edge-namespace* is defined."
  (is (boundp 'graph-db:*edge-namespace*)))

(test test-l1.globals.edge-namespace-is-16-byte-array
  "Verify *edge-namespace* is 16-byte array."
  (is (vectorp graph-db:*edge-namespace*))
  (is (= (length graph-db:*edge-namespace*) 16)))

(test test-l1.globals.vertex-and-edge-namespaces-distinct
  "Verify *vertex-namespace* != *edge-namespace*."
  (is (not (equalp graph-db:*vertex-namespace* graph-db:*edge-namespace*))))

;;;; ============================================================================
;;;; SENTINEL TESTS
;;;; ============================================================================

(test test-l1.globals.min-sentinel-defined
  "Verify +min-sentinel+ is defined."
  (is (boundp 'graph-db:+min-sentinel+)))

(test test-l1.globals.min-sentinel-value
  "Verify +min-sentinel+ is :gmin."
  (is (eq graph-db:+min-sentinel+ :gmin)))

(test test-l1.globals.max-sentinel-defined
  "Verify +max-sentinel+ is defined."
  (is (boundp 'graph-db:+max-sentinel+)))

(test test-l1.globals.max-sentinel-value
  "Verify +max-sentinel+ is :gmax."
  (is (eq graph-db:+max-sentinel+ :gmax)))

(test test-l1.globals.reduce-master-key-defined
  "Verify +reduce-master-key+ is defined."
  (is (boundp 'graph-db:+reduce-master-key+)))

(test test-l1.globals.reduce-master-key-value
  "Verify +reduce-master-key+ is :gagg."
  (is (eq graph-db:+reduce-master-key+ :gagg)))

;;;; ============================================================================
;;;; VE-INDEX TESTS
;;;; ============================================================================

(test test-l1.globals.ve-key-bytes-defined
  "Verify +ve-key-bytes+ is defined."
  (is (boundp 'graph-db:+ve-key-bytes+)))

(test test-l1.globals.ve-key-bytes-value
  "Verify +ve-key-bytes+ is 18."
  (is (= graph-db:+ve-key-bytes+ 18)))

(test test-l1.globals.ve-key-bytes-formula
  "Verify +ve-key-bytes+ = +key-bytes+ + 2 (for edge type)."
  (is (= graph-db:+ve-key-bytes+ (+ graph-db:+key-bytes+ 2))))

(test test-l1.globals.null-ve-key-defined
  "Verify +null-ve-key+ is defined."
  (is (boundp 'graph-db:+null-ve-key+)))

(test test-l1.globals.null-ve-key-correct-size
  "Verify +null-ve-key+ is correct size."
  (is (= (length graph-db:+null-ve-key+) graph-db:+ve-key-bytes+)))

(test test-l1.globals.null-ve-key-all-zeros
  "Verify +null-ve-key+ is all zeros."
  (is (every #'zerop graph-db:+null-ve-key+)))

(test test-l1.globals.max-ve-key-defined
  "Verify +max-ve-key+ is defined."
  (is (boundp 'graph-db:+max-ve-key+)))

(test test-l1.globals.max-ve-key-correct-size
  "Verify +max-ve-key+ is correct size."
  (is (= (length graph-db:+max-ve-key+) graph-db:+ve-key-bytes+)))

(test test-l1.globals.max-ve-key-all-255
  "Verify +max-ve-key+ is all 0xFF."
  (is (every (lambda (b) (= b 255)) graph-db:+max-ve-key+)))

;;;; ============================================================================
;;;; VEV-INDEX TESTS
;;;; ============================================================================

(test test-l1.globals.vev-key-bytes-defined
  "Verify +vev-key-bytes+ is defined."
  (is (boundp 'graph-db:+vev-key-bytes+)))

(test test-l1.globals.vev-key-bytes-value
  "Verify +vev-key-bytes+ is 34."
  (is (= graph-db:+vev-key-bytes+ 34)))

(test test-l1.globals.vev-key-bytes-formula
  "Verify +vev-key-bytes+ = 2 * +key-bytes+ + 2."
  (is (= graph-db:+vev-key-bytes+ (+ (* 2 graph-db:+key-bytes+) 2))))

(test test-l1.globals.null-vev-key-defined
  "Verify +null-vev-key+ is defined."
  (is (boundp 'graph-db:+null-vev-key+)))

(test test-l1.globals.null-vev-key-correct-size
  "Verify +null-vev-key+ is correct size."
  (is (= (length graph-db:+null-vev-key+) graph-db:+vev-key-bytes+)))

(test test-l1.globals.max-vev-key-defined
  "Verify +max-vev-key+ is defined."
  (is (boundp 'graph-db:+max-vev-key+)))

(test test-l1.globals.max-vev-key-correct-size
  "Verify +max-vev-key+ is correct size."
  (is (= (length graph-db:+max-vev-key+) graph-db:+vev-key-bytes+)))

;;;; ============================================================================
;;;; TYPE CODE TESTS (COMPREHENSIVE)
;;;; ============================================================================

(test test-l1.globals.type-codes-defined
  "Verify all type codes are defined."
  ;; Sample a few key ones
  (is (boundp 'graph-db:+unknown+))
  (is (boundp 'graph-db:+negative-integer+))
  (is (boundp 'graph-db:+positive-integer+))
  (is (boundp 'graph-db:+string+))
  (is (boundp 'graph-db:+vertex+))
  (is (boundp 'graph-db:+edge+))
  (is (boundp 'graph-db:+uuid+))
  (is (boundp 'graph-db:+timestamp+)))

(test test-l1.globals.type-codes-unique
  "Verify type codes 0-30 are unique."
  (let ((codes (list graph-db:+unknown+
                      graph-db:+negative-integer+
                      graph-db:+positive-integer+
                      graph-db:+character+
                      graph-db:+symbol+
                      graph-db:+string+
                      graph-db:+list+
                      graph-db:+vector+
                      graph-db:+single-float+
                      graph-db:+double-float+
                      graph-db:+ratio+
                      graph-db:+t+
                      graph-db:+null+
                      graph-db:+blob+
                      graph-db:+dotted-list+
                      graph-db:+keyword+
                      graph-db:+slot-key+
                      graph-db:+id+
                      graph-db:+vertex+
                      graph-db:+edge+
                      graph-db:+skip-list+
                      graph-db:+ve-index+
                      graph-db:+type-index+
                      graph-db:+pcons+
                      graph-db:+pqueue+
                      graph-db:+mpointer+
                      graph-db:+pcell+
                      graph-db:+index-list+
                      graph-db:+vev-index+
                      graph-db:+bit-vector+
                      graph-db:+bignum+)))
    (is (= (length codes) (length (remove-duplicates codes))))))

(test test-l1.globals.type-codes-range
  "Verify type codes 0-30 are in range [0, 30]."
  (is (and (>= graph-db:+unknown+ 0) (<= graph-db:+unknown+ 30)))
  (is (and (>= graph-db:+bignum+ 0) (<= graph-db:+bignum+ 30))))

(test test-l1.globals.user-type-codes-above-100
  "Verify user-defined codes start at 100."
  (is (= graph-db:+uuid+ 100))
  (is (= graph-db:+timestamp+ 101)))

;;;; ============================================================================
;;;; PARAMETER TESTS
;;;; ============================================================================

(test test-l1.globals.initial-extents-defined
  "Verify *initial-extents* is defined."
  (is (boundp 'graph-db:*initial-extents*)))

(test test-l1.globals.initial-extents-value
  "Verify *initial-extents* is 10."
  (is (= graph-db:*initial-extents* 10)))

(test test-l1.globals.max-locks-defined
  "Verify *max-locks* is defined."
  (is (boundp 'graph-db:*max-locks*)))

(test test-l1.globals.max-locks-value
  "Verify *max-locks* is 10000."
  (is (= graph-db:*max-locks* 10000)))

(test test-l1.globals.graph-hash-defined
  "Verify *graph-hash* is defined."
  (is (boundp 'graph-db:*graph-hash*)))

;;;; ============================================================================
;;;; PROLOG ENGINE STATE TESTS
;;;; ============================================================================

(test test-l1.globals.occurs-check-defined
  "Verify *occurs-check* is defined."
  (is (boundp 'graph-db:*occurs-check*)))

(test test-l1.globals.occurs-check-default-true
  "Verify *occurs-check* defaults to T."
  (is (eq graph-db:*occurs-check* t)))

(test test-l1.globals.trail-defined
  "Verify *trail* is defined."
  (is (boundp 'graph-db:*trail*)))

(test test-l1.globals.trail-is-array
  "Verify *trail* is an adjustable array."
  (is (arrayp graph-db:*trail*))
  (is (adjustable-array-p graph-db:*trail*)))

(test test-l1.globals.var-counter-defined
  "Verify *var-counter* is defined."
  (is (boundp 'graph-db:*var-counter*)))

(test test-l1.globals.var-counter-integer
  "Verify *var-counter* is an integer."
  (is (integerp graph-db:*var-counter*)))

(test test-l1.globals.functor-defined
  "Verify *functor* is defined."
  (is (boundp 'graph-db:*functor*)))

(test test-l1.globals.functor-default-nil
  "Verify *functor* defaults to NIL."
  (is (null graph-db:*functor*)))

(test test-l1.globals.select-list-defined
  "Verify *select-list* is defined."
  (is (boundp 'graph-db:*select-list*)))

(test test-l1.globals.select-list-default-nil
  "Verify *select-list* defaults to NIL."
  (is (null graph-db:*select-list*)))

(test test-l1.globals.cont-defined
  "Verify *cont* is defined."
  (is (boundp 'graph-db:*cont*)))

(test test-l1.globals.cont-default-nil
  "Verify *cont* defaults to NIL."
  (is (null graph-db:*cont*)))

;;;; ============================================================================
;;;; PROLOG FUNCTORS TESTS (PLATFORM-SPECIFIC)
;;;; ============================================================================

(test test-l1.globals.prolog-global-functors-defined
  "Verify *prolog-global-functors* is defined."
  (is (boundp 'graph-db:*prolog-global-functors*)))

(test test-l1.globals.prolog-global-functors-hash-table
  "Verify *prolog-global-functors* is a hash table."
  (is (hash-table-p graph-db:*prolog-global-functors*)))

(test test-l1.globals.user-functors-defined
  "Verify *user-functors* is defined."
  (is (boundp 'graph-db:*user-functors*)))

(test test-l1.globals.user-functors-hash-table
  "Verify *user-functors* is a hash table."
  (is (hash-table-p graph-db:*user-functors*)))

;;;; ============================================================================
;;;; PROLOG SPECIAL VALUES TESTS
;;;; ============================================================================

(test test-l1.globals.prolog-trace-defined
  "Verify *prolog-trace* is defined."
  (is (boundp 'graph-db:*prolog-trace*)))

(test test-l1.globals.unbound-defined
  "Verify +unbound+ is defined."
  (is (boundp 'graph-db:+unbound+)))

(test test-l1.globals.unbound-value
  "Verify +unbound+ is :unbound."
  (is (eq graph-db:+unbound+ :unbound)))

(test test-l1.globals.no-bindings-defined
  "Verify +no-bindings+ is defined."
  (is (boundp 'graph-db:+no-bindings+)))

(test test-l1.globals.no-bindings-is-list
  "Verify +no-bindings+ is a list."
  (is (listp graph-db:+no-bindings+)))

(test test-l1.globals.fail-defined
  "Verify +fail+ is defined."
  (is (boundp 'graph-db:+fail+)))

(test test-l1.globals.fail-value
  "Verify +fail+ is NIL."
  (is (null graph-db:+fail+)))

;;;; ============================================================================
;;;; INTEGRATION TESTS
;;;; ============================================================================

(test test-l1.globals.key-sizes-consistent
  "Verify key size constants are internally consistent."
  (is (= graph-db:+bucket-size+ (+ graph-db:+key-bytes+ graph-db:+value-bytes+)))
  (is (= graph-db:+ve-key-bytes+ (+ graph-db:+key-bytes+ 2)))
  (is (= graph-db:+vev-key-bytes+ (+ (* 2 graph-db:+key-bytes+) 2))))

(test test-l1.globals.sentinel-keys-correct-size
  "Verify sentinel keys match key-bytes sizes."
  (is (= (length graph-db:+null-key+) graph-db:+key-bytes+))
  (is (= (length graph-db:+max-key+) graph-db:+key-bytes+))
  (is (= (length graph-db:+null-ve-key+) graph-db:+ve-key-bytes+))
  (is (= (length graph-db:+null-vev-key+) graph-db:+vev-key-bytes+)))

;;;; ============================================================================
;;;; TEST RUNNER
;;;; ============================================================================

(defun run-l1-globals-tests ()
  "Run all Layer 1 Globals tests and report results."
  (let* ((results (run! 'l1.globals))
         (passed (fiveam:results-passed results))
         (failed (fiveam:results-failed results))
         (skipped (fiveam:results-skipped results)))
    (format t "~&Layer 1 Globals Tests Summary:~%")
    (format t "  Passed:  ~D~%" passed)
    (format t "  Failed:  ~D~%" failed)
    (format t "  Skipped: ~D~%" skipped)
    (format t "  Total:   ~D~%" (+ passed failed skipped))
    (zerop failed)))

(in-package :cl-user)
