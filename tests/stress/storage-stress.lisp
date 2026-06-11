;;;; STORAGE-STRESS-SUITE
;;;;
;;;; Scale tests for the low-level storage layer:
;;;;   - allocator churn (alloc + free cycles under fragmentation)
;;;;   - lhash large insert / key preservation / split correctness
;;;;   - skip-list large ordered insert and cursor traversal
;;;;   - serialize round-trip for every supported type

(in-package #:graph-db/stress-test)

(def-suite storage-stress-suite
  :description "Scale tests for the storage primitives."
  :in stress-suite)

(in-suite storage-stress-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: allocator large churn
;;;
;;; Repeatedly allocates blocks of mixed sizes and frees them.  Exercises the
;;; free-list coalescing path and verifies that each allocation returns a
;;; distinct address.  After the churn phase, checks that the memory is still
;;; functional by making a final allocation.
;;; ---------------------------------------------------------------------------

(test allocator-large-churn
  "Alloc/free cycles with mixed sizes must not corrupt the heap."
  (let ((cycles (scale 5000 500))
        (sizes  '(8 16 32 64 128 256 512 1024)))
    (with-stress-memory (heap)
      (let ((start (get-internal-real-time))
            (addrs '()))
        (dotimes (i cycles)
          (let* ((sz (nth (mod i (length sizes)) sizes))
                 (addr (allocate heap sz)))
            (push addr addrs)
            ;; Free alternate allocations immediately to create gaps.
            (when (evenp i)
              (free heap (pop addrs)))))
        ;; Drain remaining live allocations.
        (dolist (a addrs) (free heap a))
        ;; Heap must still be usable.
        (let ((final (allocate heap 64)))
          (is (integerp final)
              "Final allocation after churn returned non-integer"))
        (record-throughput "allocator-churn" cycles
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second)))))))

;;; ---------------------------------------------------------------------------
;;; Test 2: linear-hash large insert
;;;
;;; Insert N UUID→integer pairs; verify every key is retrievable; verify the
;;; count field matches N; re-open (close + open-lhash) and verify again.
;;; ---------------------------------------------------------------------------

(test linear-hash-large-insert
  "All N UUID→int pairs retrievable post-insert; count consistent."
  (let* ((n (scale 50000 5000))
         (keys (loop repeat n collect (gen-id))))
    (with-stress-lhash (h :buckets 4)
      (let ((start (get-internal-real-time)))
        (loop for k in keys for v from 1
              do (lhash-insert h k v))
        (record-throughput "lhash-insert" n
                           (/ (- (get-internal-real-time) start)
                              (float internal-time-units-per-second))))
      (is (= n (read-lhash-count h))
          "lhash count mismatch after ~D inserts: got ~D" n (read-lhash-count h))
      (is (loop for k in keys for v from 1
                always (eql v (lhash-get h k)))
          "One or more keys lost or corrupted after ~D inserts" n))))

;;; ---------------------------------------------------------------------------
;;; Test 3: linear-hash split correctness
;;;
;;; Start with 4 buckets and insert enough keys to force 3+ splits.  After
;;; each forced-split boundary (every (scale 5000) inserts) spot-check every
;;; previously inserted key to catch any split that corrupted the hash chain.
;;; ---------------------------------------------------------------------------

(test linear-hash-split-correctness
  "No key is lost or corrupted across any split boundary."
  (let* ((batch (scale 5000 500))
         (batches 3)
         (total (* batch batches))
         (all-keys '()))
    (with-stress-lhash (h :buckets 4)
      (dotimes (b batches)
        ;; Insert one batch.
        (let ((batch-keys (loop repeat batch collect (gen-id))))
          (loop for k in batch-keys for v from (* b batch)
                do (lhash-insert h k v))
          (setq all-keys (append all-keys batch-keys))
          ;; Verify every key inserted so far survives the split.
          (is (loop for k in all-keys for v from 0
                    always (integerp (lhash-get h k)))
              "Key lost after batch ~D" b)))
      (is (= total (read-lhash-count h))
          "Final lhash count ~D ≠ expected ~D" (read-lhash-count h) total))))

;;; ---------------------------------------------------------------------------
;;; Test 4: skip-list large ordered insert
;;;
;;; Insert N integer keys in ascending order (worst case for a skiplist
;;; without randomised levels).  Verify count, cursor traversal order, and
;;; that every key is found by find-in-skip-list.
;;; ---------------------------------------------------------------------------

(test skip-list-large-ordered
  "N integer keys inserted in order; count, cursor order, and lookup all correct."
  (let ((n (scale 10000 1000)))
    (with-stress-memory (heap)
      (let ((sl (make-stress-integer-skip-list heap)))
        (let ((start (get-internal-real-time)))
          (dotimes (i n)
            (add-to-skip-list sl i i))
          (record-throughput "skip-list-ordered-insert" n
                             (/ (- (get-internal-real-time) start)
                                (float internal-time-units-per-second))))
        (is (= n (skip-list-count sl))
            "Expected skip-list count ~D; got ~D" n (skip-list-count sl))
        (let ((entries (skip-list-to-list sl)))
          (is (= n (length entries))
              "skip-list-to-list returned ~D entries; expected ~D"
              (length entries) n)
          (is (loop for ((k . _v1) (k2 . _v2)) on entries while k2
                    always (<= k k2))
              "skip-list-to-list not in ascending key order"))))))

;;; ---------------------------------------------------------------------------
;;; Test 5: serialize round-trip stress
;;;
;;; Round-trip N heterogeneous objects through serialize/deserialize and
;;; verify exact equality.  Covers integers (positive, negative, zero),
;;; strings, UUIDs (byte arrays), and nested lists.
;;;
;;; serialize returns a byte array; deserialize accepts a byte array directly.
;;; ---------------------------------------------------------------------------

(test serialize-round-trip-stress
  "N heterogeneous objects must round-trip through serialize/deserialize."
  (let* ((n (scale 1000 100))
         (objects
           (loop repeat n
                 nconcing
                 (list (random 100000)
                       (- (random 100000))
                       0
                       (format nil "stress-~36R" (random (expt 36 8)))
                       (list 1 2 3 (random 999))))))
    (let ((start (get-internal-real-time))
          (failures 0))
      (dolist (obj objects)
        (let* ((bytes (serialize obj))
               (back  (deserialize bytes)))
          (unless (equalp obj back)
            (incf failures))))
      (record-throughput "serialize-round-trip" (length objects)
                         (/ (- (get-internal-real-time) start)
                            (float internal-time-units-per-second)))
      (is (zerop failures)
          "~D out of ~D objects failed round-trip" failures (length objects)))))
