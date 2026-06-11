;;;; CONCURRENT-MMAP-REMAP-SUITE
;;;;
;;;; Targeted regression for the mmap remap-vs-reader race (see
;;;; docs/mmap-remap-race-plan.md).  Writer threads drive the allocator to grow
;;;; (mremap / munmap+mmap) a memory-mapped file repeatedly while reader threads
;;;; hammer byte accessors on the same file.  Before the remap-lock fix this
;;;; faulted (SIGSEGV) on every implementation; the SEGV-retry :around handlers
;;;; then masked it on SBCL but collapsed on CCL/ECL at high thread counts.
;;;; After the fix no access may fault: *mmap-segv-retries* must stay 0 and no
;;;; thread may error.

(in-package #:graph-db/concurrent-stress-test)

(def-suite concurrent-mmap-remap-suite
  :description "mmap heap-grow races concurrent readers; no faults allowed."
  :in concurrent-stress-suite)

(in-suite concurrent-mmap-remap-suite)

(test remap-under-concurrent-readers
  "Force many small heap grows while readers hammer the mapping; with the
remap-lock no byte access faults (0 SEGV-retries), nothing errors, and a
sentinel byte survives the remaps intact."
  ;; Small initial map + small extents => frequent remaps (the dangerous event).
  (with-temp-directory (dir)
    (let* ((path   (namestring (merge-pathnames "heap.dat" dir)))
           (memory (graph-db::create-memory path (* 256 1024)
                                            :extent-size (* 64 1024)))
           (t-count (max 8 *stress-thread-count*))
           (writers (max 1 (floor t-count 2)))
           (allocs  500))   ; allocations per writer -> forces repeated growth
      (setf graph-db::*mmap-segv-retries* 0)
      (unwind-protect
           ;; Allocate a sentinel block before the storm and read it back from
           ;; the reader threads.  It lives at a fixed, always-valid offset;
           ;; when a concurrent grow moves the mapping, an unlocked read here
           ;; would dereference a stale/NIL pointer and fault.
           (multiple-value-bind (sentinel size) (graph-db::allocate memory 8)
             (declare (ignore size))
             (graph-db::set-byte (graph-db::memory-mmap memory) sentinel 123)
             (run-threads
              t-count
              (lambda (i)
                (if (< i writers)
                    ;; Writer: allocate (grows the heap) and write each block.
                    (dotimes (_ allocs)
                      (multiple-value-bind (offset sz)
                          (graph-db::allocate memory 64)
                        (declare (ignore sz))
                        (graph-db::set-byte (graph-db::memory-mmap memory)
                                            offset 42)))
                    ;; Reader: hammer the sentinel offset.
                    (dotimes (_ (* allocs 4))
                      (graph-db::get-byte (graph-db::memory-mmap memory)
                                          sentinel)))))
             (is (zerop graph-db::*mmap-segv-retries*)
                 "Expected 0 mmap SEGV-retries under concurrent remap; got ~D"
                 graph-db::*mmap-segv-retries*)
             (is (= 123 (graph-db::get-byte (graph-db::memory-mmap memory)
                                            sentinel))
                 "Sentinel byte did not survive the remaps"))
        (ignore-errors (graph-db::close-memory memory))
        (collect-garbage)))))
