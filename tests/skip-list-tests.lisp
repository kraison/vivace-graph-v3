;;;; Tests for the persistent skip list (skip-list.lisp).
;;;;
;;;; Each test builds an integer-keyed skip list over a temp heap via the
;;;; MAKE-INTEGER-SKIP-LIST fixture in suite.lisp.

(in-package #:graph-db/test)

(def-suite skip-list-suite
  :description "skip list add / find / remove / ordering / count."
  :in graph-db-suite)

(in-suite skip-list-suite)

(defun sl-find-value (sl key)
  "Return the value stored under KEY in SL, or NIL if absent."
  (let ((node (find-in-skip-list sl key)))
    (and node (%sn-value node))))

(defun sl-live-count (sl)
  "Number of live entries, counted by walking the level-0 chain.
NB: we deliberately do NOT use graph-db's SKIP-LIST-COUNT here -- it has an
infinite loop (it never advances its cursor).  SKIP-LIST-TO-LIST walks the
chain correctly, and %SL-LENGTH is the maintained counter."
  (length (skip-list-to-list sl)))

(test add-and-find
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 5 "five")
      (add-to-skip-list sl 1 "one")
      (add-to-skip-list sl 9 "nine")
      (is (string= "five" (sl-find-value sl 5)))
      (is (string= "one" (sl-find-value sl 1)))
      (is (string= "nine" (sl-find-value sl 9))))))

(test find-missing-returns-nil
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 5 "five")
      (is (null (find-in-skip-list sl 6))))))

(test maintains-sorted-order
  "Keys inserted in random order are returned in ascending order."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap))
          (keys (alexandria:shuffle (loop for i below 100 collect i))))
      (dolist (k keys) (add-to-skip-list sl k (* k k)))
      (let ((dumped (skip-list-to-list sl)))
        (is (equal (loop for i below 100 collect i)
                   (mapcar #'car dumped)))
        ;; values came back attached to the right keys
        (is (every (lambda (pair) (= (cdr pair) (* (car pair) (car pair))))
                   dumped))))))

(test count-matches-inserts
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 50) (add-to-skip-list sl i i))
      (is (= 50 (sl-live-count sl)))
      (is (= 50 (%sl-length sl))))))

(test remove-deletes-key
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 10) (add-to-skip-list sl i (* 10 i)))
      (remove-from-skip-list sl 5)
      (is (null (find-in-skip-list sl 5)))
      (is (= 9 (sl-live-count sl)))
      ;; neighbours survive and the order is still intact
      (is (= 40 (sl-find-value sl 4)))
      (is (= 60 (sl-find-value sl 6)))
      (is (equal '(0 1 2 3 4 6 7 8 9) (mapcar #'car (skip-list-to-list sl)))))))

(test duplicate-add-ignored-without-duplicates
  "With duplicates disallowed, re-adding an existing key is a no-op: the
count and the original value are unchanged."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 7 "first")
      (add-to-skip-list sl 7 "second")
      (is (= 1 (sl-live-count sl)))
      (is (= 1 (%sl-length sl)))
      (is (string= "first" (sl-find-value sl 7))))))

(test skip-list-count-terminates
  "Regression: skip-list-count used to loop forever because it never
advanced its cursor.  It must now return the live element count."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 25) (add-to-skip-list sl i i))
      (is (= 25 (skip-list-count sl)))
      (is (= (sl-live-count sl) (skip-list-count sl))))))

(test bulk-insert-all-retrievable
  "A larger shuffled load stays fully retrievable and correctly counted."
  (with-temp-memory (heap :size (* 1024 1024 128))
    (let ((sl (make-integer-skip-list heap))
          (keys (alexandria:shuffle (loop for i below 1000 collect i))))
      (dolist (k keys) (add-to-skip-list sl k (- k)))
      (is (= 1000 (sl-live-count sl)))
      (dolist (k '(0 1 250 499 500 999))
        (is (= (- k) (sl-find-value sl k)))))))
