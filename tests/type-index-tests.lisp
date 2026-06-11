;;;; Tests for the type-index (type-index.lisp): a fixed table of per-type-id
;;;; index-lists mapping a node type to the ids of its members.
;;;;
;;;; The reopen path (open-type-index/deserialize-index-list) needs a bound
;;;; *graph*, so these exercise the in-memory push/remove/get operations.

(in-package #:graph-db/test)

(def-suite type-index-suite
  :description "type-index push / remove / get / per-type isolation."
  :in graph-db-suite)

(in-suite type-index-suite)

(test push-then-present
  (with-temp-type-index (idx heap)
    (let ((id (gen-id)))
      (type-index-push id 1 idx)
      (is-true (key-in-list-p id (get-type-index-list idx 1))))))

(test types-are-isolated
  "Ids pushed under one type-id do not appear under another."
  (with-temp-type-index (idx heap)
    (let ((a (gen-id)) (b (gen-id)))
      (type-index-push a 1 idx)
      (type-index-push b 2 idx)
      (is-true (key-in-list-p a (get-type-index-list idx 1)))
      (is-true (key-in-list-p b (get-type-index-list idx 2)))
      (is-false (key-in-list-p a (get-type-index-list idx 2)))
      (is-false (key-in-list-p b (get-type-index-list idx 1))))))

(test multiple-ids-per-type
  (with-temp-type-index (idx heap)
    (let ((ids (loop repeat 10 collect (gen-id))))
      (dolist (id ids) (type-index-push id 7 idx))
      (let ((listed (index-list-keys (get-type-index-list idx 7))))
        (is (= 10 (length listed)))
        (dolist (id ids)
          (is-true (member id listed :test #'equalp)))))))

(test unless-present-deduplicates
  (with-temp-type-index (idx heap)
    (let ((id (gen-id)))
      (type-index-push id 3 idx :unless-present t)
      (type-index-push id 3 idx :unless-present t)
      (is (= 1 (length (index-list-keys (get-type-index-list idx 3))))))))

(test remove-from-type
  (with-temp-type-index (idx heap)
    (let ((keep (gen-id)) (drop (gen-id)))
      (type-index-push keep 4 idx)
      (type-index-push drop 4 idx)
      (type-index-remove drop 4 idx)
      (is-true (key-in-list-p keep (get-type-index-list idx 4)))
      (is-false (key-in-list-p drop (get-type-index-list idx 4))))))
