(ql:quickload :graph-db)
(use-package :graph-db)
(defvar *graph-name* :test-graph)
(defvar *graph-path* "/var/tmp/test-graph/")
(setq *graph* (make-graph *graph-name* *graph-path*))

;;; Types
(defun email-p (x)
  (and (stringp x)
       (find #\@ x)
       x))
(deftype email () `(satisfies email-p))

;;; Schema
(def-vertex person ()
  ((first-name :type string)
   (middle-name :type string)
   (last-name :type string))
  :test-graph)

(def-vertex customer (person)
  ((email :type email))
  :test-graph)

(def-vertex product ()
  ((name :type string)
   (upc :type string))
  :test-graph)

(def-vertex merchant ()
  ((name :type string))
  :test-graph)

(def-edge likes ()
 ()
 :test-graph)

(def-edge sells ()
 ()
 :test-graph)

;;; Indexes
;; This will index both customers and people
(def-view last-name :lessp (person :test-graph)
  (:map
   (lambda (person)
     (when (last-name person)
       (yield (last-name person) nil)))))

;; This will only index customers
(def-view email :greaterp (customer :test-graph)
  (:map
   (lambda (customer)
     (when (email customer)
       (yield (email customer) nil)))))

(defun lookup-people-by-last-name (last-name)
  (let ((people (invoke-graph-view 'person 'last-name :key last-name)))
    (if people
        (mapcar (lambda (person)
                  (lookup-vertex (cdr (assoc :id person))))
                people)
        nil)))

(defun lookup-customer-by-email (email)
  (let ((customers (invoke-graph-view 'customer 'email :key email)))
    (if customers
        (lookup-vertex (cdr (assoc :id (first customers))))
        nil)))

;;; Add some data
(with-transaction ()
  (let ((c1 (make-customer :first-name "Joe" :last-name "Blow" :email "joe@blow.com"))
        (c2 (make-customer :first-name "Jill" :last-name "Blow" :email "jill@blow.com"))
        (m1 (make-merchant :name "Snake Oil, Inc."))
        (p1 (make-product :name "Oil of Longevity" :upc "1234567890"))
        (p2 (make-product :name "Oil of Slipperiness" :upc "abcdefghijk")))
    (make-sells :from m1 :to p1)
    ;; The above is equivalent to
    ;; (make-edge 'sells m1 p1 1 nil)
    (make-sells :from m1 :to p2)
    (make-likes :from c1 :to p1 :weight 100.0)
    (make-likes :from c2 :to p2 :weight 50.0)))

;;; Now run some queries
(lookup-customer-by-email "joe@blow.com")

(lookup-people-by-last-name "Blow")

(select (:flat nil)
        (?liker ?product)
        (likes ?liker ?product))

(select (:flat nil :limit 1 :skip 0)
        (?liker ?product ?how-much)
        (likes ?liker ?product ?how-much))

(select-flat (?customer) (is-a ?customer customer))

(let ((person (select-one (?person) (is-a ?person person))))
  (declare (special person))
  (destructuring-bind (product like-qty)
      (select (:flat t :limit 1 :skip 0)
              (?product ?like-qty)
              (lisp ?person person) ;; Import the person into Prolog
              (likes ?person ?product ?like-qty))
    (format nil "~A likes '~A' with a degree of ~F"
            (first-name person)
            (name product)
            like-qty)))

(close-graph *graph*)
