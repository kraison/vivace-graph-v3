(deftype country () `(satisfies stringp))
(deftype currency () `(satisfies stringp))
(deftype url () `(satisfies stringp))

(defun phone-p (x)
  (and (stringp x)
       (= (length x) 10)
       (every (lambda (x)
                (member x (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
              x)
       x))
(deftype phone () `(satisfies phone-p))

(defun email-p (x)
  (and (stringp x)
       (find #\@ x)
       x))
(deftype email () `(satisfies email-p))

(defun zip-code-p (x)
  (and (stringp x)
       (= (length x) 5)
       (every (lambda (x)
                (member x (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
              x)
       x))
(deftype zip-code () `(satisfies zip-code-p))

(defun gender-p (x) (member x '("male" "female" "neuter") :test 'equalp))
(deftype gender () `(satisfies gender-p))

(setq *graph* (make-graph :test "/var/tmp/graph/"))

(def-vertex customer ()
  ((first-name :type string)
   middle-name
   (last-name :type string)
   (email :type email :index t :unique? t :not-null t)
   (address-1 :type string)
   address-2
   (city :type string)
   (state :type string)
   (zip :type zip-code)
   (country :type country)
   (phone-1 :type phone)
   (phone-2 :type phone)
   (latitude :type float :read-only t)
   (longitude :type float :read-only t)
   (gender :type gender)
   unique-identifier
   (curator? :type boolean :read-only t)
   (date-added :type date :read-only t :index t)
   (session :ephemeral? t))
  :test)

;  ((:single-field email :type :string :unique? t)
;   (:multi-field (last-name first-name middle-name) :type :full-text)
;   (:geo (address-1 address-2 city state zip country)))

(def-edge has-want-list ()
  ((timestamp :type integer))
  :test)

(def-vertex want-list ()
  ((name :type string)
   (public? :type boolean))
  :test)

(def-edge contains-product ()
  ()
  :test)

(def-vertex product ()
  ((sku :type string :not-null t)
   (name :type string :not-null t)
   (price :type integer :not-null t)
   (currency :type currency :not-null t :default "USD")
   (upccode :type string)
   (description :type string)
   (category-words :type list)
   (gender :type gender :read-only t :private t)
   (manually-cataloged? :type boolean :private t :index t)
   (linkurl :type url :private t)
   (imageurl :type url)
   short-description
   long-description
   (link-id :private t))
  :test)

(def-edge in-taxon ()
  ()
  :test)

(def-edge prefers-taxon ()
  ()
  :test)

(def-vertex product-taxon ()
  ((name :type string :unique? t))
  :test)

(def-edge in-circle ()
  ((name :type string)
   (public? :type boolean))
  :test)

(def-view email (customer :test)
  (lambda (customer)
    (emit (email customer) nil)))

(defun lookup-customer-by-email (email)
  (let ((r (exec-view 'customer 'email :key email)))
    (let ((id (cdr (assoc :id (first r)))))
      (when id
        (lookup-vertex id)))))

(defvar *c* (make-customer :email "raison@chatsubo.net"))
(make-customer :email "bob@chatsubo.net")
(make-customer :email "kevin@chatsubo.net")
(make-customer :email "zed@chatsubo.net")
(make-customer :email "joe@chatsubo.net")
(make-customer :email "ed@chatsubo.net")
(make-customer :email "x-ray@chatsubo.net")
(make-customer :email "delta@chatsubo.net")
(dotimes (i 1000)
  (make-customer :email (format nil "~20,'0D@xyz.com" i)))

(def-view taxon-name (product-taxon :test)
  (lambda (taxon)
    (emit (name taxon) nil)))

(defvar *comp* (make-product-taxon :name "computers"))
(make-product-taxon :name "shoes")
(make-product-taxon :name "jewelry")

(make-prefers-taxon :from (id *c*) :to (id *comp*) :weight 1.0)
