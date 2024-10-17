(in-package :social-shopping)

(defvar *customer-lock-count* 10000)
(defvar *customer-locks* (map-into (make-array *customer-lock-count*) 'make-rw-lock))

(defun lookup-customer-lock (customer)
  (aref *customer-locks*
        (mod (sxhash (if (customer-p customer)
                         (email customer)
                         customer))
             *customer-lock-count*)))

(defun acquire-customer-lock (customer &key reading-p (kind :read))
  (let ((lock (lookup-customer-lock customer)))
    (if (eql kind :write)
        (acquire-write-lock lock :reading-p reading-p)
        (acquire-read-lock lock))
    lock))

(defmacro with-write-locked-customer ((customer &key reading-p) &body body)
  (with-gensyms (lock)
    `(let ((,lock (acquire-customer-lock ,customer :kind :write :reading-p ,reading-p)))
       (unwind-protect
            (progn ,@body)
         (release-write-lock ,lock)))))

(defmacro with-read-locked-customer ((customer) &body body)
  (with-gensyms (lock)
    `(let ((,lock (acquire-customer-lock ,customer :kind :read)))
       (unwind-protect
            (progn ,@body)
         (release-read-lock ,lock)))))

(defmethod print-object ((c customer) stream)
  (format stream "'CUSTOMER ~A ~A'" (string-id c) (email c)))

(json-rpc::def-json-rpc-encoding :customer (raw-val)
  (list :json
        (json-rpc::encode-json-to-string (node-to-alist raw-val))))

(json-rpc::def-json-rpc-encoding :customer-list (raw-val)
  (list :json
        (json-rpc::encode-json-to-string
         (mapcar 'node-to-alist raw-val))))

(defgeneric create-circle (customer circle-name &key public-p description))

(defun lookup-customer-by-email (email)
  (declare (special email))
  (select-one (?customer)
              (lisp ?email email)
              (invoke-view customer email ?email ?customer)))

(defun lookup-customer-by-username (username)
  (declare (special username))
  (select-one (?customer)
              (lisp ?username username)
              (invoke-view customer username ?username ?customer)))

(defun lookup-customer-by-unique-identifier (unique-identifier)
  (declare (special unique-identifier))
  (select-one (?customer)
              (lisp ?uid unique-identifier)
              (invoke-view customer unique-identifier ?uid ?customer)))

(defun get-all-customers-by-unique-identifier (unique-identifier)
  (declare (special unique-identifier))
  (select-flat (?customer)
               (lisp ?uid unique-identifier)
               (invoke-view customer unique-identifier ?uid ?customer)))

(defmethod add-address ((customer customer) &key address-1 address-2 city state zip
                        country)
  (let ((string-address
         (make-string-address :address-1 address-1
                              :address-2 address-2
                              :city city
                              :state state
                              :zip zip
                              :country country)))
    (multiple-value-bind (lat lon)
        (handler-case
            (geo-locate-helper string-address)
          (error (c)
            (log:debug "Cannot geo-locate ~A: ~A" customer c)
            (values nil nil))
      (with-transaction ()
        (let ((address (make-address :latitude lat
                                     :longitude lon
                                     :address-1 address-1
                                     :address-2 address-2
                                     :city city
                                     :state state
                                     :zip zip
                                     :country country)))
          (make-has-address :from customer :to address :date-added (now))
          ;; FIXME: add to search index for geolocation
          address))))))

(defmethod address ((customer customer))
  (declare (special customer))
  (select-one (?address)
              (lisp ?c customer)
              (has-address ?c ?address)))

(defmethod update-address ((customer customer) p)
  (let ((address (address customer)))
    ;; This is not wrapped in a transaction, because UPDATE-CUSTOMER
    ;; establishes the outer transaction. When calling this directly,
    ;; it must be done within a transaction, or it will signal a
    ;; NO-TRANSACTION-IN-PROGRESS error.
    (if address
        (let ((new-address (copy address)))
          (when (@ p :address-1)
            (setf (address-1 new-address) (@ p :address-1)))
          (when (@ p :address-2)
            (setf (address-2 new-address) (@ p :address-2)))
          (when (@ p :city)
            (setf (city new-address) (@ p :city)))
          (when (@ p :state)
            (setf (city new-address) (@ p :city)))
          (when (@ p :zip)
            (setf (zip new-address) (@ p :zip)))
          (when (@ p :country)
            (setf (country new-address) (@ p :country)))
          (save new-address))
        (add-address customer
                     :address-1 (@ p :address-1)
                     :address-2 (@ p :address-2)
                     :city (@ p :city)
                     :state (@ p :state)
                     :zip (@ p :zip)
                     :country (@ p :country)))))

(defmethod send-welcome-email ((customer customer))
  (handler-case
      (submit-mail (email customer)
                   "Welcome to OfferSavvy"
                   (with-output-to-string (email)
                     (with-open-file (in *welcome-email-txt-file*)
                       (do ((line (read-line in nil :eof)
                                  (read-line in nil :eof)))
                           ((eql line :eof))
                         (format email "~A~%" line))))
                   :html-p
                   (with-output-to-string (email)
                     (with-open-file (in *welcome-email-html-file*)
                       (do ((line (read-line in nil :eof)
                                  (read-line in nil :eof)))
                           ((eql line :eof))
                         (format email "~A~%" line)))))
    (error (c)
      (log:error "Coud not send welcome email to ~A: ~A" customer c))))

(defmethod incf-points ((customer customer) &optional (delta 1))
  (with-write-locked-customer (customer)
    (let ((c (copy customer)))
      (if (integerp (points c))
          (incf (points c) delta)
          (setf (points c) delta))
      (save c))))

(defmethod decf-points ((customer customer) &optional (delta 1))
  (with-write-locked-customer (customer)
    (let ((c (copy customer)))
      (if (integerp (points c))
          (decf (points c) delta)
          (setf (points c) 0))
      (save c))))

(defun create-customer (&key first-name last-name email middle-name
                        unique-identifier phone-1 phone-2 gender
                        visible-in-search show-my-circles address-1
                        address-2 city state zip country curator-p
                        id username (points 0))
  (unless email
    (error "Cannot create a customer without an email address."))
  (with-write-locked-customer (email)
    (cond ((customer-p (lookup-customer-by-email email))
           (error "Customer with email ~A already exists." email))
          ((and username
                (customer-p (lookup-customer-by-username username)))
           ;; FIXME: race condition on username
           (error "Customer with username ~A already exists." username))
          (t
           (with-transaction ()
             (let ((customer
                    (make-customer :id id
                                   :points points
                                   :date-added (now)
                                   :curator-p curator-p
                                   :first-name first-name
                                   :middle-name middle-name
                                   :last-name last-name
                                   :gender gender
                                   :email email
                                   :username username
                                   :phone-1 phone-1
                                   :phone-2 phone-2
                                   :visible-in-search visible-in-search
                                   :show-my-circles show-my-circles
                                   :unique-identifier unique-identifier)))
               (when (and address-1 city state zip)
                 (handler-case
                     (add-address customer
                                  :address-1 address-1
                                  :address-2 address-2
                                  :city city
                                  :state state
                                  :zip zip
                                  :country country)
                   (error (c)
                     (mark-deleted customer)
                     (error c))))
               (add-to-search-index customer)
               (send-welcome-email customer)
               customer))))))

(defmethod update-customer ((customer customer) p)
  (let ((lock nil)
        (new-customer nil))
    (unwind-protect
         (with-transaction ()
           (setf new-customer (copy customer))
           (when (and (@ p :email) (not (equal (email customer) (@ p :email))))
             (setq lock (acquire-customer-lock (@ p :email) :kind :write))
             (if (lookup-customer-by-email (@ p :email))
                 (error "Email address '~A' is in use." (@ p :email))
                 (setf (email new-customer) (@ p :email)))
             (let ((saved-customer (save new-customer)))
               (setq new-customer (copy saved-customer)))
             (release-write-lock lock))

           (when (and (@ p :username) (not (equal (username customer) (@ p :username))))
             (setq lock (acquire-customer-lock (@ p :username) :kind :write))
             (if (lookup-customer-by-username (@ p :username))
                 (error "username '~A' is in use." (@ p :username))
                 (setf (username new-customer) (@ p :username)))
             (let ((saved-customer (save new-customer)))
               (setq new-customer (copy saved-customer)))
             (release-write-lock lock))

           (when (member :first-name p :key 'car)
             (setf (first-name new-customer) (@ p :first-name)))
           (when (member :middle-name p :key 'car)
             (setf (middle-name new-customer) (@ p :middle-name)))
           (when (member :last-name p :key 'car)
             (setf (last-name new-customer) (@ p :last-name)))
           (when (member :phone-1 p :key 'car)
             (setf (phone-1 new-customer) (@ p :phone-1)))
           (when (member :phone-2 p :key 'car)
             (setf (phone-2 new-customer) (@ p :phone-2)))
           (when (member :unique-identifier p :key 'car)
             (setf (unique-identifier new-customer) (@ p :unique-identifier)))
           (when (member :gender p :key 'car)
             (setf (gender new-customer) (@ p :gender)))
           (when (member :curator-p p :key 'car)
             (setf (curator-p new-customer) (@ p :curator-p)))
           (when (member :visible-in-search p :key 'car)
             (setf (visible-in-search new-customer) (@ p :visible-in-search)))
           (when (member :show-my-circles p :key 'car)
             (setf (show-my-circles new-customer) (@ p :show-my-circles)))
           (save new-customer)
           (if (visible-in-search new-customer)
               (add-to-search-index new-customer)
               (ignore-errors
                 (delete-from-search-index customer))))
      (when (rw-lock-p lock)
        (release-write-lock lock)))
    (when (or (@ p :address-1)
              (@ p :address-2)
              (@ p :city)
              (@ p :state)
              (@ p :zip)
              (@ p :country))
      (update-address new-customer p))
    new-customer))

(defmethod grant-curator-access ((customer customer))
  (with-transaction ()
    (let ((new-customer (copy customer)))
      (setf (curator-p new-customer) t)
      (save new-customer))))

(defmethod delete-customer ((customer customer))
  (ignore-errors
    (delete-from-search-index customer))
  (mark-deleted customer))

(defun list-customers ()
  (select (:flat t) (?c) (is-a ?c customer)))

(defmethod products ((customer customer) &key limit skip &allow-other-keys)
  (declare (special customer))
  (select (:limit limit :skip skip :flat t)
          (?product)
          (lisp ?customer customer)
          (subscribed-to ?customer ?product)))

(defmethod get-product-subscription ((customer customer) (product product))
  (declare (special customer product))
  (select-one (?sub)
              (lisp ?customer customer)
              (lisp ?product product)
              (incoming-edges ?product subscribed-to ?sub ?customer)))

(defmethod register-customer-for-product ((customer customer) (product product))
  (with-write-locked-customer (customer)
    (let ((sub (get-product-subscription customer product)))
      (if sub
          sub
          (make-subscribed-to :from customer :to product :date-added (now))))))

(defmethod deregister-customer-for-product ((customer customer) (product product))
  (declare (special customer product))
  (with-write-locked-customer (customer)
    (do-query
      (lisp ?customer customer)
      (lisp ?product product)
      (retract subscribed-to ?customer ?product))))

(defun search-customers (&key email full-name lat long (miles 10) (num-docs 30)
                         first-doc exclude-list)
  "Search using elasticsearch"
  (if exclude-list
      (let ((query
             (with-output-to-string (q)
               (format q "{\"fields\":[],")
               (format q "\"query\":{\"bool\":{")
               (when full-name
                 (format q "\"must\":{\"match\":{\"FULL-NAME\":\"~A\"}}," full-name))
               (when email
                 (format q "\"must\":{\"match\":{\"EMAIL\":\"~A\"}}," email))
               (format q "~{~A~^,~}"
                       (mapcar (lambda (c)
                                 (format nil "\"must_not\":{\"match\":{\"EMAIL\":\"~A\"}}"
                                         (if (customer-p c)
                                             (email c)
                                             c)))
                               exclude-list))
               ;; FIXME: add filters for geo_distance
               (format q "}}}"))))
        (log:debug "CUSTOMER-SEARCH: ~A" query)
        (mapcar (lambda (r)
                  (lookup-customer (@ r :|_id|)))
                (free-form-search query
                                  "customer" "social-shopping"
                                  :size num-docs :from first-doc)))
      (let ((query nil))
        (when full-name
          (push `(:full-name . ,(string-downcase full-name)) query))
        (when email
          (push `(:email . ,email) query))
        (mapcar 'lookup-customer
                (es-search query
                           "customer" "social-shopping"
                           :size num-docs
                           :from first-doc
                           :return-ids? t
                           :no-fields? t
                           :filter (when (and lat long)
                                     `(("geo_distance"
                                        (("distance"
                                          . ,(format nil "~Fmi" (or miles 10)))
                                         ("customer.LOCATION"
                                          (("lat" . ,lat)
                                           ("lon" . ,long))))))))))))
