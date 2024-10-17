(in-package :social-shopping)

(defvar *collection-lock-count* 10000)
(defvar *collection-locks* (map-into (make-array *collection-lock-count*) 'make-rw-lock))

(defun lookup-collection-lock (collection)
  (aref *collection-locks*
        (mod (sxhash (if (collection-p collection)
                         (string-id collection)
                         collection))
             *collection-lock-count*)))

(defun acquire-collection-lock (collection &key reading-p (kind :read))
  (let ((lock (lookup-collection-lock collection)))
    (if (eql kind :write)
        (acquire-write-lock lock :reading-p reading-p)
        (acquire-read-lock lock))
    lock))

(defmacro with-write-locked-collection ((collection &key reading-p)
                                        &body body)
  (with-gensyms (lock)
    `(let ((,lock (acquire-collection-lock ,collection
                                           :kind :write
                                           :reading-p ,reading-p)))
       (unwind-protect
            (progn ,@body)
         (release-write-lock ,lock)))))

(defmacro with-read-locked-collection ((collection) &body body)
  (with-gensyms (lock)
    `(let ((,lock (acquire-collection-lock ,collection :kind :read)))
       (unwind-protect
            (progn ,@body)
         (release-read-lock ,lock)))))

(defmethod print-object ((c collection) stream)
  (format stream "'COLLECTION ~A: ~A'" (string-id c) (collection-name c)))

(json-rpc::def-json-rpc-encoding :collection-def (raw-val)
  (list :json
        (json-rpc::encode-json-to-string (node-to-alist raw-val))))

(json-rpc::def-json-rpc-encoding :collection-defs (raw-val)
  (list :json
        (json-rpc::encode-json-to-string
         (mapcar 'node-to-alist raw-val))))

(defmethod collection-exists-p ((customer customer) (name string))
  (declare (special customer name))
  (select-one (?collection)
              (lisp ?customer customer)
              (lisp ?name name)
              (created-by ?collection ?customer)
              (node-slot-value ?collection inactive-p ?inactive-p)
              (= ?inactive-p nil)
              (node-slot-value ?collection collection-name ?this-name)
              (= ?name ?this-name)))

(defmethod create-collection ((customer customer) (name string) &key
                              description public-p (indexed-p t))
  (with-write-locked-customer (customer)
    (let ((existing-collection (collection-exists-p customer name)))
      (if existing-collection
          (values existing-collection nil)
          (with-transaction ()
            (let ((collection (make-collection :collection-name name
                                               :indexed-p indexed-p
                                               :date-added (now)
                                               :description description
                                               :public-p public-p)))
              (make-created-by :from collection :to customer)
              (make-moderated-by :from collection :to customer)
              (make-participant-in :from customer :to collection)
              (when public-p
                (add-to-search-index collection)
                (record-collection-creation customer collection))
              (values collection t)))))))

(defmethod memberships ((collection collection) &key limit skip)
  (declare (special collection))
  (select (:limit limit :skip skip :flat t)
          (?membership)
          (lisp ?collection collection)
          (member-of ?membership ?collection)))

(defmethod items-in ((collection collection) &key limit skip)
  (declare (special collection))
  (select (:limit limit :skip skip :flat t)
          (?item)
          (lisp ?collection collection)
          (member-of ?membership ?collection)
          (has-membership ?item ?membership)))

(defmethod get-item-ids ((collection collection) &key limit skip)
  (mapcar 'id (items-in collection :limit limit :skip skip)))

(defmethod clone ((collection collection) (customer customer) &key
                  name description public-p (indexed-p t))
  (if (collection-exists-p customer
                           (or name (collection-name collection)))
      (error "Cannot clone using ~A as name;  customer already has a collection by that name."
             (or name (collection-name collection)))
      (with-transaction ()
        (let ((clone (create-collection
                      customer
                      (or name (collection-name collection))
                      :indexed-p indexed-p
                      :public-p public-p
                      :description description)))
          (dolist (item (items-in collection))
            (add-to-collection customer item clone :record-in-feed-p nil))
          (make-cloned-from :from clone :to collection)
          (record-collection-cloning collection clone)
          (when public-p
            (add-to-search-index clone))
          clone))))

(defmethod owner ((collection collection))
  (declare (special collection))
  (select-one (?owner)
              (lisp ?collection collection)
              (created-by ?collection ?owner)))

(defmethod customer ((collection collection))
  (owner collection))

(defmethod list-collections ((customer customer) &key skip limit)
  (declare (special customer))
  (select (:limit limit :skip skip :flat t)
          (?collection)
          (lisp ?customer customer)
          (created-by ?collection ?customer)
          (node-slot-value ?collection inactive-p ?inactive-p)
          (= ?inactive-p nil)))

(defmethod list-inactive-collections ((customer customer) &key skip limit)
  (declare (special customer))
  (select (:limit limit :skip skip :flat t)
          (?collection)
          (lisp ?customer customer)
          (created-by ?collection ?customer)
          (node-slot-value ?collection inactive-p ?inactive-p)
          (= ?inactive-p t)))

(defmethod update-collection ((collection collection) &key
                              name description public-p indexed-p)
  (let ((customer (owner collection)))
    (with-write-locked-customer (customer)
      (with-transaction ()
        (let ((new-collection (copy collection)))
          (setf (indexed-p new-collection) indexed-p)
          (when description
            (setf (description new-collection) description))
          (when name
            (setf (collection-name new-collection) name))
          (setf (public-p new-collection) public-p)
          (save new-collection)
          (when (public-p new-collection)
            (add-to-search-index new-collection))
          new-collection)))))

(defmethod update-collection ((customer customer) &key
                              name description public-p indexed-p)
  (with-write-locked-customer (customer)
    (let ((collection (collection-exists-p customer name)))
      (if (collection-p collection)
          (update-collection collection
                             :name name
                             :description description
                             :indexed-p indexed-p
                             :public-p public-p)
          (error "Unknown collection ~A" name)))))

(defmethod already-viewable-p ((circle circle) (collection collection))
  (declare (special circle collection))
  (select-one
   (?can-view)
   (lisp ?circle circle)
   (lisp ?collection collection)
   (outgoing-edges ?circle participant-in ?can-view ?collection)))

(defmethod invite-to-collection ((customer customer) (circle circle)
                                 (collection collection))
  (with-write-locked-collection (collection)
    (or (already-viewable-p circle collection)
        (prog1
            (make-participant-in :from circle :to collection)
          (record-collection-invitation collection circle)))))

(defmethod invite-to-collection ((customer customer) (circle circle)
                                 (collection-name string))
  (let ((collection (collection-exists-p customer collection-name)))
    (if (collection-p collection)
        (with-write-locked-collection (collection)
          (or (already-viewable-p circle collection)
              (prog1
                  (make-participant-in :from circle :to collection)
                (record-collection-invitation collection circle))))
        (error "Unknown collection '~A'" collection-name))))

(defmethod invite-to-collection ((customer customer) (circle-name string)
                                 (collection-name string))
  (let ((circle (lookup-customer-circle customer circle-name)))
    (if circle
        (invite-to-collection customer circle collection-name)
        (error "Unknown circle '~A'" circle-name))))

(defmethod send-collection-invite-msg ((collection collection) (customer customer))
  (let ((owner (owner collection)))
    (create-message (id-to-string *srn-var-id*)
                    (string-id customer)
                    "You've been invited!"
                    (with-output-to-string (out)
                      (format out "You have been invited to view")
                      (if (first-name owner)
                          (format out " ~A's " (first-name owner))
                          (format out " a "))
                      (format out "collection <a href=\"/collection/~A/~A\"\>\"~A\"</a>."
                              (string-id collection)
                              (hunchentoot:url-encode (collection-name collection))
                              (collection-name collection))))))

(defmethod uninvite-from-collection ((customer customer)
                                     (circle-name string)
                                     (collection collection))
  (with-write-locked-collection (collection)
    (let ((circle (lookup-customer-circle customer circle-name)))
      (if circle
          (let ((edge (already-viewable-p circle collection)))
            (when edge
              (mark-deleted edge)))
          (error "Unknown circle '~A'" circle-name)))))

(defmethod uninvite-from-collection ((customer customer)
                                     (circle-name string)
                                     (collection-name string))
  (let ((circle (lookup-customer-circle customer circle-name)))
    (if circle
        (let ((collection (collection-exists-p customer collection-name)))
          (if (collection-p collection)
              (with-write-locked-collection (collection)
                (let ((edge (already-viewable-p circle collection)))
                  (when edge
                    (mark-deleted edge))))
              (error "Unknown collection '~A'" collection-name)))
        (error "Unknown circle '~A'" circle-name))))

(defmethod already-viewable-p ((other customer) (collection collection))
  (declare (special other collection))
  (select-one
   (?can-view)
   (lisp ?other other)
   (lisp ?collection collection)
   (outgoing-edges ?other participant-in ?can-view ?collection)))

(defmethod invite-to-collection ((customer customer) (other customer)
                                 (collection collection))
  (with-write-locked-collection (collection)
    (or (already-viewable-p other collection)
        (prog1
            (make-participant-in :from other :to collection)
          (record-collection-invitation collection other)))))

(defmethod invite-to-collection ((customer customer) (other customer)
                                 (collection-name string))
  (let ((collection (collection-exists-p customer collection-name)))
    (if (collection-p collection)
        (with-write-locked-collection (collection)
          (or (already-viewable-p other collection)
              (prog1
                  (make-participant-in :from other :to collection)
                (record-collection-invitation collection other))))
        (error "Unknown collection '~A'" collection-name))))

(defmethod uninvite-from-collection ((customer customer)
                                     (invitee customer)
                                     (collection collection))
  (with-write-locked-collection (collection)
    (let ((edge (already-viewable-p invitee collection)))
      (when edge
        (mark-deleted edge)))))

(def-global-prolog-functor participant-in-collection/2
    (customer collection cont)
  (setq customer (var-deref customer)
        collection (var-deref collection))
  (if (customer-p customer)
      (let ((table (make-hash-table :test 'node-equal)))
        ;; Check for personal invitations
        (map-edges (lambda (edge)
                     (let ((this-collection (lookup-collection (to edge))))
                       (unless (gethash this-collection table)
                         (let ((old-trail (fill-pointer *trail*)))
                           (when (unify this-collection collection)
                             (setf (gethash this-collection table) t)
                             (funcall cont))
                           (undo-bindings old-trail)))))
                   *graph*
                   :edge-type 'participant-in
                   :vertex customer
                   :direction :out)
        ;; Check for group invitations
        (map-edges
         (lambda (circle-edge)
           (let ((circle (lookup-circle (to circle-edge))))
             (map-edges (lambda (edge)
                          (let ((this-collection (lookup-collection (to edge))))
                            (unless (gethash this-collection table)
                              (let ((old-trail (fill-pointer *trail*)))
                                (when (unify this-collection collection)
                                  (setf (gethash this-collection table) t)
                                  (funcall cont))
                                (undo-bindings old-trail)))))
                        *graph*
                        :edge-type 'participant-in
                        :vertex circle
                        :direction :out)))
         *graph*
         :edge-type 'in-circle
         :vertex customer
         :direction :out))
      ;; FIXME: iterate over all customers if customer is not specified.
      ))

(defmethod customer-can-view-p ((other customer) (collection collection))
  (declare (special other collection))
  (when (or (select-one
             (?can-view)
             (lisp ?other other)
             (lisp ?collection collection)
             (outgoing-edges ?other participant-in ?can-view ?collection))
            (select-one
             (?can-view)
             (lisp ?other other)
             (lisp ?collection collection)
             (in-circle ?other ?circle)
             (outgoing-edges ?circle
                             participant-in
                             ?can-view
                             ?collection)))
    t))

(defmethod list-all-invitations ((collection collection) &key limit skip)
  (declare (special collection))
  (select (:flat t :limit limit :skip skip)
          (?customer-or-circle)
          (lisp ?collection collection)
          (participant-in ?customer-or-circle ?collection)))

(defmethod list-invited-customers ((collection collection) &key limit skip)
  (declare (special collection))
  (select (:flat t :limit limit :skip skip)
          (?customer)
          (lisp ?collection collection)
          (participant-in ?customer ?collection)
          (is-a ?customer customer)))

(defmethod list-invited-circles ((collection collection) &key limit skip)
  (declare (special collection))
  (select (:flat t :limit limit :skip skip)
          (?circle)
          (lisp ?collection collection)
          (participant-in ?circle ?collection)
          (is-a ?circle circle)))

(defmethod list-collections-to-which-customer-is-invited
    ((customer customer) &key skip limit)
  (declare (special customer))
  (select (:flat t :limit limit :skip skip)
          (?collection)
          (lisp ?customer customer)
          (participant-in-collection ?customer ?collection)
          (unique ?collection)
          (node-slot-value ?collection inactive-p ?inactive-p)
          (= ?inactive-p nil)
          (not (created-by ?collection ?customer))))

(defmethod list-collections-with-invitations ((customer customer) &key
                                              skip limit)
  (declare (special customer))
  (select (:limit limit :skip skip)
          (?collection ?circle-or-customer)
          (lisp ?customer customer)
          (created-by ?collection ?customer)
          (node-slot-value ?collection inactive-p ?inactive-p)
          (= ?inactive-p nil)
          (participant-in ?circle-or-customer ?collection)
          (not (= ?circle-or-customer ?customer))))

(defmethod list-public-collections ((customer customer) &key skip limit)
  (declare (special customer))
  (select (:limit limit :skip skip :flat t)
          (?collection)
          (lisp ?customer customer)
          (created-by ?collection ?customer)
          (node-slot-value ?collection inactive-p ?inactive-p)
          (= ?inactive-p nil)
          (node-slot-value ?collection public-p ?public-p)
          (= ?public-p t)))

(defmethod collected-count ((product collectable))
  (let ((key (string-id product)))
    (or
     (@ (invoke-graph-view 'has-membership
                           'collected-count
                           :group-p t :key key)
        :value)
     0)))

(defmethod who-collected ((product collectable) &key
                          skip limit (public-p t))
  (declare (special product))
  (if public-p
      (select (:skip skip :limit limit :flat t)
              (?customer)
              (lisp ?product product)
              (has-membership ?product ?membership)
              (member-of ?membership ?collection)
              (node-slot-value ?collection inactive-p ?inactive-p)
              (= ?inactive-p nil)
              (added-by ?membership ?customer)
              (unique ?customer)
              (node-slot-value ?customer visible-in-search ?visible)
              (= ?visible t))
      (select (:skip skip :limit limit :flat t)
              (?customer)
              (lisp ?product product)
              (has-membership ?product ?membership)
              (member-of ?membership ?collection)
              (added-by ?membership ?customer)
              (unique ?customer))))

(defmethod showcased-item ((collection collection))
  (declare (special collection))
  (or (select-one (?product)
                  (lisp ?collection collection)
                  (is-showcased ?membership ?collection)
                  (has-membership ?product ?membership))
      (first (items-in collection :limit 1))))

(defmethod add-showcased-item ((collection collection) (item collectable))
  (declare (special collection))
  (let ((membership (item-in-collection-p collection item)))
    (if (membership-p membership)
        (let ((current-hero-shot-edge
               (select-one (?is-showcased)
                           (lisp ?collection collection)
                           (outgoing-edges ?membership is-showcased
                                           ?is-showcased ?collection))))
          (when current-hero-shot-edge
            (mark-deleted current-hero-shot-edge))
          (make-is-showcased :from membership :to collection))
        (error "~A is not in this collection." item))))

(defmethod collections ((product collectable) &key limit skip only-public-p)
  (declare (special product))
  (if only-public-p
      (select (:limit limit :skip skip :flat t)
              (?collection)
              (lisp ?product product)
              (has-membership ?product ?membership)
              (member-of ?membership ?collection)
              (node-slot-value ?collection inactive-p ?inactive-p)
              (= ?inactive-p nil)
              (node-slot-value ?collection public-p ?public-p)
              (= ?public-p t))
      (select (:limit limit :skip skip :flat t)
              (?collection)
              (lisp ?product product)
              (has-membership ?product ?membership)
              (member-of ?membership ?collection)
              (node-slot-value ?collection inactive-p ?inactive-p)
              (= ?inactive-p nil))))

(defmethod trending-collections (&key skip limit)
  ;; FIXME: pre-compute this using statistics!
  (select (:limit limit :skip skip :flat t)
          (?collection)
          (is-a ?collection collection)
          (node-slot-value ?collection inactive-p ?inactive-p)
          (= ?inactive-p nil)
          (node-slot-value ?collection public-p ?public-p)
          (= ?public-p t)))

(defmethod trending-collections-wall (&key skip limit)
  ;; FIXME: pre-compute this using statistics!
  (map-query (lambda (collection)
               (let ((showcased-item (showcased-item collection)))
                 (list (string-id collection)
                       (collection-name collection)
                       (if showcased-item
                           (string-id showcased-item)
                           nil))))
             (select (:limit limit :skip skip)
                     (?collection)
                     (is-a ?collection collection)
                     (node-slot-value ?collection inactive-p ?inactive-p)
                     (= ?inactive-p nil)
                     (node-slot-value ?collection public-p ?public-p)
                     (= ?public-p t))
             :collect-p t))

(defmethod collection-wall ((collection collection) (_ null) &key
                            limit skip)
  (declare (special collection) (ignore _))
  (map-query 'offer-wall-view
             (select (:limit limit :skip skip)
                     (?item)
                     (lisp ?collection collection)
                     (member-of ?membership ?collection)
                     (has-membership ?item ?membership))
             :collect-p t))

(defmethod collection-wall ((collection collection) (customer customer)
                            &key limit skip)
  (declare (ignore customer))
  (declare (special collection))
  (map-query 'offer-wall-view
             (select (:limit limit :skip skip)
                     (?item)
                     (lisp ?collection collection)
                     (member-of ?membership ?collection)
                     (has-membership ?item ?membership))
             :collect-p t))

(defmethod collection-wall ((customer customer) (collection-name string)
                            &key limit skip)
  (let ((collection (collection-exists-p customer collection-name)))
    (when (collection-p collection)
      (collection-wall collection customer :limit limit :skip skip))))

(defmethod collection-length ((collection collection))
  (or (@ (invoke-graph-view 'member-of
                            'list-length
                            :group-p t
                            :key (string-id collection))
         :value)
      0))

(defmethod filter-collection ((collection collection) &key
                              skip (limit nil) price-low price-high
                              gender taxon)
  (declare (special collection gender price-low price-high taxon))
  (unless gender (setq gender :any))
  (if (equalp gender "neuter") (setq gender :any))
  (cond ((null taxon)
         (setq taxon :any))
        ((stringp taxon)
         (setq taxon (lookup-taxon-by-key taxon))))
  (unless price-low (setq price-low 0))
  (unless price-high (setq price-high 100000000))
  (select (:limit limit :skip skip :flat t)
          (?product)
          (lisp ?collection collection)
          (lisp ?preferred-taxon taxon)
          (lisp ?gender gender)
          (lisp ?price-low price-low)
          (lisp ?price-high price-high)
          (member-of ?membership ?collection)
          (has-membership ?product ?membership)
          (gender-match ?product ?gender)
          (taxon-match ?product ?preferred-taxon)
          (price-between ?product ?price-low ?price-high)))

;          (node-slot-value ?product price ?price)
;          (<= ?price ?price-high)
;          (>= ?price ?price-low)
;
;          (has-taxon ?product ?taxon)
;          (node-slot-value ?taxon key ?taxon-name)
;          (or (= ?preferred-taxon :any)
;              (= ?taxon-name ?preferred-taxon))
;          (unique ?product)))

(defmethod item-in-collection-p ((collection collection)
                                 (product collectable)
                                 &key &allow-other-keys)
  (declare (special product collection))
  (select-one (?membership)
              (lisp ?product product)
              (lisp ?collection collection)
              (member-of ?membership ?collection)
              (has-membership ?product ?membership)))

(defmethod item-in-collection-p ((customer customer) (id string)
                                 &key collection-name)
  (let ((colelctable (lookup-collectable id))
        (collection (collection-exists-p customer collection-name)))
    (when (and colelctable collection)
      (item-in-collection-p collection colelctable))))

(defmethod in-which-collections ((customer customer)
                                 (product collectable))
  (declare (special customer product))
  (select-flat (?collection)
               (lisp ?c customer)
               (lisp ?product product)
               (participant-in-collection ?c ?collection)
               (node-slot-value ?collection inactive-p ?inactive-p)
               (= ?inactive-p nil)
               (member-of ?membership ?collection)
               (has-membership ?product ?membership)))

(defmethod in-which-collections ((customer customer) (id string))
  (let ((item (lookup-collectable id)))
    (when item
      (in-which-collections customer item))))

(defmethod add-to-collection ((customer customer) (product collectable)
                              (collection collection) &key (record-in-feed-p t))
  (with-write-locked-collection (collection)
    (or (values (item-in-collection-p collection product) nil)
        (with-transaction ()
          (let ((membership (make-membership :date-added (now))))
            (make-has-membership :from product :to membership)
            (make-member-of :from membership :to collection)
            (make-added-by :from membership :to customer)
            (when record-in-feed-p
              (ignore-errors
                (record-collection-addition collection product customer)))
            (values membership t))))))

(defmethod remove-from-collection ((customer customer)
                                   (product collectable)
                                   (collection collection))
  (declare (special collection product))
  (with-write-locked-collection (collection)
    (let ((quad
           (select-first
            (?has-membership ?membership ?member-of ?added-by)
            (lisp ?product product)
            (lisp ?collection collection)
            (incoming-edges ?collection member-of ?member-of ?membership)
            (incoming-edges ?membership
                            has-membership
                            ?has-membership
                            ?product)
            (outgoing-edges ?membership added-by ?added-by ?c2))))
      (when quad
        (with-transaction ()
          (map nil 'mark-deleted quad))
        t))))

(defmethod delete-collection ((collection collection))
  (with-write-locked-collection (collection)
    ;; FIXME: cascade
    ;; FIXME: de-index the collection if indexed-p
    (mark-deleted collection)))

(defmethod deactivate-collection ((collection collection))
  (with-write-locked-collection (collection)
    (with-transaction ()
      (let ((c (copy collection)))
        (setf (inactive-p c) t)
        ;; FIXME: de-index the collection if indexed-p
        (save c)))))

(defmethod reactivate-collection ((collection collection))
  (with-write-locked-collection (collection)
    (when (inactive-p collection)
      (with-transaction ()
        (let ((c (copy collection)))
          (setf (inactive-p c) nil)
          ;; FIXME: re-index the collection if indexed-p
          (save c))))))

(defun search-collections (&key description num-docs first-doc wall-of-deals-p sort)
  (let ((query
         (with-output-to-string (q)
           (format q "{")
           (format q "\"fields\":[],")
           (if sort
               (format q "\"sort\":~A," sort)
               (format q "\"sort\":[~{\"~A\"~^,~}]," (list "_score")))
           (format q "\"query\":{\"bool\":{")
           (let ((clauses nil))
             (push
              (format nil "\"must\":{\"match\":{\"SEARCHABLE-DESCRIPTION\":\"~A\"}}"
                      (format nil "~{~A~^ ~}" (normalize-and-split description)))
              clauses)
             (format q "~{~A~^,~}" clauses)
             (format q "}}}")))))
    (log:debug "~A" query)
    (multiple-value-bind (results hits)
        (free-form-search query "collection" "social-shopping" :size num-docs :from first-doc)
      (values
       (mapcar (lambda (r)
                 (let ((fields (@ r :|fields|)))
                   (let ((id (@ r :|_id|)))
                     (let ((collection (lookup-collection id)))
                       (if wall-of-deals-p
                           (offer-wall-view collection)
                           collection)))))
               results)
       hits))))
