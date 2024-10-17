(in-package :social-shopping)

(defun load-graph-views ()
  (def-view email (merchant :social-shopping)
    (:map
     (lambda (merchant)
       (when (email merchant)
         (yield (email merchant) nil)))))

  (def-view company-name (merchant :social-shopping)
    (:map
     (lambda (merchant)
       (when (company-name merchant)
         (yield (company-name merchant) nil)))))

  (def-view email (customer :social-shopping)
    (:map
     (lambda (customer)
       (when (email customer)
         (yield (email customer) nil)))))

  (def-view username (customer :social-shopping)
    (:map
     (lambda (customer)
       (when (username customer)
         (yield (username customer) nil)))))

  (def-view unique-identifier (customer :social-shopping)
    (:map
     (lambda (customer)
       (when (unique-identifier customer)
         (yield (unique-identifier customer) nil)))))

  (def-view curator (customer :social-shopping)
    (:map
     (lambda (customer)
       (when (curator-p customer)
         (yield (email customer) nil)))))

  (def-view key (product-taxon :social-shopping)
    (:map
     (lambda (taxon)
       (when (key taxon)
         (yield (key taxon) nil)))))

  (def-view sku (product :social-shopping)
    (:map
     (lambda (product)
       (when (sku product)
         (yield (sku product) nil)))))

  (def-view rand (product :social-shopping)
    (:map
     (lambda (product)
       (declare (ignore product))
       (let ((state (make-random-state)))
         (yield (random 1.0 state) nil)))))

  (def-view rand (bookmarklet :social-shopping)
    (:map
     (lambda (b)
       (declare (ignore b))
       (let ((state (make-random-state)))
         (yield (random 1.0 state) nil)))))

  (def-view list-length (in-want-list :social-shopping)
    (:map
     (lambda (in-want-list)
       (let ((customer (to in-want-list)))
         (yield (string-id customer) 1))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view want-count (in-want-list :social-shopping)
    (:map
     (lambda (in-want-list)
       (let ((p (lookup-collectable (from in-want-list))))
         (yield (string-id p) 1))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view want-count-by-hour (in-want-list :social-shopping)
    (:map
     (lambda (in-want-list)
       (let ((p (lookup-collectable (from in-want-list))))
         (when (timestamp-p (date-added in-want-list))
           (let ((hour (timestamp-minimize-part (date-added in-want-list) :min)))
             (yield (format nil "~A~A~A"
                            (timestring hour)
                            #\Nul
                            (string-id p))
                    1))))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view collected-count-by-hour (has-membership :social-shopping)
    (:map
     (lambda (has-membership)
       (let ((p (lookup-collectable (from has-membership)))
             (membership (lookup-membership (to has-membership))))
         (when (timestamp-p (date-added membership))
           (let ((hour (timestamp-minimize-part (date-added membership) :min)))
             (yield (format nil "~A~A~A"
                            (timestring hour)
                            #\Nul
                            (string-id p))
                    1))))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view collected-count (has-membership :social-shopping)
    (:map
     (lambda (has-membership)
       (let ((p (lookup-collectable (from has-membership))))
         (yield (string-id p) 1))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view list-length (member-of :social-shopping)
    (:map
     (lambda (member-of)
       (let ((c (lookup-collection (to member-of))))
         (yield (string-id c) 1))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view count-by-hour (subscribed-to :social-shopping)
    (:map
     (lambda (subscribed-to)
       (let ((p (lookup-product (to subscribed-to))))
         (when (timestamp-p (date-added subscribed-to))
           (let ((hour (timestamp-minimize-part (date-added subscribed-to) :min)))
             (yield (format nil "~A~A~A"
                            (timestring hour)
                            #\Nul
                            (string-id p))
                    1))))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view circle-count (in-circle :social-shopping)
    (:map
     (lambda (in-circle)
       (let ((circle (lookup-circle (to in-circle))))
         (yield (string-id circle) 1))))
    (:reduce
     (lambda (keys values)
       (declare (ignore keys))
       (apply '+ values))))

  (def-view tag (hashtag :social-shopping)
    (:map
     (lambda (hashtag)
       (when (tag hashtag)
         (yield (tag hashtag) nil)))))

  (def-view tag (keyword-tag :social-shopping)
    (:map
     (lambda (keyword-tag)
       (when (tag keyword-tag)
         (yield (tag keyword-tag) nil)))))

  )
