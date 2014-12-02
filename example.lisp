(def-vertex customer ()
  ((first-name :type string)
   (middle-name :type string)
   (last-name :type string)
   (email :type email))
  :test-db)

(def-view email (customer :test-db)
  (:map
   (lambda (customer)
     (when (email customer)
       (yield (email customer) nil)))))

(defun lookup-customer-by-email (email)
  (let ((customers (invoke-graph-view 'customer 'email :key email)))
    (if customers
        (lookup-vertex (@ (first customers) :id))
        nil)))
