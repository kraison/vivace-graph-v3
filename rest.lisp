(in-package :graph-db)

(defvar *rest-port* 8080)
(defvar *rest-app* nil)
(defvar *clack-app* nil)

(defmethod json-encode ((edge edge))
  (let* ((class (class-of edge))
         (class-name (class-name class)))
    (json-rpc::encode-json-to-string
     (nconc
      (list (cons :id (string-id edge))
            (cons :type (json:lisp-to-camel-case
                         (symbol-name class-name)))
            (cons :from (string-id (from edge)))
            (cons :to (string-id (to edge))))
      (mapcar (lambda (slot-name)
                (cons slot-name (slot-value edge slot-name)))
              (data-slots (find-class class-name)))))))

(defmethod json-encode ((vertex vertex))
  (let* ((class (class-of vertex))
         (class-name (class-name class)))
    (json-rpc::encode-json-to-string
     (nconc
      (list (cons :id (string-id vertex))
            (cons :type (json:lisp-to-camel-case
                         (symbol-name class-name))))
      (mapcar (lambda (slot-name)
                (cons slot-name (slot-value vertex slot-name)))
              (data-slots (find-class class-name)))))))

(defmethod json-encode ((graph graph))
  (json-rpc::encode-json-to-string
   (list
    (cons :name (graph-name graph))
    (cons :type :graph)
    (cons :mode (if (typep graph 'slave-graph)
                    :read-only
                    :read-write))
    (cons :vertex-types
          (loop
             for key being the hash-keys
             using (hash-value type-definition)
             in (gethash :vertex (schema-type-table (schema graph)))
             if (numberp key)
             collecting
               (list (cons :name (node-type-name type-definition))
                     (cons :slots
                           (mapcar
                            (lambda (slot-def)
                              (list (cons :name (first slot-def))
                                    (cons :type
                                          (let ((type-pos (position :type slot-def)))
                                            (if type-pos
                                                (nth (1+ type-pos) slot-def)
                                                :any)))))
                            (node-type-slots type-definition))))))
    (cons :edge-types
          (loop
             for key being the hash-keys
             using (hash-value type-definition)
             in (gethash :edge (schema-type-table (schema graph)))
             if (numberp key)
             collecting
               (list (cons :name (node-type-name type-definition))
                     (cons :slots
                           (mapcar
                            (lambda (slot-def)
                              (list (cons :name (first slot-def))
                                    (cons :type
                                          (let ((type-pos (position :type slot-def)))
                                            (if type-pos
                                                (nth (1+ type-pos) slot-def)
                                                :any)))))
                            (node-type-slots type-definition)))))))))

(defun get-param (params name)
  (cdr (assoc name params)))

(defun rest-login (params)
  )

(defun rest-logout (params)
  )

(defun rest-get-graph (params)
  (let ((graph-name (get-param params :graph-name)))
    (if graph-name
        (let ((graph (lookup-graph
                      (intern (json:camel-case-to-lisp graph-name) :keyword))))
          (if (graph-p graph)
              (json-encode graph)
              (progn
                (json:encode-json-to-string
                 (list
                  (cons :error
                        (format nil "Unknown graph ~A" graph-name))))))))))

(defun rest-get-vertex (params)
  (let* ((graph-name (get-param params :graph-name))
         (node-id (get-param params :node-id))
         (graph (lookup-graph
                 (intern (json:camel-case-to-lisp graph-name) :keyword))))
    (if (graph-p graph)
        (let ((node (lookup-vertex node-id :graph graph)))
          (if node
              (json-encode node)
              (json:encode-json-to-string
               (list
                (cons :error
                      (format nil "Unknown vertex ~A" node-id))))))
        (json:encode-json-to-string
         (list
          (cons :error
                (format nil "Unknown graph ~A" graph-name)))))))

(defun rest-post-vertex (params)
  (let* ((graph-name (get-param params :graph-name))
         (graph (lookup-graph
                 (intern (json:camel-case-to-lisp graph-name) :keyword)))
         (type-name (get-param params :type)))
    (if (graph-p graph)
        (let ((type (lookup-node-type-by-name
                     (intern (json:camel-case-to-lisp type-name) :keyword)
                     :vertex
                     :graph graph)))
          (if type
              (let* ((class (find-class (node-type-name type)))
                     (slots (mapcar (lambda (slot)
                                      (intern (symbol-name slot) :keyword))
                                    (data-slots class)))
                     (constructor (node-type-constructor type))
                     (slot-args (flatten
                                 (mapcar (lambda (slot)
                                           (list slot
                                                 (cdr (assoc (json:lisp-to-camel-case
                                                              (symbol-name slot))
                                                             params
                                                             :test 'string-equal))))
                                         slots)))
                     (node (apply constructor slot-args)))
                (json-encode node))
              (json:encode-json-to-string
               (list
                (cons :error
                      (format nil "Unknown vertex type ~A" type-name))))))
        (json:encode-json-to-string
         (list
          (cons :error
                (format nil "Unknown graph ~A" graph-name)))))))

(defun rest-delete-vertex (params)
  (let* ((graph-name (get-param params :graph-name))
         (node-id (get-param params :node-id))
         (graph (lookup-graph
                 (intern (json:camel-case-to-lisp graph-name) :keyword))))
    (if (graph-p graph)
        (let ((node (lookup-vertex node-id :graph graph)))
          (if node
              (progn
                (mark-deleted node)
                (json:encode-json-to-string
                 (list (cons :success
                             (format nil "Deleted ~A" node-id)))))
              (json:encode-json-to-string
               (list
                (cons :error
                      (format nil "Unknown vertex ~A" node-id))))))
        (json:encode-json-to-string
         (list
          (cons :error
                (format nil "Unknown graph ~A" graph-name)))))))

(defun rest-get-edge (params)
  (let* ((graph-name (get-param params :graph-name))
         (node-id (get-param params :node-id))
         (graph (lookup-graph
                 (intern (json:camel-case-to-lisp graph-name) :keyword))))
    (if (graph-p graph)
        (let ((node (lookup-edge node-id :graph graph)))
          (if node
              (json-encode node)
              (json:encode-json-to-string
               (list
                (cons :error
                      (format nil "Unknown edge ~A" node-id))))))
        (json:encode-json-to-string
         (list
          (cons :error
                (format nil "Unknown graph ~A" graph-name)))))))

(defun rest-post-edge (params)
  (let* ((graph-name (get-param params :graph-name))
         (*graph* (lookup-graph
                   (intern (json:camel-case-to-lisp graph-name) :keyword)))
         (type-name (get-param params :type)))
    (if (graph-p *graph*)
        (let ((type (lookup-node-type-by-name
                     (intern (json:camel-case-to-lisp type-name) :keyword)
                     :edge
                     :graph *graph*)))
          (if type
              (let* ((class (find-class (node-type-name type)))
                     (slots (mapcar (lambda (slot)
                                      (intern (symbol-name slot) :keyword))
                                    (data-slots class)))
                     (constructor (node-type-constructor type))
                     (from (lookup-vertex (cdr (assoc "from" params :test 'string-equal))))
                     (to (lookup-vertex (cdr (assoc "to" params :test 'string-equal))))
                     (slot-args (flatten
                                 (mapcar (lambda (slot)
                                           (list slot
                                                 (cdr (assoc (json:lisp-to-camel-case
                                                              (symbol-name slot))
                                                             params
                                                             :test 'string-equal))))
                                         slots))))
                (if (and from to)
                    (let ((node (apply constructor :from from :to to slot-args)))
                      (json-encode node))
                    (json:encode-json-to-string
                     (list
                      (cons :error
                            "You must provide both FROM and TO vertices")))))
              (json:encode-json-to-string
               (list
                (cons :error
                      (format nil "Unknown edge type ~A" type-name))))))
        (json:encode-json-to-string
         (list
          (cons :error
                (format nil "Unknown graph ~A" graph-name)))))))

(defun rest-delete-edge (params)
  (let* ((graph-name (get-param params :graph-name))
         (node-id (get-param params :node-id))
         (graph (lookup-graph
                 (intern (json:camel-case-to-lisp graph-name) :keyword))))
    (if (graph-p graph)
        (let ((node (lookup-edge node-id :graph graph)))
          (if node
              (progn
                (mark-deleted node)
                (json:encode-json-to-string
                 (list (cons :success
                             (format nil "Deleted ~A" node-id)))))
              (json:encode-json-to-string
               (list
                (cons :error
                      (format nil "Unknown edge ~A" node-id))))))
        (json:encode-json-to-string
         (list
          (cons :error
                (format nil "Unknown graph ~A" graph-name)))))))

(defun start-rest (&optional (port *rest-port*))
  (prog1
      (setq *rest-app* (make-instance 'ningle:<app>))
    (setf (ningle:route *rest-app* "/login" :method :post)
          'rest-login

          (ningle:route *rest-app* "/logout" :method :post)
          'rest-logout

          (ningle:route *rest-app* "/graph/:graph-name" :method :get)
          'rest-get-graph

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :get)
          'rest-get-vertex

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :delete)
          'rest-delete-vertex

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:type" :method :post)
          'rest-post-vertex

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :get)
          'rest-get-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :delete)
          'rest-delete-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:type" :method :post)
          'rest-post-edge)

    (setq *clack-app* (clack:clackup *rest-app* :port port))))

(defun stop-rest (&optional (app *clack-app*))
  (clack:stop app))
