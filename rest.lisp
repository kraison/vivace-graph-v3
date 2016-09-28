(in-package :graph-db)

(defvar *rest-port* 8080)
(defvar *rest-app* nil)
(defvar *clack-app* nil)

(defmacro with-rest-graph ((graph-name) &body body)
  `(let ((*graph* (lookup-graph (intern (json:camel-case-to-lisp ,graph-name) :keyword))))
     (if (graph-p *graph*)
         (progn
           ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list
             (cons :error
                   (format nil "Unknown graph ~A" ,graph-name))))))))

(defmacro with-rest-vertex ((id) &body body)
  `(let ((vertex (lookup-vertex ,id)))
     (if vertex
         (progn
           ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list
             (cons :error
                   (format nil "Unknown vertex ~A" ,id))))))))

(defmacro with-rest-edge ((id) &body body)
  `(let ((edge (lookup-edge ,id)))
     (if edge
         (progn
           ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list
             (cons :error
                   (format nil "Unknown edge ~A" ,id))))))))

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
  (with-rest-graph ((get-param params :graph-name))
    (json-encode *graph*)))

(defun rest-get-vertex (params)
  (with-rest-graph ((get-param params :graph-name))
    (with-rest-vertex ((get-param params :node-id))
      (json-encode vertex))))

(defun rest-post-vertex (params)
  (with-rest-graph ((get-param params :graph-name))
    (let* ((type-name (get-param params :type))
           (type (lookup-node-type-by-name
                  (intern (json:camel-case-to-lisp type-name) :keyword)
                  :vertex
                  :graph *graph*)))
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
                  (format nil "Unknown vertex type ~A" type-name))))))))

(defun rest-put-vertex (params)
  (with-rest-graph ((get-param params :graph-name))
    (with-rest-vertex ((get-param params :node-id))
      (with-transaction ()
        (let ((slots (data-slots (class-of vertex)))
              (new-vertex (copy vertex)))
          (dolist (slot slots)
            (let ((kv (assoc (json:lisp-to-camel-case
                              (symbol-name slot))
                             params
                             :test 'string-equal)))
              (when kv
                (setf (slot-value new-vertex slot)
                      (cdr kv)))))
          (json-encode (save new-vertex)))))))

(defun rest-delete-vertex (params)
  (with-rest-graph ((get-param params :graph-name))
    (with-rest-vertex ((get-param params :node-id))
      (mark-deleted vertex))))

(defun rest-get-edge (params)
  (with-rest-graph ((get-param params :graph-name))
    (with-rest-edge ((get-param params :node-id))
      (json-encode edge))))

(defun rest-post-edge (params)
  (with-rest-graph ((get-param params :graph-name))
    (let* ((type-name (get-param params :type))
           (type (lookup-node-type-by-name
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
                  (format nil "Unknown edge type ~A" type-name))))))))

(defun rest-put-edge (params)
  (with-rest-graph ((get-param params :graph-name))
    (with-rest-edge ((get-param params :node-id))
      (with-transaction ()
        (let ((slots (data-slots (class-of edge)))
              (new-edge (copy edge)))
          (dolist (slot slots)
            (let ((kv (assoc (json:lisp-to-camel-case
                              (symbol-name slot))
                             params
                             :test 'string-equal)))
              (when kv
                (setf (slot-value new-edge slot)
                      (cdr kv)))))
          (json-encode (save new-edge)))))))

(defun rest-delete-edge (params)
  (with-rest-graph ((get-param params :graph-name))
    (with-rest-edge ((get-param params :node-id))
      (mark-deleted edge))))

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

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :put)
          'rest-put-vertex

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :get)
          'rest-get-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :delete)
          'rest-delete-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:type" :method :post)
          'rest-post-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :put)
          'rest-put-edge)

    (setq *clack-app* (clack:clackup *rest-app* :port port))))

(defun stop-rest (&optional (app *clack-app*))
  (clack:stop app))
