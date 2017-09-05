(in-package :graph-db)

(defvar *rest-port* 8080)
(defvar *rest-app* nil)
(defvar *clack-app* nil)
(defvar *rest-procedures*
  #+sbcl (make-hash-table :synchronized t :test 'equalp)
  #+ccl (make-hash-table :shared t :test 'equalp))
(defvar *rest-passwd-file* "rpasswd")
(defvar *htpasswd-bin* "/usr/bin/htpasswd")

(defun add-rest-user (username password)
  (trivial-shell:shell-command
   (format nil "~A -b ~A ~A ~A ~A"
           *htpasswd-bin*
           (if (probe-file *rest-passwd-file*) "" "-c")
           *rest-passwd-file*
           username
           password)))

(defun delete-rest-user (username)
  (trivial-shell:shell-command
   (format nil "~A -D ~A ~A"
           *htpasswd-bin*
           *rest-passwd-file*
           username)))

(defun auth-rest-user (username password)
  (with-open-file (stream *rest-passwd-file*)
    (do ((line (read-line stream nil :eof)
               (read-line stream nil :eof)))
        ((eql line :eof))
      (let ((pair (cl-ppcre:split "\\:" line)))
        (when (equalp (first pair) username)
          (destructuring-bind (_ hash salt enc)
              (cl-ppcre:split "\\$" (second pair))
            (declare (ignore _ enc))
            (multiple-value-bind (out err exit-code)
                (trivial-shell:shell-command
                 (format nil "openssl passwd -~A -salt ~A ~A"
                         hash salt password))
              (declare (ignore exit-code err))
              (setq out (cl-ppcre:regex-replace-all "\\s+$" out ""))
              (log:warn "GOT ~A FOR ~A" out (second pair))
              (return-from auth-rest-user
                (equalp out (second pair))))))))))

#|
  ;; This only works with newer versions of htpasswd;  see openssl version above
  (multiple-value-bind (out err exit-code)
      (trivial-shell:shell-command
       (format nil "~A -vb ~A ~A ~A"
               *htpasswd-bin*
               *rest-passwd-file*
               username
               password))
    (declare (ignore out err))
    (eq 0 exit-code)))
|#

(defmacro with-rest-auth ((username password) &body body)
  `(if (auth-rest-user ,username ,password)
       (progn
         ,@body)
       (progn
         (setf (lack.response:response-status ningle:*response*) 401)
         (json:encode-json-to-string
          (list
           (cons :error "Invalid credentials"))))))

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

(defmethod json-encode-edge-list (seq)
  (json-rpc::encode-json-to-string
   (map 'list
        (lambda (edge)
          (let* ((class (class-of edge))
                 (class-name (class-name class)))
            (nconc
             (list (cons :id (string-id edge))
                   (cons :type (json:lisp-to-camel-case
                                (symbol-name class-name)))
                   (cons :from (string-id (from edge)))
                   (cons :to (string-id (to edge))))
             (mapcar (lambda (slot-name)
                       (cons slot-name (slot-value edge slot-name)))
                     (data-slots (find-class class-name))))))
        seq)))

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
  (cdr (assoc name params :test 'equalp)))

(defmacro def-rest-procedure (name lambda-list &body body)
  (with-gensyms (params)
  `(let ((fn (lambda (,params)
               (let (,@(mapcar (lambda (var)
                                 (list var
                                       `(get-param ,params
                                                   ,(json:lisp-to-camel-case
                                                     (symbol-name var)))))
                               lambda-list))
                 (progn
                   ,@body)))))
     (setf (gethash ,name *rest-procedures*) fn)
     (setf (gethash ,(json:lisp-to-camel-case (symbol-name name)) *rest-procedures*) fn))))

(defun call-rest-procedure (name params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (let ((fn (gethash name *rest-procedures*)))
      (if (functionp fn)
          (with-rest-graph ((get-param params :graph-name))
            (funcall fn params))
          (progn
            (setf (lack.response:response-status ningle:*response*) 404)
            (json:encode-json-to-string
             (list
              (cons :error
                    (format nil "Unknown procedure '~A'" name)))))))))

(defun rest-get-graph (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (json-encode *graph*))))

(defun rest-get-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-vertex ((get-param params :node-id))
        (json-encode vertex)))))

(defun rest-post-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
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
                    (format nil "Unknown vertex type ~A" type-name)))))))))

(defun rest-put-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
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
            (json-encode (save new-vertex))))))))

(defun rest-delete-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-vertex ((get-param params :node-id))
        (mark-deleted vertex)))))

(defun rest-get-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-edge ((get-param params :node-id))
        (json-encode edge)))))

(defun rest-post-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
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
                    (format nil "Unknown edge type ~A" type-name)))))))))

(defun rest-put-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
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
            (json-encode (save new-edge))))))))

(defun rest-delete-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-edge ((get-param params :node-id))
        (mark-deleted edge)))))

(defun rest-list-edges (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-vertex ((get-param params :node-id))
        (json-encode-edge-list
         (nconc (map-edges 'identity *graph*
                           :direction :out
                           :collect-p t
                           :vertex vertex)
                (map-edges 'identity *graph*
                           :direction :in
                           :collect-p t
                           :vertex vertex)))))))

(defun start-rest (&optional (port *rest-port*))
  (prog1
      (setq *rest-app* (make-instance 'ningle:<app>))
    (setf (ningle:route *rest-app* "/graph/:graph-name" :method :get)
          'rest-get-graph

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :get)
          'rest-get-vertex

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id/edges" :method :get)
          'rest-list-edges

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
          'rest-put-edge

          (ningle:route *rest-app* "/graph/:graph-name/procedure/:procedure-name" :method :post)
          '(lambda (params)
            (call-rest-procedure procedure-name params)))

    (setq *clack-app* (clack:clackup *rest-app* :port port))))

(defun stop-rest (&optional (app *clack-app*))
  (clack:stop app)
  (setq *rest-app* nil))
