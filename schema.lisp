(in-package :graph-db)

(defstruct schema
  (lock (make-recursive-lock))
  (type-table
   #+sbcl (make-hash-table :test 'eql :synchronized t)
   #+ccl (make-hash-table :test 'eql :shared t)
   #+lispworks (make-hash-table :test 'eql :single-thread nil))
  (class-locks
   #+sbcl (make-hash-table :test 'eql :synchronized t)
   #+ccl (make-hash-table :test 'eql :shared t)
   #+lispworks (make-hash-table :test 'eql :single-thread nil))
  (next-edge-id 1 :type (unsigned-byte 16))
  (next-vertex-id 1 :type (unsigned-byte 16)))

(defstruct node-type
  name
  parent-type
  id
  graph-name
  slots
  package
  constructor)

(defgeneric instantiate-node-type (node-type-def graph))

(defmacro with-write-locked-class ((name graph) &body body)
  `(let ((rw-lock (gethash ,name (schema-class-locks (schema ,graph)))))
     (with-write-lock (rw-lock)
       ,@body)))

(defmacro with-read-locked-class ((name graph) &body body)
  `(let ((rw-lock (gethash ,name (schema-class-locks (schema ,graph)))))
     (with-read-lock (rw-lock)
       ,@body)))

(defun list-edge-types (&optional (graph *graph*))
  (nconc (list 0)
         (loop
            for key being the hash-keys
            in (gethash :edge (schema-type-table (schema graph)))
            if (numberp key)
            collecting key)))

(defun list-vertex-types (&optional (graph *graph*))
  (nconc (list 0)
         (loop
            for key being the hash-keys
            in (gethash :vertex (schema-type-table (schema graph)))
            if (numberp key)
            collecting key)))

(defmethod init-schema ((graph graph))
  (let ((schema (make-schema)))
    (setf (schema graph) schema)
    (setf (gethash :edge (schema-type-table (schema graph)))
          #+sbcl (make-hash-table :test 'eql :synchronized t)
          #+ccl (make-hash-table :test 'eql :shared t)
          #+lispworks (make-hash-table :test 'eql :single-thread nil))
    (setf (gethash :vertex (schema-type-table (schema graph)))
          #+sbcl (make-hash-table :test 'eql :synchronized t)
          #+ccl (make-hash-table :test 'eql :shared t)
          #+lispworks (make-hash-table :test 'eql :single-thread nil))
    (setf (gethash 'edge (schema-class-locks schema))
          (make-rw-lock))
    (setf (gethash 'vertex (schema-class-locks schema))
          (make-rw-lock))
    (slot-value graph 'schema)))

(defmethod save-schema ((schema schema) (graph graph))
  (with-recursive-lock-held ((schema-lock schema))
    (let ((schema-file (format nil "~A/schema.dat" (location graph))))
      (let ((locks (schema-class-locks schema))
            (schema-lock (schema-lock schema)))
        ;; (format t "Pre Locks: ~a, Schema-lock: ~a~%" (schema-class-locks schema)
        ;;         (schema-lock schema))
        ;; (setf (schema-class-locks schema) nil)
        ;; (setf (schema-lock schema) nil)
        (cl-store:store (schema graph) schema-file)
        ;; (setf (schema-lock schema) schema-lock)
        ;; (setf (schema-class-locks schema) locks)
        ;; (format t "Post Locks: ~a, Schema-lock: ~a~%" (schema-class-locks schema)
        ;;         (schema-lock schema))
        schema))))

(defmethod get-next-type-id ((schema schema) parent)
  (with-recursive-lock-held ((schema-lock schema))
    (cond ((or (eql parent :edge) (eql parent 'edge))
           (prog1
               (schema-next-edge-id schema)
             (incf (schema-next-edge-id schema))))
          ((or (eql parent :vertex) (eql parent 'vertex))
           (prog1
               (schema-next-vertex-id schema)
             (incf (schema-next-vertex-id schema))))
          (t (error "Unknown parent type ~S" parent)))))

(defmethod schema-string-representation ((schema schema))
  "Return a string representation of SCHEMA. Two schemas with the same
node structure will have EQUALP string representations. This is meant
for a quick test of replication compatibility, not guaranteed equality
testing."
  (with-output-to-string (stream)
    (loop with parent-alist = (alexandria:hash-table-alist
                               (schema-type-table schema))
          for (parent . table) in (sort parent-alist #'string<
                                        :key 'car)
          do
          (format stream "~A~%" parent)
          (loop with node-types = (remove-if-not #'node-type-p
                                                 (alexandria:hash-table-values table))
                for node-type in (sort node-types #'string<
                                       :key 'node-type-name)
                do
                (format stream "  ~A~%" (node-type-name node-type))
                (loop with slots = (mapcar 'first (node-type-slots node-type))
                      for slot in (sort slots #'string<)
                      do (format stream "   ~A~%" slot))))))

(defmethod schema-digest ((schema schema))
  "Return a digest of the string representation of SCHEMA. Used in
replication for a quick schema compatibility check."
  (with-output-to-string (stream)
    (map nil
         (lambda (octet)
           (format stream "~(~2,'0X~)" octet))
         (md5:md5sum-string (schema-string-representation schema)
                             :external-format :utf8))))

(defmethod all-node-types ((graph graph))
  (let ((types nil))
    (maphash (lambda (parent table)
               (push (intern (symbol-name parent)) types)
               (maphash (lambda (child ctable)
                          (declare (ignore ctable))
                          (when (and (not (keywordp child))
                                     (not (numberp child)))
                            (push child types)))
                        table))
             (schema-type-table (schema graph)))
    types))

(defun lookup-node-type-by-id (id parent &key (graph *graph*))
  (assert (and (integerp id) (>= id 0) (< id +max-node-types+)))
  (let ((meta (gethash id (gethash parent (schema-type-table (schema graph))))))
    meta))

(defun lookup-node-type-by-name (name parent &key (graph *graph*))
  (let ((id (gethash name (gethash parent (schema-type-table (schema graph))))))
    (when id
      (lookup-node-type-by-id id parent :graph graph))))

(defmethod update-node-type ((meta node-type) (graph graph))
  (setf (gethash (node-type-id meta)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        meta)
  (setf (gethash (node-type-name meta)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        (node-type-id meta))
  (setf (gethash (intern (symbol-name (node-type-name meta)) :keyword)
                 (gethash (node-type-parent-type meta)
                          (schema-type-table (schema graph))))
        (node-type-id meta))
  (finalize-inheritance (find-class (node-type-name meta)))
  (save-schema (schema graph) graph))

(defmacro def-node-type (name parent-types slot-specs graph-name)
  (with-gensyms (meta graph)
    (let* ((constructor (intern (format nil "MAKE-~A" name)))
           (predicate (intern (format nil "~A-P" name)))
           (lookup-fn (intern (format nil "LOOKUP-~A" name))))
      (setq slot-specs
            (mapcar (lambda (spec)
                      (let ((s1
                             (if (listp spec)
                                 (if (find :accessor spec)
                                     spec
                                     (append spec (list :accessor (first spec))))
                                 (list spec :accessor spec))))
                        (if (find :initarg s1)
                            s1
                            (append s1 (list :initarg (intern (symbol-name (first s1)) :keyword))))))
                    slot-specs))
      `(progn
         (defclass ,name (,@parent-types)
           (,@slot-specs)
           (:metaclass node-class))
         (let* ((,meta
                 (make-node-type
                  :name ',name
                  :parent-type
                  ',(intern (symbol-name (last1 parent-types)) :keyword)
                  :graph-name ',graph-name
                  :slots ',slot-specs
                  :package (package-name *package*)
                  :constructor ',constructor)))
           ;; FIXME: why is this necessary when inheriting from another node subclass?
           ;;(unless (class-finalized-p (find-class ',name))
           (finalize-inheritance (find-class ',name))
           ;;)
           (defun ,predicate (thing)
             (typep thing ',name))
           (defun ,lookup-fn (id &key include-deleted-p (graph *graph*))
             (let ((thing ,(if (eql (last1 parent-types) 'edge)
                               `(lookup-edge id :graph graph)
                               `(lookup-vertex id :graph graph))))
               (when (and (typep thing ',name)
                          (or include-deleted-p
                              (not (deleted-p thing))))
                 thing)))
           ,(let ((args (if (eql (last1 parent-types) 'edge)
                            '(&rest make-args
                              &key (graph *graph*) id deleted-p revision from to weight &allow-other-keys)
                            '(&rest make-args
                              &key (graph *graph*) id deleted-p revision &allow-other-keys))))
                 `(defun ,constructor ,args
                    (let ((slots (remove-if
                                  'null
                                  (mapcar
                                   (lambda (slot-name)
                                     (let ((key (intern (symbol-name slot-name) :keyword)))
                                       (let ((pos (position key make-args)))
                                         (when pos
                                           (cons key (nth (1+ pos) make-args))))))
                                   (data-slots (find-class ',name))))))
                      ,(if (eql (last1 parent-types) 'edge)
                           `(if (and (lookup-vertex (id from) :graph graph)
                                     (lookup-vertex (id to) :graph graph))
                                (make-edge (node-type-id
                                            (lookup-node-type-by-name ',name :edge :graph graph))
                                           from to weight
                                           slots ;(list ,@slots)
                                           :id id :revision revision :deleted-p deleted-p
                                           :graph graph)
                                (error (format nil "Node ~a not found in graph ~a" from graph)))
                           `(make-vertex (node-type-id
                                          (lookup-node-type-by-name ',name :vertex :graph graph))
                                         slots ;(list ,@slots)
                                         :id id :revision revision :deleted-p deleted-p
                                         :graph graph)))))
           ,(when (eql (last1 parent-types) 'edge)
                  (let ((functor-name (intern (format nil "~A/2" name))))
                    `(def-global-prolog-functor ,functor-name (from to cont)
                       (setq from (var-deref from)
                             to (var-deref to))
                       (when *prolog-trace*
                         (format t "TRACE: ~A(~S ~S)~%" ',functor-name from to))
                       (cond ((and (not (graph-db::var-p from)) (not (graph-db::var-p to)))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (funcall cont)))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :from-vertex from
                                         :to-vertex to
                                         :edge-type ',name))
                             ((not (graph-db::var-p from))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (to edge))))
                                               (when (unify to v2)
                                                 (funcall cont)))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex from
                                         :direction :out
                                         :edge-type ',name))
                             ((not (graph-db::var-p to))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (from edge))))
                                               (when (unify from v2)
                                                 (funcall cont)))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex to
                                         :direction :in
                                         :edge-type ',name))
                             (t
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (funcall cont)))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :edge-type ',name))))))
           ,(when (eql (last1 parent-types) 'edge)
                  (let ((functor-name (intern (format nil "~A/3" name))))
                    `(def-global-prolog-functor ,functor-name (from to weight cont)
                       (setq from (var-deref from)
                             to (var-deref to)
                             weight (var-deref weight))
                       (when *prolog-trace*
                         (format t "TRACE: ~A(~S ~S ~S)~%" ',functor-name from to weight))
                       (cond ((and (not (graph-db::var-p from)) (not (graph-db::var-p to)))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (when (unify weight (weight edge))
                                                       (funcall cont))))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :from-vertex from
                                         :to-vertex to
                                         :edge-type ',name))
                             ((not (graph-db::var-p from))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (to edge))))
                                               (when (unify to v2)
                                                 (when (unify weight (weight edge))
                                                   (funcall cont))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex from
                                         :direction :out
                                         :edge-type ',name))
                             ((not (graph-db::var-p to))
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v2 (lookup-vertex (from edge))))
                                               (when (unify from v2)
                                                 (when (unify weight (weight edge))
                                                   (funcall cont))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :vertex to
                                         :direction :in
                                         :edge-type ',name))
                             (t
                              (map-edges (lambda (edge)
                                           (let ((old-trail (fill-pointer *trail*)))
                                             (let ((v1 (lookup-vertex (from edge))))
                                               (when (unify from v1)
                                                 (let ((v2 (lookup-vertex (to edge))))
                                                   (when (unify to v2)
                                                     (when (unify weight (weight edge))
                                                       (funcall cont))))))
                                             (undo-bindings old-trail)))
                                         *graph*
                                         :edge-type ',name)))))
                  )
           (push ,meta (gethash ',graph-name *schema-node-metadata*))
           (let ((,graph (lookup-graph ',graph-name)))
             (when ,graph
               (instantiate-node-type ,meta ,graph)))
           )))))

(defmacro def-vertex (name parent-types slot-specs graph-name)
  `(def-node-type ,name (,@parent-types vertex) ,slot-specs ,graph-name))

(defmacro def-edge (name parent-types slot-specs graph-name)
  `(def-node-type ,name (,@parent-types edge) ,slot-specs ,graph-name))

(defmethod node-type-diff ((meta1 node-type) (meta2 node-type))
  (let ((new-slots (set-difference (node-type-slots meta2)
                                   (node-type-slots meta1)
                                   :test 'equalp))
        (removed-slots (set-difference (node-type-slots meta1)
                                       (node-type-slots meta2)
                                       :test 'equalp)))
    (values (or new-slots removed-slots) new-slots removed-slots)))

(defmethod instantiate-node-type ((meta node-type) (graph graph))
  (with-recursive-lock-held ((schema-lock (schema graph)))
    ;; Check if this type exists and if it differs from old spec
    (log:debug "Looking up ~A: ~A ~A" meta (node-type-name meta) (node-type-parent-type meta))
    (let ((old-meta (lookup-node-type-by-name (node-type-name meta)
                                              (node-type-parent-type meta)
                                              :graph graph)))
      (if (node-type-p old-meta)
          (multiple-value-bind (changes-p new-slots removed-slots)
              (node-type-diff old-meta meta)
            (setf (node-type-id meta) (node-type-id old-meta))
            (if changes-p
                (progn
                  ;; FIXME: what to do with slot changes-p
                  (log:debug "REMOVED SLOTS FOR ~S:~% ~S"
                             (node-type-name meta) removed-slots)
                  (log:debug "NEW SLOTS FOR ~S:~% ~S"
                             (node-type-name meta) new-slots)
                  (update-node-type meta graph))
                old-meta))
          ;; Else if new, assign node-type-id
          (progn
            (setf (gethash (node-type-name meta)
                           (schema-class-locks (schema graph)))
                  (make-rw-lock))
            (setf (node-type-id meta)
                  (get-next-type-id (schema graph)
                                    (node-type-parent-type meta)))
            (update-node-type meta graph))))))

(defmethod update-schema ((graph-name symbol))
  (let ((graph (lookup-graph graph-name)))
    (if graph
        (update-schema graph)
        (error "Cannot update schema for graph ~A: graph not open!" graph-name))))

(defmethod update-schema ((graph graph))
  (with-recursive-lock-held ((schema-lock (schema graph)))
    (let ((node-metadata (gethash (graph-name graph) *schema-node-metadata*)))
      ;; New metadata is pushed on the front of its list; apply
      ;; metadata oldest to newest by reversing
      (dolist (meta (reverse node-metadata))
        (instantiate-node-type meta graph)))
    (save-schema (schema graph) graph)))
