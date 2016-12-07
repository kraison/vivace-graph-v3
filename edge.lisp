(in-package :graph-db)

(alexandria:define-constant +edge-header-size+
    ;; Size, in bytes, of the standard node header, plus two vertex
    ;; ids and a 64-bit weight
    (+ +node-header-size+
       16
       16
       8))

(defclass edge (node)
  ((from :accessor from :initform +null-key+ :initarg :from
         :type (simple-array (unsigned-byte 8) (16))
         :persistent nil :ephemeral nil :meta t)
   (to :accessor to :initform +null-key+ :initarg :to
       :type (simple-array (unsigned-byte 8) (16))
       :persistent nil :ephemeral nil :meta t)
   (weight :accessor weight :initform 1.0 :initarg :weight :type float
           :persistent nil :ephemeral nil :meta t))
  (:metaclass node-class))

(defmethod print-object ((node node) stream)
  (format stream "#<~A ~S REV ~S (~S -> ~S)>"
          (type-of node) (uuid:byte-array-to-uuid (id node))
          (revision node) (uuid:byte-array-to-uuid (from node))
          (uuid:byte-array-to-uuid (to node))))

(let ((random-states (list (make-random-state)
                           (progn (sleep 1) (make-random-state)))))
  (defun gen-edge-id ()
    (let* ((now (format nil "~,6F~6D~' :@/graph-db::print-byte-array/"
                        (coerce (gettimeofday) 'double-float)
                        (random 1000000 (nth (random 2) random-states))
                        (get-random-bytes 16)))
           (id (uuid:uuid-to-byte-array
                (uuid:make-v5-uuid *edge-namespace* now))))
      id)))

(defun %make-edge (&key id type-id revision deleted-p data-pointer data bytes from
                     to weight written-p heap-written-p type-idx-written-p
                     ve-written-p vev-written-p views-written-p)
  (let ((edge (get-edge-buffer)))
    (when id (setf (id edge) id))
    (when from (setf (from edge) from))
    (when to (setf (to edge) to))
    (when weight (setf (weight edge) weight))
    (when type-id (setf (type-id edge) type-id))
    (when revision (setf (revision edge) revision))
    ;; Flags
    (when deleted-p (setf (deleted-p edge) deleted-p))
    (when written-p (setf (written-p edge) written-p))
    (when heap-written-p (setf (heap-written-p edge) heap-written-p))
    (when type-idx-written-p (setf (type-idx-written-p edge) type-idx-written-p))
    (when ve-written-p (setf (ve-written-p edge) ve-written-p))
    (when vev-written-p (setf (vev-written-p edge) vev-written-p))
    (when views-written-p (setf (views-written-p edge) views-written-p))

    (when data-pointer (setf (data-pointer edge) data-pointer))
    (when data (setf (data edge) data))
    (when bytes (setf (bytes edge) bytes))
    edge))

(defun serialize-edge-head (mf e offset)
  (setq offset (serialize-node-head mf e offset))
  (dotimes (i 16)
    (set-byte mf (incf offset) (aref (from e) i)))
  (dotimes (i 16)
    (set-byte mf (incf offset) (aref (to e) i)))
  (let ((int (ieee-floats:encode-float64 (weight e))))
    (dotimes (i 8)
      (set-byte mf (incf offset) (ldb (byte 8 0) int))
      (setq int (ash int -8)))))

(defun deserialize-edge-head (mf offset)
  (multiple-value-bind
        (deleted-p written-p heap-written-p type-idx-written-p views-written-p
                   ve-written-p vev-written-p type-id revision pointer offset)
      (deserialize-node-head mf offset)
    (let* ((subclass (if (eq type-id 0)
                         'edge
                         (let ((type-meta (lookup-node-type-by-id
                                           type-id :edge)))
                           (node-type-name type-meta))))
           (e (%make-edge
               :deleted-p deleted-p
               :written-p written-p
               :heap-written-p heap-written-p
               :type-idx-written-p type-idx-written-p
               :views-written-p views-written-p
               :ve-written-p ve-written-p
               :vev-written-p vev-written-p
               :type-id type-id
               :revision revision
               :data-pointer pointer
               :from (let ((vec (get-buffer 16)))
                       (dotimes (i 16)
                         (setf (aref vec i) (get-byte mf (incf offset))))
                       vec)
               :to (let ((vec (get-buffer 16)))
                     (dotimes (i 16)
                       (setf (aref vec i) (get-byte mf (incf offset))))
                     vec)
               :weight (let ((int 0))
                         (dotimes (i 8)
                           (setq int (dpb (get-byte mf (incf offset))
                                          (byte 8 (* i 8)) int)))
                         (ieee-floats:decode-float64 int)))))
      (change-class e subclass))))

(defun make-edge-table (location &key (key-test 'uuid-array-equal)
                                   (base-buckets (expt 2 18)))
  (let ((table
         (make-lhash :test key-test
                     :location location
                     :value-bytes +edge-header-size+
                     :bucket-size 24
                     :buckets base-buckets
                     :key-serializer 'serialize-key
                     :key-deserializer 'deserialize-key
                     :value-serializer 'serialize-edge-head
                     :value-deserializer 'deserialize-edge-head)))
    table))

(defmethod lookup-edge ((id string) &key (graph *graph*))
  (lookup-edge (read-id-array-from-string id) :graph graph))

(defmethod lookup-edge ((id array) &key (graph *graph*))
  (lookup-object id (edge-table graph) *transaction* graph))

(defmethod add-to-ve-index ((edge edge) (graph graph) &key unless-present)
  (let ((in-ve-key (make-ve-key :id (to edge) :type-id (type-id edge)))
        (out-ve-key (make-ve-key :id (from edge) :type-id (type-id edge))))
    (ve-index-push (ve-index-in graph) in-ve-key (id edge)
                   :unless-present unless-present)
    (ve-index-push (ve-index-out graph) out-ve-key (id edge)
                   :unless-present unless-present)))

(defmethod remove-from-ve-index ((edge edge) (graph graph))
  (let ((in-ve-key (make-ve-key :id (to edge) :type-id (type-id edge)))
        (out-ve-key (make-ve-key :id (from edge) :type-id (type-id edge))))
    (ve-index-remove (ve-index-in graph) in-ve-key (id edge))
    (ve-index-remove (ve-index-out graph) out-ve-key (id edge))))

(defmethod add-to-vev-index ((edge edge) (graph graph) &key unless-present)
  (let ((vev-key (make-vev-key :in-id (to edge)
                               :out-id (from edge)
                               :type-id (type-id edge)))
        (table (vev-index-table (vev-index graph))))
    ;;(log:debug "add-to-vev-index: ~A" vev-key)
    ;;(log:debug "add-to-vev-index: EDGE: ~A" edge)
    (with-locked-hash-key (table vev-key)
      (let ((index-list (%lhash-get table vev-key)))
        (if index-list
            (progn
              ;;(log:debug "add-to-vev-index: Got ~A" index-list)
              (if unless-present
                  (index-list-pushnew (id edge) index-list)
                  (index-list-push (id edge) index-list))
              (%lhash-update table vev-key index-list)
              ;;(log:debug "add-to-vev-index: AFTER PUSH: ~A" index-list)
              )
            (progn
              (setq index-list
                    (make-index-list (heap graph) (id edge)))
              ;;(log:debug "add-to-vev-index: Made new ~A" index-list)
              (%lhash-insert table vev-key index-list)))
        (cache-index-list (vev-index graph) vev-key index-list)))))

(defmethod remove-from-vev-index ((edge edge) (graph graph))
  (let ((vev-key (make-vev-key :in-id (to edge)
                               :out-id (from edge)
                               :type-id (type-id edge)))
        (table (vev-index-table (vev-index graph))))
    (with-locked-hash-key (table vev-key)
      (let ((index-list (%lhash-get table vev-key)))
        (when index-list
          ;;(log:debug "Removing ~A from ~A" edge index-list)
          (remove-from-index-list (id edge) index-list)
          (%lhash-update table vev-key index-list)
          (cache-index-list (vev-index graph) vev-key index-list))))))

(defmethod add-to-type-index ((edge edge) (graph graph) &key unless-present)
  (type-index-push (id edge) (type-id edge) (edge-index graph)
                   :unless-present unless-present))

(defmethod remove-from-type-index ((edge edge) (graph graph))
  (type-index-remove (id edge) (type-id edge) (edge-index graph)))

(defun make-edge (type from to weight data &key id revision deleted-p
                  retry-p
                  (graph *graph*))
  (when (stringp id)
    (setq id (read-id-array-from-string id)))
  (typecase from
    (string (setq from (read-id-array-from-string from)))
    (vertex (setq from (id from))))
  (typecase to
    (string (setq to (read-id-array-from-string to)))
    (vertex (setq to (id to))))
  (let ((type-meta (or (and (eq type :generic) :generic)
                       (and (eq 0 type) :generic)
                       (and (integerp type)
                            (lookup-node-type-by-id type :edge :graph graph))
                       (lookup-node-type-by-name type :edge :graph graph))))
    (if type-meta
        (let* ((subclass (if (eq type-meta :generic)
                             'edge
                             (node-type-name type-meta)))
               (bytes (when data (serialize data)))
               (e (%make-edge
                   :id (or id (gen-edge-id))
                   :type-id (if (eq type-meta :generic)
                                0
                                (node-type-id type-meta))
                   :revision (or revision 0)
                   :deleted-p deleted-p
                   :written-p nil
                   :from from
                   :to to
                   :weight weight
                   :bytes bytes
                   :data data)))
          (change-class e subclass)
          (setf (bytes e) bytes)
          (handler-case
              (create-node e graph)
            (duplicate-key-error (c)
              (if retry-p
                  (let ((*print-pretty* nil))
                    (log:error "EDGE: Duplicate key error: ~A. Retrying MAKE-EDGE"
                               (id e))
                    (make-edge type from to weight data
                               :id (gen-edge-id)
                               :revision revision
                               :deleted-p deleted-p :graph graph))
                  (error c)))))
        (error "Unknown edge type ~A" type))))

(defmethod copy-edge ((edge edge))
  (let ((e (copy-node edge)))
    (setf (slot-value e 'from) (slot-value edge 'from)
          (slot-value e 'to) (slot-value edge 'to)
          (slot-value e 'weight) (slot-value edge 'weight))
    e))

(defmethod save-edge ((edge edge) &key (graph *graph*))
  ;; you must copy the edge before writing to its slots,
  ;; in case others are reading it!
  (let ((class-name (class-name (class-of edge))))
    (if (lookup-view-group class-name graph)
        (with-write-locked-view-group (class-name graph)
          (multiple-value-bind (new old)
              (save-node edge (edge-table graph) :graph graph)
            (%update-in-views graph new old class-name)
            new))
        (multiple-value-bind (new old)
            (save-node edge (edge-table graph) :graph graph)
          (declare (ignore old))
          new))))

(defmethod delete-edge ((edge edge) &key (graph *graph*))
  (when (deleted-p edge)
    (error 'edge-already-deleted-error
           :node edge))
  (delete-node edge graph))

(defmethod active-edge-p ((edge edge) &key (graph *graph*))
  (and (not (deleted-p edge))
       (let ((from (lookup-vertex (from edge) :graph graph)))
         (if (vertex-p from)
             (not (deleted-p from))
             nil))
       (let ((to (lookup-vertex (to edge) :graph graph)))
         (if (vertex-p to)
             (not (deleted-p to))
             nil))))

(defmethod edge-exists-p (edge-type (vertex1 vertex) (vertex2 vertex)
                          &key (graph *graph*))
  (let ((type-meta (or (and (integerp edge-type)
                            (lookup-node-type-by-id edge-type :edge))
                       (lookup-node-type-by-name edge-type :edge))))
    (when type-meta
      (let* ((vev-key (make-vev-key :in-id (id vertex2)
                                    :out-id (id vertex1)
                                    :type-id (node-type-id type-meta)))
             (index-list (lookup-vev-index-list vev-key graph)))
        (when index-list
          (map-index-list
           (lambda (edge-id)
             (let ((edge (lookup-edge edge-id :graph graph)))
               (when (and (written-p edge)
                          (active-edge-p edge))
                 (return-from edge-exists-p edge))))
           index-list))))))

(defun map-edges (fn graph &key collect-p edge-type vertex direction
                  include-deleted-p to-vertex from-vertex exclude-edge-types)
  ;; FIXME: need to handle subclasses when edge-type is specified
  (let ((result nil))
    (cond ((and edge-type to-vertex from-vertex)
           (let ((type-meta (or (and (integerp edge-type)
                                     (lookup-node-type-by-id edge-type :edge))
                                (lookup-node-type-by-name edge-type :edge))))
             (when type-meta
               (let* ((vev-key (make-vev-key :in-id (id to-vertex)
                                             :out-id (id from-vertex)
                                             :type-id (node-type-id type-meta)))
                      (index-list (lookup-vev-index-list vev-key graph)))
                 (when index-list
                   (map-index-list
                    (lambda (edge-id)
                      (let ((edge (lookup-edge edge-id :graph graph)))
                        (when (and (written-p edge)
                                   (or include-deleted-p
                                       ;;(not (deleted-p edge))))
                                       (active-edge-p edge)))
                          (if collect-p
                              (push (funcall fn edge) result)
                              (funcall fn edge)))))
                    index-list))))))
          ((and to-vertex from-vertex)
           (let ((mapper
                  (lambda (edge-type-id)
                    (when (or (zerop edge-type-id)
                              (null exclude-edge-types)
                              (not
                               (member (node-type-name
                                        (lookup-node-type-by-id edge-type-id :edge))
                                       exclude-edge-types)))
                      (map-edges fn graph
                                 :collect-p collect-p
                                 :edge-type edge-type-id
                                 :from-vertex from-vertex
                                 :to-vertex to-vertex
                                 :exclude-edge-types exclude-edge-types
                                 :include-deleted-p include-deleted-p)))))
             (if collect-p
                 (setq result (mapcan mapper (list-edge-types)))
                 (dolist (type-id (list-edge-types))
                   (funcall mapper type-id)))))
          ((and edge-type vertex)
           (let ((type-meta (or (and (integerp edge-type)
                                     (lookup-node-type-by-id edge-type :edge))
                                (lookup-node-type-by-name edge-type :edge))))
             (when type-meta
               (let* ((ve-key (make-ve-key :id (id vertex)
                                           :type-id (node-type-id type-meta)))
                      (index-list
                       (cond ((eq direction :out)
                              (lookup-ve-out-index-list ve-key graph))
                             ((eq direction :in)
                              (lookup-ve-in-index-list ve-key graph))
                             (t (error "Unknown direction: ~S" direction)))))
                 (when index-list
                   (map-index-list
                    (lambda (edge-id)
                      (let ((edge (lookup-edge edge-id :graph graph)))
                        (when (and (written-p edge)
                                   (or include-deleted-p
                                       ;;(not (deleted-p edge))))
                                       (active-edge-p edge)))
                          (if collect-p
                              (push (funcall fn edge) result)
                              (funcall fn edge)))))
                    index-list))))))
          (vertex
           (let ((mapper
                  (lambda (edge-type-id)
                    (when (or (zerop edge-type-id)
                              (null exclude-edge-types)
                              (not
                               (member (node-type-name
                                        (lookup-node-type-by-id edge-type-id :edge))
                                       exclude-edge-types)))
                      (map-edges fn graph
                                 :collect-p collect-p
                                 :edge-type edge-type-id
                                 :vertex vertex
                                 :direction direction
                                 :exclude-edge-types exclude-edge-types
                                 :include-deleted-p include-deleted-p)))))
             (if collect-p
                 (setq result (mapcan mapper (list-edge-types)))
                 (dolist (type-id (list-edge-types))
                   (funcall mapper type-id)))))
          (edge-type
           (let* ((type-meta (or (and (integerp edge-type)
                                      (lookup-node-type-by-id edge-type :edge))
                                 (lookup-node-type-by-name edge-type :edge)))
                  (edge-type-id (node-type-id type-meta)))
             (when edge-type-id
               (let ((index-list (get-type-index-list (edge-index graph) edge-type-id)))
                 (map-index-list (lambda (id)
                                   (let ((edge (lookup-edge id :graph graph)))
                                     (when (and (written-p edge)
                                                (or include-deleted-p
                                                    ;;(not (deleted-p edge))))
                                                    (active-edge-p edge)))
                                       (if collect-p
                                           (push (funcall fn edge) result)
                                           (funcall fn edge)))))
                                 index-list)))))
          (t
           (map-lhash #'(lambda (pair)
                          (let ((edge (cdr pair)))
                            (when (and (written-p edge)
                                       (or include-deleted-p
                                           (active-edge-p edge))
                                       (not (member (type-of edge) exclude-edge-types)))
                              (setf (id edge) (car pair))
                              (if collect-p
                                  (push (funcall fn edge) result)
                                  (funcall fn edge)))))
                      (edge-table *graph*))))
    (when collect-p (nreverse result))))

(defmethod outgoing-edges ((vertex vertex) &key (graph *graph*) edge-type include-deleted-p)
  (map-edges 'identity graph :vertex vertex :edge-type edge-type :direction :out
             :collect-p t :include-deleted-p include-deleted-p))

(defmethod incoming-edges ((vertex vertex) &key (graph *graph*) edge-type include-deleted-p)
  (map-edges 'identity graph :vertex vertex :edge-type edge-type :direction :in
             :collect-p t :include-deleted-p include-deleted-p))

(defmethod compact-edges ((graph graph))
  (map-edges (lambda (edge)
               (unless (active-edge-p edge)
                 (unless (deleted-p edge)
                   (delete-edge edge :graph graph))
                 (remove-from-type-index edge graph)
                 (remove-from-ve-index edge graph)
                 (remove-from-vev-index edge graph)))
             *graph*
             :include-deleted-p t))
