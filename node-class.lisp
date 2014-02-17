(in-package :graph-db)

(defclass node-class (standard-class) nil)

(defmethod sb-mop:validate-superclass ((class node-class) (super standard-class))
  "Node classes may inherit from ordinary classes."
  t)

(defclass node-slot-definition (standard-slot-definition)
  ((persistent :accessor persistent-p :initarg :persistent :initform t :allocation :instance)
   (indexed :accessor indexed-p :initarg :index :initform nil :allocation :instance)
   (ephemeral :accessor ephemeral-p :initarg :ephemeral :initform nil :allocation :instance)
   (meta :accessor meta-p :initarg :meta :initform nil :allocation :instance)))

(defmethod persistent-p (slot-def)
  nil)

(defmethod indexed-p (slot-def)
  nil)

(defmethod ephemeral-p (slot-def)
  nil)

(defmethod meta-p (slot-def)
  nil)

(defclass node-direct-slot-definition
    (standard-direct-slot-definition node-slot-definition)
  ())

(defclass node-effective-slot-definition
    (standard-effective-slot-definition node-slot-definition)
  ())

(defmethod data-slots ((instance node-class))
  "Return a list of managed slot names for an instance."
  (map 'list 'slot-definition-name
       (remove-if-not #'(lambda (i)
                          (or (persistent-p i) (ephemeral-p i)))
                      (sb-pcl:class-slots instance))))

(defmethod meta-slot-names ((instance node-class))
  "Return a list of metadata slot names for an instance."
  ;;(dbg "meta-slot-names(~A)" instance)
  (let ((names
         (map 'list 'slot-definition-name
              (remove-if-not #'(lambda (i)
                                 (meta-p i))

               (sb-pcl:class-slots instance)))))
    ;;(dbg "meta-slot-names(~A): ~A" instance names)
    names))

(defmethod persistent-slot-names ((instance node-class))
  "Return a list of persistent slot names for an instance."
  ;;(dbg "persistent-slot-names(~A)" instance)
  (let ((names
         (map 'list 'slot-definition-name
              (remove-if-not #'(lambda (i)
                                 (persistent-p i))
                             (sb-pcl:class-slots instance)))))
    ;;(dbg "persistent-slot-names(~A): ~A" instance names)
    names))

(defmethod ephemeral-slot-names ((instance node-class))
  "Return a list of persistent slot names for an instance."
  ;;(dbg "ephemeral-slot-names(~A)" instance)
  (let ((names
         (map 'list 'slot-definition-name
              (remove-if-not #'(lambda (i)
                                 (ephemeral-p i))
                             (sb-pcl:class-slots instance)))))
    ;;(dbg "ephemeral-slot-names(~A): ~A" instance names)
    names))

(defmethod direct-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  ;;(dbg "direct-slot-definition-class for ~A" class)
  (find-class 'node-direct-slot-definition))

(defmethod effective-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  ;;(dbg "effective-slot-definition-class for ~A" class)
  (find-class 'node-effective-slot-definition))

(defmethod compute-effective-slot-definition :around
    ((class node-class) slot-name direct-slots)
  "Ensure inheritance from direct slot definition of persistent, indexed,
   and ephemeral properties."
  ;;(dbg "compute-effective-slot-definition for ~A / ~A: ~A" class slot-name direct-slots)
  (let ((slot (call-next-method)))
    ;;(dbg "  SLOT: ~A" slot)
    (cond ((or (meta-p slot) (some 'meta-p direct-slots))
           (setf (slot-value slot 'meta) t)
           (setf (slot-value slot 'persistent) nil))
          ((or (persistent-p slot) (some 'persistent-p direct-slots))
           (setf (slot-value slot 'persistent) t))
          (t
           (setf (slot-value slot 'persistent) nil)
           (setf (slot-value slot 'ephemeral) t)))
    (when (or (indexed-p slot) (some 'indexed-p direct-slots))
      (setf (slot-value slot 'indexed) t)
      ;; FIXME: Generate index if needed
      )
    slot))

(defmethod find-all-subclasses ((class class))
  ;;(dbg "Finding subclasses for ~A" class)
  (let ((result nil))
    (labels ((find-them (class)
               (let ((subclasses (sb-mop:class-direct-subclasses class)))
                 ;;(dbg "Found subclasses for ~A: ~A" class subclasses)
                 (dolist (subclass subclasses)
                   (unless (find subclass result)
                     (push subclass result)
                     (find-them subclass))))))
      (find-them class)
      result)))

(defmethod find-all-subclass-names ((class class))
  (mapcar 'class-name (find-all-subclasses class)))

(defmethod find-graph-parent-classes ((class node-class))
  (let ((classes
         (remove-if (lambda (class)
                      (or (eq (class-name class) 'vertex)
                          (eq (class-name class) 'edge)
                          (eq (class-name class) 'primitive-node)))
                    (sb-mop:class-direct-superclasses class))))
    (remove-duplicates
     (nconc classes
            (mapcan 'find-graph-parent-classes classes)))))

(defclass node ()
  ((id :accessor id :initform +null-key+ :initarg :id :meta t
       :type (simple-array (unsigned-byte 8) (16)) :persistent nil)
   (type-id :accessor type-id :initform 1 :initarg :type-id :meta t
            :type (unsigned-byte 16) :persistent nil)
   (revision :accessor revision :initform 0 :initarg :revision :meta t
             :type (unsigned-byte 32) :persistent nil)
   (%revision-table :accessor %revision-table :initform (make-hash-table :test 'eq)
                    :initarg :revision-table :meta t :persistent nil)
   (heap-written-p :accessor heap-written-p :initform nil :initarg :heap-written-p
                   :type boolean :meta t :persistent nil)
   (type-idx-written-p :accessor type-idx-written-p :initform nil :meta t
                       :initarg :type-idx-written-p :type boolean :persistent nil)
   (ve-written-p :accessor ve-written-p :initform nil :initarg :ve-written-p
                 :type boolean :meta t :persistent nil)
   (vev-written-p :accessor vev-written-p :initform nil :initarg :vev-written-p
                  :type boolean :meta t :persistent nil)
   (views-written-p :accessor views-written-p :initform nil :meta t
                    :initarg :views-written-p :type boolean :persistent nil)
   (written-p :accessor written-p :initform nil :initarg :written-p :type boolean
              :meta t :persistent nil)
   (data-pointer :accessor data-pointer :initform 0 :initarg :data-pointer
                 :type (unsigned-byte 64) :meta t :persistent nil)
   (deleted-p :accessor deleted-p :initform nil :initarg :deleted-p :type boolean
              :meta t :persistent nil)
   (data :accessor data :initarg :data :initform nil :meta t :persistent nil)
   (bytes :accessor bytes :initform :init :initarg :bytes :meta t :persistent nil))
  (:metaclass node-class))

