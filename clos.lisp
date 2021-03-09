(in-package :graph-db)

(defvar *meta-slots*
  '(id %type-id %revision %deleted-p %heap-written-p %type-idx-written-p %ve-written-p
    %vev-written-p %views-written-p %written-p %data-pointer %data %bytes from to weight))

(defclass graph-class (standard-class)
  ())

(defmethod validate-superclass ((class graph-class) (super standard-class))
  "Graph classes may inherit from ordinary classes."
  t)

(defclass graph-slot-definition (standard-slot-definition)
  ())

(defclass graph-direct-slot-definition
    (standard-direct-slot-definition graph-slot-definition)
  ())

(defclass graph-effective-slot-definition
    (standard-effective-slot-definition graph-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class graph-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'graph-direct-slot-definition))

(defmethod effective-slot-definition-class ((class graph-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'graph-effective-slot-definition))

(defmethod compute-effective-slot-definition :around ((class graph-class) slot-name direct-slots)
  (let ((slot (call-next-method)))
    ;;
    slot))

(defmethod slot-value-using-class :around ((class graph-class) instance slot)
  (let ((slot-name (sb-mop:slot-definition-name slot)))
    (if (find slot-name *meta-slots*)
        (call-next-method)
        (let ((key (intern (symbol-name slot-name) :keyword)))
          (cdr (assoc key (data instance)))))))

(defmethod (setf slot-value-using-class) :around (new-value (class graph-class) instance slot)
  (let ((slot-name (sb-mop:slot-definition-name slot)))
    (if (find slot-name *meta-slots*)
        (call-next-method)
        (let ((key (intern (symbol-name slot-name) :keyword)))
          (setf (cdr (assoc key (data instance))) new-value)
          (if *current-transaction*
              (pushnew instance (txn-update-queue *current-transaction*) :test 'equalp :key 'id)
              (save-node instance))))))

(defmethod slot-makunbound-using-class :around ((class graph-class) instance slot)
  (let ((slot-name (sb-mop:slot-definition-name slot)))
    (if (find slot-name *meta-slots*)
        (call-next-method)
        (let ((key (intern (symbol-name slot-name) :keyword)))
          (setf (data instance) (delete key (data instance) :key 'car))
          instance))))

(defclass node ()
  ((id :accessor id :initform +null-key+ :initarg :id
       :type (simple-array (unsigned-byte 8) (16)))
   (%type-id :accessor %type-id :initform 1 :initarg :%type-id
            :type (unsigned-byte 16))
   (%revision :accessor %revision :initform 0 :initarg :%revision
             :type (unsigned-byte 32))
   (%deleted-p :accessor %deleted-p :initform nil :initarg :%deleted-p :type boolean)
   (%heap-written-p :accessor %heap-written-p :initform nil :initarg :%heap-written-p
                   :type boolean)
   (%type-idx-written-p :accessor %type-idx-written-p :initform nil
                       :initarg :%type-idx-written-p :type boolean)
   (%ve-written-p :accessor %ve-written-p :initform nil :initarg :%ve-written-p
                 :type boolean)
   (%vev-written-p :accessor %vev-written-p :initform nil :initarg :%vev-written-p
                  :type boolean)
   (%views-written-p :accessor %views-written-p :initform nil
                    :initarg :%views-written-p :type boolean)
   (%written-p :accessor %written-p :initform nil :initarg :%written-p :type boolean)

   (%data-pointer :accessor %data-pointer :initform 0 :initarg :%data-pointer
                 :type (unsigned-byte 64))
   (%data :accessor %data :initarg :%data :initform nil)
   (%bytes :accessor %bytes :initform :init :initarg :%bytes))
  (:metaclass graph-class))

