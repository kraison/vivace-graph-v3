(in-package :graph-db)

(defclass guid (uuid:uuid)
  ((bytes :initarg :bytes :accessor bytes)
   (to-string :initarg :to-string :accessor to-string)))

(defmethod print-object ((id guid) stream)
  "Prints an uuid in the string represenation of an uuid. (example string 6ba7b810-
9dad-11d1-80b4-00c04fd430c8)"
  (if (slot-boundp id 'to-string)
      (format stream "~A" (to-string id))
      (format stream "~(~8,'0X~4,'0X~4,'0X~2,'0X~2,'0X~12,'0X~)"
              (uuid::time-low id)
              (uuid::time-mid id)
              (uuid::time-high id)
              (uuid::clock-seq-var id)
              (uuid::clock-seq-low id)
              (uuid::node id))))

(defun guid-to-byte-array (uuid &optional type-specifier)
  "Converts an guid to byte-array"
  (let ((array
         (if type-specifier
             (make-array 17 :element-type '(unsigned-byte 8))
             (make-array 16 :element-type '(unsigned-byte 8))))
        (x 0))
    (when type-specifier
      (setf (aref array x) type-specifier)
      (incf x))
    (with-slots
          (uuid:time-low uuid:time-mid uuid:time-high-and-version
                         uuid:clock-seq-and-reserved
                         uuid:clock-seq-low uuid:node)
        uuid
      (loop for i from 3 downto 0
         do (setf (aref array (+ x (- 3 i))) (ldb (byte 8 (* 8 i)) uuid:time-low)))
      (loop for i from 5 downto 4
         do (setf (aref array (+ x i)) (ldb (byte 8 (* 8 (- 5 i))) uuid:time-mid)))
      (loop for i from 7 downto 6
         do (setf (aref array (+ x i))
                  (ldb (byte 8 (* 8 (- 7 i))) uuid:time-high-and-version)))
      (setf (aref array (+ x 8)) (ldb (byte 8 0) uuid:clock-seq-and-reserved))
      (setf (aref array (+ x 9)) (ldb (byte 8 0) uuid:clock-seq-low))
      (loop for i from 15 downto 10
         do (setf (aref array (+ x i)) (ldb (byte 8 (* 8 (- 15 i))) uuid:node)))
      array)))

(defun make-guid ()
  (unless uuid::*uuid-random-state*
    (setf uuid::*uuid-random-state* (make-random-state t)))
  (let ((id (make-instance
             'guid
             :time-low (random #xffffffff uuid::*uuid-random-state*)
             :time-mid (random #xffff uuid::*uuid-random-state*)
             :time-high (dpb #b0100 (byte 4 12) (ldb (byte 12 0) (random #xffff uuid::*uuid-random-state*)))
             :clock-seq-var (dpb #b10 (byte 2 6) (ldb (byte 8 0) (random #xff uuid::*uuid-random-state*)))
             :clock-seq-low (random #xff uuid::*uuid-random-state*)
             :node (random #xffffffffffff uuid::*uuid-random-state*))))
    (setf (bytes id) (guid-to-byte-array id))
    (setf (to-string id) (print-object id nil))
    id))

(defun parse-uuid-block (string start end)
  (parse-integer string :start start :end end :radix 16))

(defun read-guid-from-string (string)
  "Creates an uuid from the string represenation of an uuid. (example input string
6ba7b8109dad11d180b400c04fd430c8)"
  (setq string (remove #\- string))
  (unless (= (length string) 32)
    (error "~@<Could not parse ~S as UUID: string representation ~
has invalid length (~D). A valid UUID string representation has 32 ~
characters.~@:>" string (length string)))
  (make-instance 'guid
                 :to-string     string
                 :bytes         (read-id-array-from-string string)
                 :time-low      (parse-uuid-block string  0 8)
                 :time-mid      (parse-uuid-block string  8 12)
                 :time-high     (parse-uuid-block string 12 16)
                 :clock-seq-var (parse-uuid-block string 16 18)
                 :clock-seq-low (parse-uuid-block string 18 20)
                 :node          (parse-uuid-block string 20 32)))

(defun byte-array-to-guid (array)
  "Converts a byte-array generated with uuid-to-byte-array to an uuid."
  (check-type array
              (array (unsigned-byte 8) (16))
              "Provided value is not an one-dimensional array with 16 elements of type (unsigned-byte 8)")
  (make-instance 'guid
                 :bytes array
                 :time-low (uuid::arr-to-bytes 0 3 array)
                 :time-mid (uuid::arr-to-bytes 4 5 array)
                 :time-high (uuid::arr-to-bytes 6 7 array)
                 :clock-seq-var (aref array 8)
                 :clock-seq-low (aref array 9)
                 :node (uuid::arr-to-bytes 10 15 array)))
