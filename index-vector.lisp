(in-package :graph-db)

(defstruct
    (index-vector
      (:print-function
       (lambda (i s d)
         (declare (ignore d))
         (let ((*print-base* 10))
           (format s "#<INDEX-VECTOR~%  :ADRESS ~S~%  :SIZE ~S~%  :VECTOR ~S>"
                   (index-vector-address i)
                   (index-vector-size i)
                   (map 'list
                        #'uuid:byte-array-to-uuid
                        (index-vector-vector i))))))
      (:constructor %make-index-vector))
  (address 0 :type (unsigned-byte 64))
  (size 0 :type integer)
  (vector #() :type (array (simple-array (unsigned-byte 8) (16))))
  heap)

(defun make-index-vector (heap key-vector)
  (let* ((total-size (+ 8 (* 16 (length key-vector))))
         (address (allocate heap total-size))
         (index-vector (%make-index-vector :address address
                                           :heap heap
                                           :size (length key-vector)
                                           :vector key-vector)))
    (serialize-uint64 (memory-mmap heap) (index-vector-size index-vector) address)
    (incf address 7)
    (dotimes (i (index-vector-size index-vector))
      (dotimes (j 16)
        (set-byte (memory-mmap heap) (incf address) (aref (aref key-vector i) j))))
    (if *graph*
        (setf (gethash (index-vector-address index-vector)
                       (index-vector-cache *graph*)) index-vector)
        index-vector)))

(defun get-index-vector (heap address)
  (or (and *graph* *cache-enabled* (gethash address (index-vector-cache *graph*)))
      (let* ((size (deserialize-uint64 (memory-mmap heap) address))
             (index-vector
              (%make-index-vector
               :address address
               :heap heap
               :size size
               :vector (make-array size
                                   :element-type
                                   '(simple-array (unsigned-byte 8) (16))))))
        (incf address 7)
        (dotimes (i size)
          (let ((key (get-buffer 16)))
            (dotimes (j 16)
              (setf (aref key j) (get-byte (memory-mmap heap) (incf address))))
            (setf (aref (index-vector-vector index-vector) i) key)))
        (if *graph*
            (setf (gethash (index-vector-address index-vector)
                           (index-vector-cache *graph*)) index-vector)
            index-vector))))

(defun index-vector-push-extend (index-vector key &key free-old-p)
  (let* ((heap (index-vector-heap index-vector))
         (new-size (1+ (index-vector-size index-vector)))
         (total-size (+ 8 (* 16 new-size)))
         (old-address (index-vector-address index-vector))
         (new-address (allocate heap total-size))
         (current-address new-address)
         (key-vector (make-array new-size
                                 :initial-element +null-key+
                                 :element-type
                                 '(simple-array (unsigned-byte 8) (16)))))
    (serialize-uint64 (memory-mmap heap) new-size current-address)
    (incf current-address 7)
    (dotimes (i (index-vector-size index-vector))
      (setf (aref key-vector i) (aref (index-vector-vector index-vector) i))
      (dotimes (j 16)
        (set-byte (memory-mmap heap)
                  (incf current-address)
                  (aref (aref key-vector i) j))))
    ;; Add new key
    (setf (aref key-vector (1- new-size)) key)
    (dotimes (j 16)
      (set-byte (memory-mmap heap)
                (incf current-address)
                (aref key j)))
    (setf (index-vector-address index-vector)
          new-address
          (index-vector-vector index-vector)
          key-vector
          (index-vector-size index-vector)
          new-size)
    (when free-old-p
      (free heap old-address))
    ;;(when *graph*
    ;;  (setf (gethash (index-vector-address index-vector) (index-vector-cache *graph*))
    ;;        index-vector))
    (values index-vector old-address)))

(defun index-vector-remove (index-vector key &key free-old-p)
  (let* ((heap (index-vector-heap index-vector))
         (key-vector (remove key (index-vector-vector index-vector) :test 'equalp))
         (new-size (length key-vector))
         (total-size (+ 8 (* 16 new-size)))
         (old-address (index-vector-address index-vector))
         (new-address (allocate heap total-size))
         (current-address new-address))
    (serialize-uint64 (memory-mmap heap) new-size current-address)
    (incf current-address 7)
    (dotimes (i new-size)
      (dotimes (j 16)
        (set-byte (memory-mmap heap)
                  (incf current-address)
                  (aref (aref key-vector i) j))))
    (setf (index-vector-address index-vector)
          new-address
          (index-vector-vector index-vector)
          key-vector
          (index-vector-size index-vector)
          new-size)
    (when free-old-p
      (free heap old-address))
    ;;(when *graph*
    ;;  (setf (gethash (index-vector-address index-vector) (index-vector-cache *graph*))
    ;;        index-vector))
    (values index-vector old-address)))

(defclass index-vector-cursor (cursor)
  ((node :initarg :node :accessor index-vector-cursor-node)
   (index :initarg :index :accessor index :initform 0)
   (index-vector :initarg :index-vector :accessor index-vector)
   (key-fn :initarg :key-fn :accessor key-fn)
   (value-fn :initarg :value-fn :accessor value-fn)
   (lookup-fn :initarg :lookup-fn :accessor lookup-fn)))

(defmethod make-cursor ((index-vector index-vector) &key
                        (cursor-class 'index-vector-cursor)
                        key-fn value-fn lookup-fn)
  (make-instance cursor-class
                 :index-vector index-vector
                 :node (aref (index-vector-vector index-vector) 0)
                 :index 0
                 :lookup-fn lookup-fn
                 :key-fn key-fn
                 :value-fn value-fn))

(defmethod cursor-next ((cursor index-vector-cursor) &optional eoc)
  (with-slots (node index index-vector) cursor
    (if node
        (let ((result node))
          (incf index)
          (setq node (funcall (lookup-fn cursor)
                              (aref (index-vector-vector index-vector) index)))
          result)
        eoc)))

(defclass index-vector-value-cursor (index-vector-cursor)
  ())

(defmethod make-values-cursor ((index-vector index-vector) &key
                               key-fn value-fn lookup-fn)
  (make-cursor index-vector
               :cursor-class 'index-vector-value-cursor
               :lookup-fn lookup-fn
               :key-fn key-fn
               :value-fn value-fn))

(defmethod cursor-next :around ((cursor index-vector-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (funcall (value-fn cursor) result))))

(defclass index-vector-key-cursor (index-vector-cursor)
  ())

(defmethod make-keys-cursor ((index-vector index-vector) &key
                             key-fn value-fn lookup-fn)
  (make-cursor index-vector
               :cursor-class 'index-vector-key-cursor
               :lookup-fn lookup-fn
               :key-fn key-fn
               :value-fn value-fn))

(defmethod cursor-next :around ((cursor index-vector-key-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (funcall (key-fn cursor) result))))

(defmethod map-index-vector (fun (index-vector index-vector) &key lookup-fn key-fn value-fn)
  (let ((cursor (make-cursor index-vector
                             :lookup-fn lookup-fn
                             :key-fn key-fn
                             :value-fn value-fn)))
    (do ((val (cursor-next cursor)
              (cursor-next cursor)))
        ((null val))
      (apply fun val))))
