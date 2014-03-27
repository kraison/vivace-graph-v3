(in-package :graph-db)

(defstruct index
  skip-list
  key-type
  order
  unique-p
  heap
  addr)

(alexandria:define-constant +string-index+ 1)
(alexandria:define-constant +number-index+ 2)
(alexandria:define-constant +index-ascending+ 1)
(alexandria:define-constant +index-descending+ 2)
(alexandria:define-constant +index-unique+ 1)
(alexandria:define-constant +index-not-unique+ 2)

(defun make-string-index (heap &key (order :ascending) unique-p
                          (value-equal 'equal))
  (let ((pointer (allocate heap 20))
        (skip-list (make-skip-list :heap heap
                                   :key-equal 'string=
                                   :key-comparison (if (eql order :ascending)
                                                       'string<
                                                       'string>)
                                   :head-key (format nil "~A" (code-char 0))
                                   :tail-key (format nil "~A" (code-char 65535))
                                   :duplicates-allowed-p (null unique-p)
                                   :value-equal value-equal)))
    (let ((index (make-index :skip-list skip-list
                             :key-type :string
                             :order order
                             :unique-p unique-p
                             :heap heap
                             :addr pointer)))
      (set-byte (memory-mmap heap) pointer +db-version+)
      (set-byte (memory-mmap heap) (incf pointer) +string-index+)
      (set-byte (memory-mmap heap) (incf pointer) (if (eql order :ascending)
                                                      +index-ascending+
                                                      +index-descending+))
      (set-byte (memory-mmap heap) (incf pointer) (if unique-p
                                                      +index-unique+
                                                      +index-not-unique+))
      (dolist (addr (list (%sn-addr (%sl-head skip-list))
                          (%sn-addr (%sl-tail skip-list))))
        (dotimes (i 8)
          (set-byte (memory-mmap heap)
                    (incf pointer)
                    (ldb (byte 8 (* i 8)) addr))))
      index)))

(defun make-number-index ()
  )

(defun make-custom-index ()
  )
