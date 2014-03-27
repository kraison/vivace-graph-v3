(in-package :graph-db)

(defun dbg (fmt &rest args)
  (apply #'format t fmt args)
  (terpri))

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(defun get-random-bytes (&optional (count 16))
  (with-open-file (in "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-byte-vector count)))
      (dotimes (i count)
        (setf (aref bytes i) (read-byte in)))
      bytes)))

(defun print-byte-array (stream array
                         &optional colon amp (delimiter #\Space))
  (declare (ignore colon amp delimiter))
  (loop
     :for x :across array
     :do (format stream "~A" (code-char x))))

(defun gettimeofday ()
  (multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
    (+ sec (/ msec 1000000))))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun line-count (file)
  (with-open-file (in file)
    (loop
       for x from 0
       for line = (read-line in nil :eof)
       until (eql line :eof)
       finally (return x))))

(defun last1 (lst)
  (first (last lst)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
      (format t " Type ; to see more or . to stop")
      (continue-p))))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "return a list of leaves of tree satisfying predicate, with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun new-interned-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun gen-id ()
  (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))

(defun parse-uuid-block (string start end)
  (parse-integer string :start start :end end :radix 16))

(defun read-uuid-from-string (string)
  "Creates an uuid from the string represenation of an uuid. (example input string
6ba7b810-9dad11d180b400c04fd430c8)"
  (setq string (remove #\- string))
  (unless (= (length string) 32)
    (error "~@<Could not parse ~S as UUID: string representation ~
has invalid length (~D). A valid UUID string representation has 32 ~
characters.~@:>" string (length string)))
  (make-instance 'uuid:uuid
                 :time-low      (parse-uuid-block string  0 8)
                 :time-mid      (parse-uuid-block string  8 12)
                 :time-high     (parse-uuid-block string 12 16)
                 :clock-seq-var (parse-uuid-block string 16 18)
                 :clock-seq-low (parse-uuid-block string 18 20)
                 :node          (parse-uuid-block string 20 32)))

(defun read-id-array-from-string (string)
  (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
    (loop for i from 3 downto 0
       do (setf (aref array (- 3 i))
                (ldb (byte 8 (* 8 i)) (parse-uuid-block string  0 8))))
    (loop for i from 5 downto 4
       do (setf (aref array i)
                (ldb (byte 8 (* 8 (- 5 i))) (parse-uuid-block string  8 12))))
    (loop for i from 7 downto 6
       do (setf (aref array i)
                (ldb (byte 8 (* 8 (- 7 i))) (parse-uuid-block string 12 16))))
    (setf (aref array 8) (ldb (byte 8 0) (parse-uuid-block string 16 18)))
    (setf (aref array 9) (ldb (byte 8 0) (parse-uuid-block string 18 20)))
    (loop for i from 15 downto 10
       do (setf (aref array i)
                (ldb (byte 8 (* 8 (- 15 i))) (parse-uuid-block string 20 32))))
    array))

(defun free-memory ()
  (- (sb-kernel::dynamic-space-size) (sb-kernel:dynamic-usage)))

(defun djb-hash (seq)
  ;; Not used
  (unless (typep seq 'sequence)
    (setq seq (format nil "~A" seq)))
  (let ((hash 5381))
    (dotimes (i (length seq))
      (let ((item (elt seq i)))
        (typecase item
          (integer   nil)
          (character (setq item (char-code item)))
          (float     (setq item (truncate item)))
          (otherwise (setq item 1)))
        (setf hash (+ (+ hash (ash hash -5)) item))))
    hash))

(defun fast-djb-hash (seq)
  ;; Not used
  (let ((hash 5381))
    (dotimes (i (length seq))
      (setf hash (+ (+ hash (ash hash -5)) (elt seq i))))
    hash))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun make-byte-vector (length)
  (make-array `(,length) :element-type '(unsigned-byte 8) :initial-element 0))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defun dump-hash (hash)
  (loop for k being the hash-keys in hash using (hash-value v)
       do (dbg "~S:~% ~S" k v)))

(defgeneric less-than (x y)
  (:documentation
   "Generic less-than operator.  Allows comparison of apples and oranges.")
  ;; Sentinels for generic skip lists
  (:method ((x (eql +min-sentinel+)) y) t)
  (:method ((x (eql +max-sentinel+)) y) nil)

  (:method ((x (eql +min-sentinel+)) (y number))  t)
  (:method ((x (eql +min-sentinel+)) (y symbol))  t)
  (:method ((x (eql +min-sentinel+)) (y (eql t))) t)
  (:method ((x (eql +min-sentinel+)) (y null))    t)
  (:method ((x (eql +min-sentinel+)) (y list))    t)
  (:method ((x number)  (y (eql +min-sentinel+))) nil)
  (:method ((x symbol)  (y (eql +min-sentinel+))) nil)
  (:method ((x (eql t)) (y (eql +min-sentinel+))) nil)
  (:method ((x null)    (y (eql +min-sentinel+))) nil)
  (:method ((x list)    (y (eql +min-sentinel+))) nil)

  (:method ((x (eql +max-sentinel+)) (y number))  nil)
  (:method ((x (eql +max-sentinel+)) (y symbol))  nil)
  (:method ((x (eql +max-sentinel+)) (y string))  nil)
  (:method ((x (eql +max-sentinel+)) (y (eql t))) nil)
  (:method ((x (eql +max-sentinel+)) (y null))    nil)
  (:method ((x (eql +max-sentinel+)) (y list))    nil)
  (:method ((x number)  (y (eql +max-sentinel+))) t)
  (:method ((x symbol)  (y (eql +max-sentinel+))) t)
  (:method ((x string)  (y (eql +max-sentinel+))) t)
  (:method ((x (eql t)) (y (eql +max-sentinel+))) t)
  (:method ((x null)    (y (eql +max-sentinel+))) t)
  (:method ((x list)    (y (eql +max-sentinel+))) t)

  (:method ((x (eql t))   (y null))      nil)
  (:method ((x null)      (y (eql t)))   t)
  (:method ((x (eql t))   y)             t)
  (:method ((x null)      y)             t)

  (:method ((x symbol)    (y symbol))    (string< (symbol-name x) (symbol-name y)))
  (:method ((x string)    (y string))    (string< x y))
  (:method ((x number)    (y number))    (< x y))
  (:method ((x timestamp) (y timestamp)) (timestamp< x y))
  (:method ((x uuid:uuid) (y uuid:uuid)) (string<
                                          (uuid:print-bytes nil x)
                                          (uuid:print-bytes nil y)))

  (:method ((x list) (y list))           (or (less-than (car x) (car y))
                                             (and (equal (car x) (car y))
                                                  (less-than (cdr x) (cdr y)))))
  (:method ((x list) y)                  t)
  (:method (x        (y list))           nil)

  (:method ((x number)    y)            t)
  (:method ((x number)    (y (eql t)))  nil)
  (:method ((x number)    (y null))     nil)
  (:method (x             (y number))   nil)

  (:method ((x string)    (y symbol))    nil)
  (:method ((x symbol)    (y string))    t)

  (:method ((x symbol)    (y timestamp)) nil)
  (:method ((x timestamp) (y symbol))    t)

  (:method ((x symbol)    (y uuid:uuid)) nil)
  (:method ((x uuid:uuid) (y symbol))    t)

  (:method ((x string)    (y timestamp)) nil)
  (:method ((x timestamp) (y string))    t)

  (:method ((x string)    (y uuid:uuid)) nil)
  (:method ((x uuid:uuid) (y string))    t)

  (:method ((x uuid:uuid) (y timestamp)) nil)
  (:method ((x timestamp) (y uuid:uuid)) t))

(defun key-vector< (v1 v2)
  (cond ((= (array-dimension v1 0) 0)
         nil)
        ((< (aref v1 0) (aref v2 0))
         t)
        ((= (aref v1 0) (aref v2 0))
         (key-vector< (subseq v1 1) (subseq v2 1)))
        (t
         nil)))

(defun key-vector<= (v1 v2)
  (cond ((= (array-dimension v1 0) 0)
         t)
        ((< (aref v1 0) (aref v2 0))
         t)
        ((= (aref v1 0) (aref v2 0))
         (key-vector<= (subseq v1 1) (subseq v2 1)))
        (t
         nil)))

