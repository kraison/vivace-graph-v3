(cl:defpackage #:xach-test
  (:use #:graph-db #:cl))

(in-package #:xach-test)

(def-vertex photo ()
  (title
   description
   date-taken)
  :photodb)

(def-vertex album ()
  (title
   description
   date-created)
  :photodb)

(def-edge has-photo ()
  ()
  :photodb)

(def-vertex user ()
  (name)
  :photodb)

(def-edge bought-photo ()
  (price)
  :photodb)

(defun load-views ()
  (def-view title (photo :photodb)
    (:map
     (lambda (photo)
       (when (title photo)
         (yield (title photo) nil))))))


(defun pmod-test (photo new-title)
  (let ((copy (copy photo)))
    (setf (title copy) new-title)
    (save copy)))

(defun conflict-test (id new-title delay &key thread)
  (let ((thunk
         (lambda ()
           (with-transaction ()
             (pmod-test (lookup-photo id) new-title)
             (sleep delay)))))
    (if thread
        (sb-thread:make-thread thunk)
        (funcall thunk))))

(defmethod print-object ((photo photo) stream)
  (print-unreadable-object (photo stream :type t :identity t)
    (format stream "~S" (title photo))))
