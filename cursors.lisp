(in-package :graph-db)

(defclass cursor ()
  ())

(defgeneric cursor-next (cursor &optional eoc))
(defgeneric cursor-prev (cursor &optional eoc))
(defgeneric make-cursor (index &key cursor-class &allow-other-keys))
(defgeneric make-values-cursor (index &key &allow-other-keys))
(defgeneric make-keys-cursor (index &key &allow-other-keys))
(defgeneric make-range-cursor (index start end &key &allow-other-keys))

