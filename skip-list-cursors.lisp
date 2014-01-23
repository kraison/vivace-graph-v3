(in-package :graph-db)

(defclass skip-list-cursor (cursor)
  ((node :initarg :node :accessor skip-list-cursor-node)
   (skip-list :initarg :skip-list :accessor skip-list)))

(defmethod cursor-next ((slc skip-list-cursor) &optional eoc)
  (with-slots (node) slc
    (if node
        (if (funcall (%sl-key-equal (skip-list slc))
                     (%sn-key node)
                     (%sn-key (%sl-tail (skip-list slc))))
            eoc
            (let ((result node))
              (setq node (node-forward (skip-list slc) node))
              result))
        eoc)))

(defclass skip-list-value-cursor (skip-list-cursor)
  ())

(defmethod cursor-next :around ((slc skip-list-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (%sn-value result))))

(defclass skip-list-key-cursor (skip-list-cursor)
  ())

(defmethod cursor-next :around ((slc skip-list-key-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (%sn-key result))))

(defmethod make-cursor ((sl skip-list) &key cursor
                        (cursor-class 'skip-list-cursor)
                        &allow-other-keys)
  (if cursor
      (progn (setf (skip-list-cursor-node cursor)
                   (node-forward sl (%sl-head sl)))
             cursor)
      (make-instance cursor-class
                     :skip-list sl
                     :node (node-forward sl (%sl-head sl)))))

(defmethod make-values-cursor ((sl skip-list) &key &allow-other-keys)
  (make-cursor sl :cursor-class 'skip-list-value-cursor))

(defmethod make-keys-cursor ((sl skip-list) &key &allow-other-keys)
  (make-cursor sl :cursor-class 'skip-list-key-cursor))

(defclass skip-list-range-cursor (skip-list-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod cursor-next :around ((slc skip-list-range-cursor) &optional eoc)
  (with-slots (node end) slc
    (if (and node
             (or (funcall (%sl-comparison (skip-list slc)) (%sn-key node) end)
                 (funcall (%sl-key-equal (skip-list slc)) (%sn-key node) end)))
        (call-next-method)
        eoc)))

(defmethod make-range-cursor ((sl skip-list) start end &key &allow-other-keys)
  (let ((preds (make-array (%sl-max-level sl)))
        (succs (make-array (%sl-max-level sl))))
    (multiple-value-bind (node level-found preds succs)
        (find-in-skip-list sl start preds succs)
      (declare (ignore level-found preds))
      (cond (node
             (make-instance 'skip-list-range-cursor
                            :node node :end end :skip-list sl))
;            (preds
;             (make-instance 'skip-list-range-cursor
;                            :node (aref preds 0)
;                            :end end :skip-list sl))
            (succs
             (make-instance 'skip-list-range-cursor
                            :node (aref succs 0)
                            :end end :skip-list sl))))))

(defmethod map-skip-list (fn (sl skip-list) &key collect-p)
  (let ((cursor (make-cursor sl)) (result nil))
    (do ((node (cursor-next cursor)
              (cursor-next cursor)))
        ((null node))
      (if collect-p
          (push (funcall fn node) result)
          (funcall fn node)))
    (when collect-p
      (nreverse result))))

(defmethod map-skip-list-keys (fn (sl skip-list) &key collect-p)
  (let ((cursor (make-cursor sl)) (result nil))
    (do ((node (cursor-next cursor)
              (cursor-next cursor)))
        ((null node))
      (if collect-p
          (push (funcall fn (%sn-key node)) result)
          (funcall fn (%sn-key node))))
    (when collect-p
      (nreverse result))))

(defmethod map-skip-list-values (fn (sl skip-list))
  (let ((cursor (make-values-cursor sl)))
    (do ((val (cursor-next cursor)
              (cursor-next cursor)))
        ((null val))
      (funcall fn val))))

(defmethod skip-list-fetch-all ((sl skip-list) key)
  "Return all values for a key in a skip list where duplicates are allowed."
  (let ((cursor (make-range-cursor sl key key))
        (result nil))
    (if cursor
        (progn
          (do ((node (cursor-next cursor) (cursor-next cursor)))
              ((null node))
            (push (second node) result))
          (nreverse result))
        nil)))
