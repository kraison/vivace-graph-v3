(in-package :graph-db)

(defstruct (pcons
             (:constructor %make-pcons)
             (:conc-name %pcons-)
             (:print-function
              (lambda (c s d)
                (declare (ignore d))
                (format s "#P(~A ~A (deleted-p ~A))"
                        (%pcons-car c) (%pcons-cdr c) (%pcons-deleted-p c)))))
  car
  cdr
  deleted-p)

(defmethod serialize-pcons ((pcons pcons) heap)
  (let ((address (allocate heap (+ 16 8 1))))
    ;; Serialize the id / pointer pair
    (dotimes (i 16)
      (set-byte heap
                (+ i address)
                (aref (%pcons-car pcons) i)))
    (serialize-uint64 heap
                      (%pcons-cdr pcons)
                      (+ 16 address))
    (let ((flags 0))
      (when (%pcons-deleted-p pcons)
        (setq flags (dpb 1 (byte 1 0) flags)))
      (set-byte heap (+ 24 address) flags))
    address))

(defgeneric deserialize-pcons (index-list address))

(defmethod mark-pcons-deleted ((pcons pcons) heap address)
  (setf (%pcons-deleted-p pcons) t)
  (let ((flags 0))
    (setq flags (dpb 1 (byte 1 0) flags))
    (set-byte heap (+ 24 address) flags)))

