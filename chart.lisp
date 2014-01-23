(ql:quickload :adw-charting-vecto)
(ql:quickload :cl-ppcre)
(ql:quickload :local-time)
(use-package :adw-charting)

(defun make-chart ()
  (let ((writes nil) (reads nil))
    (with-open-file (in "stats.csv")
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (let ((triple (cl-ppcre:split (format nil "~A" #\Tab) line)))
          (push (list (parse-integer (nth 0 triple))
                      (parse-integer (nth 1 triple)))
                writes)
          (push (list (parse-integer (nth 0 triple))
                      (parse-integer (nth 2 triple)))
                reads))))

    (with-chart (:line 1600 900)
      (set-axis :x "Time"
                :angle t
                :label-formatter
                (lambda (time)
                  (local-time:to-rfc3339-timestring
                   (local-time:universal-to-timestamp time))))
      (add-series "Writes" writes)
      (add-series "Reads" reads)
      (set-axis :y "Count")
      (save-file "stats.png"))))
