(in-package :graph-db)

(defmethod snapshot ((graph graph) &key include-deleted-p
                     (check-data-integrity-p t))
  (let ((count nil))
    (with-recursive-lock-held ((txn-lock graph))
      (let ((problems (when check-data-integrity-p
                        (check-data-integrity graph
                                              :include-deleted-p
                                              include-deleted-p))))
        (if problems
            (return-from snapshot
              (values :data-integrity-issues
                      problems))
            (progn
              (multiple-value-bind (sec msec) (sb-ext:get-time-of-day)
                (let ((snap-file (format nil "~A/txn-log/snap-~D.~6,'0D"
                                         (location graph) sec msec)))
                  (setq count (backup graph
                                      snap-file
                                      :include-deleted-p include-deleted-p))))
              count))))))

(defun find-newest-snapshot (dir)
  (let ((file (first (sort
                      (remove-if-not (lambda (file)
                                       (cl-ppcre:scan "^snap-"
                                                      (file-namestring file)))
                                     (cl-fad:list-directory dir))
                      '> :key 'file-write-date))))
    (when file
      (values file (file-write-date file)))))

(defmethod replay ((graph graph) txn-dir package-name &key (check-integrity-p t))
  (let ((snapshot (find-newest-snapshot txn-dir)))
    (when snapshot
      (recreate-graph graph snapshot :package-name package-name))
    (log:debug "Generating graph views.")
    (map nil
         (lambda (pair)
           (destructuring-bind (class-name . view-name) pair
             (regenerate-view graph class-name view-name)))
         (all-views graph))
    (log:debug "Checking data integrity.")
    (if check-integrity-p
        (check-data-integrity graph)
        graph)))
