(in-package :newshole)

(defun load-configuration ()
  (with-open-file (stream ".newshole" :if-does-not-exist nil)
    (when stream
      (let ((options (read stream :eof-error-p nil)))
        (setf *pomo-db-spec*
              (or (getf options :db-spec) *pomo-db-spec*))
        (setf *default-pathname-defaults*
              (or (getf options :default-pathname) *default-pathname-defaults*))))))
