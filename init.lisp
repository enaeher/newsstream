(in-package :newshole)

(eval-when (:compile-toplevel)
  ;; Startup hooks
  (pushnew (lambda ()
             (with-open-file (stream ".newshole" :if-does-not-exist nil)
               (when stream
                 (let ((options (read stream :eof-error-p nil)))
                   (setf *pomo-db-spec*
                         (or (getf options :db-spec) *pomo-db-spec*))
                   (setf *default-pathname-defaults*
                         (or (getf options :default-pathname) *default-pathname-defaults*)))))
             (start)
             (sb-ext:quit))
           sb-ext:*init-hooks*)
  (pushnew #'swank:create-server sb-ext:*init-hooks*))