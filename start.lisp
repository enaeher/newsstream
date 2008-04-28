(in-package :newshole)

(defun start-polling ()
  (sb-thread:make-thread (lambda ()
                           (loop
                              (let* ((now (clsql:get-time))
                                     (today (clsql:time+ (clsql-sys::date->time (clsql:get-date))
                                                         (clsql:make-duration :hour 13 :minute 30)))
                                     (tomorrow (clsql:roll today :day 1))
                                     (duration-to-sleep (if (clsql:time< now today)
                                                           (clsql:time-difference now today)
                                                           (clsql:time-difference now tomorrow))))
                                
                                (sleep (clsql:duration-reduce duration-to-sleep :second)))
                              (parse (poll))))
                         :name "news polling thread"))

(defun start ()
  (setf clsql:*db-auto-sync* t)
  (clsql:enable-sql-reader-syntax)
  (unless (clsql:probe-database *clsql-db-spec* :database-type :postgresql)
    (initialize-empty-database *clsql-db-spec* :database-type :postgresql))
  (clsql:connect *clsql-db-spec* :pool t :database-type :postgresql))

(defun initialize-empty-database (db-spec &key database-type)
  (clsql:create-database db-spec :database-type database-type)
  (clsql:with-database (clsql:*default-database* db-spec :pool t :database-type database-type)
    (clsql:create-view-from-class 'cluster-time)))