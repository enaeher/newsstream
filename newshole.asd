(defpackage :newshole-system (:use #:asdf #:cl))
(in-package :newshole-system)

;; this overrides a deranged attempt to load init files by CLSQL, but
;; that's all for the better, it's a freaking library wot?

(defmethod perform :after ((o load-op) (c (eql (asdf:find-system 'clsql))))
  (funcall (find-symbol "%ENABLE-SQL-READER-SYNTAX" (find-package 'clsql-sys))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'swank)
    (asdf:operate 'asdf:load-op 'swank)))

(defsystem :newshole
	:depends-on (:cxml :cl-ppcre :drakma :clsql :clsql-postgresql)
	:serial t
        :components
        ((:file "package")
         (:file "variables")
         (:file "utility")
         (:file "poll")
         (:file "cluster")
         (:file "render")
         (:file "start")
         (:file "init")))
