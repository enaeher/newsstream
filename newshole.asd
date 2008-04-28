(defpackage :newshole-system (:use #:asdf #:cl))
(in-package :newshole-system)

(defsystem :newshole
	:depends-on (:hunchentoot :cxml :drakma :clsql-postgresql)
	:serial t
        :components
        ((:file "package")
         (:file "variables")
         (:file "utility-macros")
         (:file "poll")
         (:file "cluster")
         (:file "start")))
