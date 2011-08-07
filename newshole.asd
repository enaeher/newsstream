(defpackage :newshole-system (:use #:asdf #:cl))
(in-package :newshole-system)

(defsystem :newshole
	:depends-on (:cxml :cl-ppcre :drakma :postmodern :swank)
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
