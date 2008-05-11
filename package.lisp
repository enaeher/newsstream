(in-package :cl-user)

(defpackage :newshole
  (:use :common-lisp
        :cl-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clsql:enable-sql-reader-syntax))
