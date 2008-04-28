(in-package :newshole)

(defmacro when-bind ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))