(in-package :newshole)

(defmacro when-bind ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))

(defmacro if-bind ((var test) then else)
  `(let ((,var ,test))
     (if ,var
       ,then
       ,else)))