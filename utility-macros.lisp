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

(defun trim-sequence-if (predicate sequence &key key)
  (subseq sequence
          (or (position-if (complement predicate) sequence :key key) 0)
          (1+ (position-if (complement predicate) sequence :key key :from-end t))))