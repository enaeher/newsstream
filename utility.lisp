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
          (or (position-if (complement predicate) sequence :key key) (length sequence))
          (if-bind (end (position-if (complement predicate) sequence :key key :from-end t))
              (1+ end)
              (length sequence))))