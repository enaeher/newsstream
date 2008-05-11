(in-package :newshole)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clsql:enable-sql-reader-syntax))

(clsql:def-view-class cluster-time ()
  ((cluster-id    :initarg :cluster-id
                  :reader cluster-id
                  :type integer)
   (received-time :initarg :received-time
                  :reader received-time
                  :type clsql:date)
   (quantity      :initarg :quantity
                  :initform 0
                  :reader quantity
                  :type integer)
   (title        :initarg :title
                 :accessor title
                 :type string)
   (publication  :initarg :publication
                 :accessor publication
                 :type string
                 :documentation "Just a string for now; no authority control.")
   (uri          :initarg :uri
                 :accessor uri
                 :type string)
   (category     :initarg :category
                 :accessor category
                 :type string
                 :documentation "Just a string for now; no authority control.")))

(defun make-cluster-time (&rest rest)
  (apply #'make-instance (cons 'cluster-time rest)))

(defun get-clusters-in-order (start-time end-time)
  "Retrieve a list of cluster IDs in order of first appearance in the
Atom feed."
  (delete-duplicates
   (apply #'nconc
          (loop for category in (clsql:select [distinct [category]]
                                              :from [cluster-time]
                                              :order-by [category])
             collect
               (mapcar #'car
                       (clsql:select [cluster-id]
                                     :from [cluster-time]
                                     :group-by [cluster-id]
                                     :where [and [= [category] category]
                                                 [between [received-time] start-time end-time]]
                                     :order-by (list (list [min [received-time]] :desc))))))
   :from-end t))