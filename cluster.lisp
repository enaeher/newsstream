(in-package :newshole)

(defclass cluster-time ()
  ((cluster-id    :initarg :cluster-id
                  :reader cluster-id
                  :col-type string)
   (received-time :initarg :received-time
                  :reader received-time
                  :col-type date)
   (quantity      :initarg :quantity
                  :initform 0
                  :reader quantity
                  :col-type integer)
   (title        :initarg :title
                 :accessor title
                 :col-type string)
   (publication  :initarg :publication
                 :accessor publication
                 :col-type string
                 :documentation "Just a string for now; no authority control.")
   (uri          :initarg :uri
                 :accessor uri
                 :col-type string)
   (category     :initarg :category
                 :accessor category
                 :col-type string
                 :documentation "Just a string for now; no authority control."))
  (:metaclass pomo:dao-class)
  (:keys cluster-id received-time))

(defun make-cluster-time (&rest rest)
  (pomo:save-dao (apply #'make-instance (cons 'cluster-time rest))))

(defun get-clusters-in-order (start-time end-time)
  "Retrieve a list of cluster IDs in order of first appearance in the
Atom feed."
  (delete-duplicates
   (apply #'nconc
          (loop for (category) in (pomo:query (:order-by (:select (:distinct 'category)
                                                                  :from 'cluster-time)
                                                         'category))
             collect
             (mapcar #'car
                     (pomo:query (:order-by 
                                  (:select 'cluster-id
                                           :from 'cluster-time
                                           :where (:and (:= 'category category)
                                                        (:between 'received-time start-time end-time))
                                           :group-by 'cluster-id)
                                  (:desc (:min 'received-time)))))))
   :from-end t
   :test 'string=))