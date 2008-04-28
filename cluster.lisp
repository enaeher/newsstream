(in-package :newshole)

(clsql:def-view-class cluster-time ()
  ((cluster-id    :initarg :cluster-id
                  :reader cluster-id
                  :type integer)
   (received-time :initarg :received-time
                  :reader received-time
                  :type clsql:wall-time)
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
#-(and)
(clsql:def-view-class cluster ()
  ((id           :initarg :id
                 :reader id
                 :type integer))
  (:documentation "Obviously useless for now, but leaving in place for future expansion."))

(defun make-cluster-time (&rest rest)
  (apply #'make-instance (cons 'cluster-time rest)))

(defun get-clusters-in-order ()
  "Retrieve a list of cluster IDs in order of first appearance in the
Atom feed."
  (mapcar #'car (clsql:select [cluster-id]
                              :from [cluster-time]
                              :group-by [cluster-id]
                              :having [> [max [quantity]] 100]
                              :order-by (list (list [min [received-time]] :desc))))
  ;; can't make up my mind
  #-(and)
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
                                     :where [and [= [category] category]]
;                                     [= category
;                                     ["(SELECT category FROM cluster_time AS ct_inner WHERE ct_inner.cluster_id = cluster_time.cluster_id ORDER BY ct_inner.received_time DESC LIMIT 1)"]]
                                     :having [> [max [quantity]] 100]
                                     :order-by (list (list [min [received-time]] :desc))))))))