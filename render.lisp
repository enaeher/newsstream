(in-package :newshole)

;;; We want to generate an SVG graph explaining the changing
;;; popularity of news stories (for which Google News clusters
;;; stand in here) over time. In this graph, time is represented
;;; on the x axis. The y axis represents 100% of news coverage;
;;; each story is displayed as a colored band the width of which
;;; represents its percentage of coverage at a given point in time.
;;;
;;; The Google News Atom feed is polled every day.

(defun wall-time-to-x-offset (wall-time end-time)
  "Derives an SVG x coordinate from WALL-TIME using *SECONDS-PER-UNIT*, where
x = 0 is the current time."
  ;; CLSQL durations are unsigned, but the coordinate should always be negative
  (- (* (clsql:duration-reduce (clsql:date-difference wall-time end-time) :day) 
        *units-per-day*)))

(defun quantity-to-y-offset (quantity &optional total)
  "Derives an SVG y coordinate from QUANTITY using *STORIES-PER-UNIT*."
  (- (* (/ quantity total) *units-per-whole*)))

(flet ((x (point) (car point)) ; to make things a little easier to read....
       (y (point) (cdr point)))

  (defun get-coordinates-for-cluster (cluster-id start-time end-time)
    "Retrieve a list of cons cells the car and cdr of which represent the x and
y offsets for each occurrence of CLUSTER-ID between START-TIME and END-TIME."
    (trim-sequence-if
     #'zerop 
     (mapcar (lambda (cluster-time)
               (cons (wall-time-to-x-offset (clsql:parse-datestring (elt cluster-time 0)) end-time)
                     (quantity-to-y-offset (or (elt cluster-time 1) 0)
                                           (elt cluster-time 2))))
             (clsql:select ["cluster" received-time]
                           ["inner-cluster" quantity]
                           ["(select sum(quantity) from cluster_time where received_time = \"cluster\".received_time)"]
                           :from      [cluster-time "cluster"]	
                           :left-join [cluster-time "inner-cluster"]
                           :on        [and [= ["cluster" received-time]
                           ["inner-cluster" received-time]]
                           [= ["inner-cluster" cluster-id]
                           cluster-id]]
                           :where     [between ["cluster" received-time] start-time end-time]
                           :group-by  '(["cluster" received-time] ["inner-cluster" quantity])
                           :order-by  ["cluster" received-time]))
                      :key #'y))  

  (let (lightp)
    (defun get-color (cluster-id)
      (let ((cluster (caar (clsql:select 'cluster-time
                                         :where [= [cluster-time cluster-id] cluster-id]
                                         :order-by '(([received-time] :desc))
                                         :limit 1))))
        (setf lightp (null lightp))
        (if-bind (color (getf *category-colors*
                              (intern (string-upcase (category cluster)) :keyword)))
            (+ color (if lightp #x151515 0)) ; alternate lighter and darker 
            "black"))))

  (defun adjust-baseline (coordinates adjusted-baseline)
    "with side effects"
    (loop for (x . y) in coordinates
       collect (cons x (incf (gethash x adjusted-baseline 0) y))))

  (defun get-adjusted-baseline (coordinates adjusted-baseline)
    "side-effect free"
    (loop for (x . y) in coordinates
       collect (cons x (gethash x adjusted-baseline 0))))

  (defun draw-connecting-lines (color top-points bottom-points)
    (loop 
       for (this next) on top-points by #'cdr
       for (previous-this previous-next) on bottom-points by #'cdr
       when (and this next previous-this previous-next)
       do
       (cxml:with-element "svg:line"
         (cxml:attribute "stroke" (format nil "#~x" color))
         (cxml:attribute "x1" (format nil "~,1f" (+ 15 (x this))))
         (cxml:attribute "y1" (format nil "~,2f" (/ (+ (y this) (y previous-this))
                                                    2)))
         (cxml:attribute "x2" (format nil "~,1f" (- (x next) 15)))
         (cxml:attribute "y2" (format nil "~,2f" (/ (+ (y next) (y previous-next))
                                                    2))))))

  (defun draw-story-titles (cluster-id start-time end-time)
    (flet ((wrap-text (text &key x-offset character-width class unescaped)
             (let ((last-wrap-point 0))
               (loop while (<= last-wrap-point (length text))
                  do
                  (cxml:with-element "svg:tspan"
                    (when class
                      (cxml:attribute "class" class))
                    (cxml:attribute "x" (format nil "~,1f" x-offset))
                    (cxml:attribute "dy" "8")
                    (let ((wrap-point
                           (or
                            (position
                             #\Space text
                             :from-end t
                             :start last-wrap-point
                             :end (min
                                   (length text)
                                   (+ last-wrap-point character-width)))
                            (position #\Space text :start last-wrap-point)
                            (length text))))
                      (funcall (if unescaped #'cxml:unescaped #'cxml:text)
                               (subseq text last-wrap-point (min (1+ wrap-point) (length text))))
                      (setf last-wrap-point (1+ wrap-point))))))))
      (loop for ((title publication time quantity))
         on (clsql:select [title] [publication] [min [received-time]] [quantity]
                          :from [cluster-time]
                          :where [and [= [cluster-id] cluster-id]
                          [between [received-time] start-time end-time]]
                          :group-by (list [title] [publication] [quantity]))
         do
         (let ((x-offset (- (wall-time-to-x-offset (clsql:parse-datestring time) end-time)
                            (/ *units-per-day* 3)))) ; text occupies 2/3 of column width
           (cxml:with-element "svg:text"
             (cxml:attribute "class" "story-title")
             (cxml:attribute "x" (format nil "~,1f" x-offset))
             (cxml:attribute "y" (- (+ *units-per-whole* 100)))
             (wrap-text title :x-offset x-offset 
                        :character-width (floor (/ *units-per-day* 4))
                        :unescaped t)           
             (wrap-text publication :x-offset x-offset
                        :character-width (floor (/ *units-per-day* 4))
                        :class "story-publication-attribution")
             (cxml:with-element "svg:tspan"
               (cxml:attribute "class" "story-quantity")
               (cxml:attribute "x" (format nil "~,1f" x-offset))
               (cxml:attribute "dy" "12")
               (cxml:text (format nil "~D stories" quantity))))))))

  (defun draw-stories (color top-points bottom-points)
    (loop 
       for top in top-points
       for bottom in bottom-points
       do
       (cxml:with-element "svg:rect"
         (cxml:attribute "fill" (format nil "#~x" color))
         (cxml:attribute "x" (format nil "~,1f" (- (x top) 15)))
         (cxml:attribute "y" (format nil "~,1f" (y top)))
         (cxml:attribute "width" "30")
         (cxml:attribute "rx" "1")
         (cxml:attribute "ry" "30")
         (cxml:attribute "height"
                         (format nil "~,1f"
                                 (- (y bottom) (y top) .5))))))

(defun draw-cluster (cluster-id start-time end-time adjusted-baseline)
  (cxml:with-element "svg:a"
    (cxml:attribute "xlink:href"
                    (caar
                     (clsql:select [uri]
                                   :from [cluster-time]
                                   :where [= [cluster-id] cluster-id]
                                   :order-by (list (list [received-time] :desc)) :limit 1)))
    (cxml:with-element "svg:g"
      (cxml:attribute "class" "story")
      (when-bind (coordinates (get-coordinates-for-cluster cluster-id start-time end-time))
        (let* ((return-path (get-adjusted-baseline coordinates adjusted-baseline))
               (adjusted-coordinates (adjust-baseline coordinates adjusted-baseline)))
          (let ((color (get-color cluster-id)))
            (draw-stories color
                          adjusted-coordinates
                          return-path)
            (draw-connecting-lines color
                                   adjusted-coordinates
                                   return-path)))
        (draw-story-titles cluster-id start-time end-time)))))
 
  (defun draw-chart (stream start-time end-time)
    (let ((sink (cxml:make-character-stream-sink stream)))
      (cxml:with-xml-output sink
        (sax:processing-instruction sink "xml-stylesheet" "type=\"text/css\" href=\"newsstream.css\"")
        (cxml:with-namespace ("xlink" "http://www.w3.org/1999/xlink")
          (cxml:with-namespace ("svg" "http://www.w3.org/2000/svg")
            (cxml:with-element "svg:svg"
              (cxml:attribute "viewBox" (format nil "~,1f ~,1f ~,1f ~,1f"
                                                (- (* *units-per-day* 6.5)) ; x origin
                                                (- (* *units-per-whole* 1.625)) ; y origin
                                                (* *units-per-day* 7) ; width
                                                (* *units-per-whole* 2))) ; height
              
              (cxml:attribute "preserveAspectRatio" "xMidYMid")
              (cxml:attribute "version" "1.1")

              ;; Adobe doesn't believe in background-color

              (cxml:with-element "rect"
                (cxml:attribute "fill" "#1a1a17")
                (cxml:attribute "x" "-1500")
                (cxml:attribute "y" "-1500")
                (cxml:attribute "width" "3000")
                (cxml:attribute "height" "3000"))

              ;; self-promotion

              (cxml:with-element "svg:a"
                (cxml:attribute "xlink:href" "http://codeanddata.com")
                (cxml:with-element "svg:image"
                  (cxml:attribute "xlink:href" "./logotype.png")
                  (cxml:attribute "x" "-75")
                  (cxml:attribute "y" "-400")
                  (cxml:attribute "width" "100")
                  (cxml:attribute "height" "34")))

              (cxml:with-element "svg:a"
                (cxml:attribute "xlink:href" "http://codeanddata.com/newsstream-details")
                (cxml:with-element "svg:text"
                  (cxml:attribute "x" (format nil "~,1f" (- (* *units-per-day* 6.375))))
                  (cxml:attribute "y" "-395")
                  (cxml:text "about newsstream")))
              
              (let ((adjusted-baseline (make-hash-table)))
                (mapcar (lambda (cluster-id)
                          (draw-cluster cluster-id start-time end-time adjusted-baseline))
                        (get-clusters-in-order start-time end-time)))
              (draw-category-labels)
              (draw-day-labels start-time end-time)
              (draw-archive-links)
              (values)))))))

(defun draw-archive-links ()
  (cxml:with-element "svg:g"
    (cxml:attribute "class" "archives")
    (let ((y-offset 20))
      (cxml:with-element "svg:a"
        (cxml:attribute "xlink:href" ".")
        (cxml:with-element "svg:text"
          (cxml:attribute "y" y-offset)
          (cxml:attribute "x" (format nil "~,1f" (- (* *units-per-day* 6.375))))
          (cxml:text "last seven days")))
      (loop for ((year)) on (clsql:query
                             "SELECT DISTINCT date_part ('year', received_time) FROM cluster_time"
                             :result-types 'integer)
         do
         (incf y-offset 10)
         (cxml:with-element "svg:text"
           (cxml:attribute "x" (format nil "~,1f" (- (* *units-per-day* 6.375))))
           (cxml:attribute "y" y-offset)
           (cxml:text (princ-to-string year)))
         (loop for ((month)) on (clsql:query
                                 (format nil "SELECT DISTINCT date_part ('month', received_time)
                                                FROM cluster_time
                                               WHERE date_part ('year', received_time) = ~D
                                            ORDER BY date_part ('month', received_time) DESC" year)
                                 :result-types 'integer)
            initially (cxml:with-element "svg:tspan" (cxml:attribute "dy" "-8") (cxml:text " "))
            do
            (cxml:with-element "svg:text"
              (cxml:attribute "x" (format nil "~,1f" (+ (- (* *units-per-day* 6.375)) 25))) 
              (cxml:attribute "y" y-offset)
              (cxml:text (clsql:month-name (parse-integer month))))
            (let ((x-offset (+ (- (* *units-per-day* 6.375)) 50)))
              (loop for (week) on (clsql:query
                                   (format nil "SELECT DISTINCT date_part ('week', received_time)
                                                  FROM cluster_time
                                                 WHERE date_part ('month', received_time) = ~d" month)
                                   :result-types 'integer)
                 do
                 (when-bind (week-span
                             (clsql:query
                              (format nil
                                      "SELECT min (received_time), max (received_time)
                                     FROM cluster_time
                                    WHERE date_part ('year', received_time) = ~d
                                      AND date_part ('week', received_time) = ~d
                                   HAVING date_part ('month', min (received_time)) = ~d"
                                      year week month)))
                     
                   (destructuring-bind ((start end))
                       week-span
                     (cxml:with-element "svg:a"
                       (cxml:attribute "xlink:href" (remove #\- start))
                       (cxml:with-element "svg:text"                    
                         (cxml:attribute "x" (format nil "~,1f" x-offset))
                         (cxml:attribute "y" y-offset)
                         (cxml:text (format nil "~D to ~D"
                                            (clsql:date-element (clsql:parse-datestring start)
                                                                :day-of-month)
                                            (clsql:date-element (clsql:parse-datestring end)
                                                                :day-of-month))))))
                   (incf x-offset 32))))
            (incf y-offset 8)))))
  (values))

  (defun draw-category-labels ()
    (loop
       for (category color) on *category-colors* by #'cddr
       for iterator from (* -20 (/ (length *category-colors*) 2)) upto 0 by 20
       do
       (cxml:with-element "svg:rect"
         (cxml:attribute "fill" (format nil "#~x" color))
         (cxml:attribute "width" "10")
         (cxml:attribute "height" 10)
         (cxml:attribute "x" iterator)
         (cxml:attribute "y" 20)
         (cxml:attribute "rx" "1")
         (cxml:attribute "ry" "30"))
       (cxml:with-element "svg:text"
         (cxml:attribute "x" iterator)
         (cxml:attribute "y" 37)
         (cxml:attribute "transform" (format nil "rotate(45 ~d 37)" iterator))
         (cxml:text (string-downcase category))))
    (values))

  (defun draw-day-labels (start-time end-time)
    (loop
       for (time) in (clsql:select [distinct [received-time]]
                                   :from [cluster-time]
                                   :where [between [received-time] start-time end-time])
       and y = (- *units-per-whole*)
       do
       (let* ((time (clsql:parse-datestring time))
              (x (wall-time-to-x-offset time end-time)))
         (cxml:with-element "svg:text"
           (cxml:attribute "x" (format nil "~,1f" x))
           (cxml:attribute "y" (format nil "~,1f" (- y 5)))
           (cxml:attribute "class" "day-label")
           (cxml:text (format nil "~d ~a ~d"
                              (clsql:date-element time :day-of-month)
                              (string-downcase (clsql:month-name (clsql:date-element time :month)))
                              (clsql:date-element time :year))))))
    (values)))

(defun draw-all-charts ()
  (loop for ((start end)) on
       (clsql:query "SELECT min(received_time), max(received_time)
                       FROM cluster_time GROUP BY date_part ('week', received_time)")
       do
       (with-open-file (stream (merge-pathnames (format nil "~a.svg" (remove #\- start)))
                               :direction :output
                               :element-type 'character
                               :external-format :utf-8
                               :if-exists :supersede)         
         (draw-chart stream (clsql:parse-datestring start) (clsql:parse-datestring end))))
  (with-open-file (stream (merge-pathnames "newsstream.svg")
                          :direction :output
                          :element-type 'character
                          :external-format :utf-8
                          :if-exists :supersede)
    
    (let ((recent-times (clsql:select [distinct [received-time]]
                                      :from [cluster-time]
                                      :order-by (list (list [received-time] :desc))
                                      :limit 7)))
      (draw-chart stream
                  (clsql:parse-datestring (caar (last recent-times)))
                  (clsql:parse-datestring (caar recent-times)))))
  (values))