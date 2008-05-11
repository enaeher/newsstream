(in-package :newshole)

;;; We want to generate an SVG graph explaining the changing
;;; popularity of news stories (for which Google News clusters
;;; stand in here) over time. In this graph, time is represented
;;; on the x axis. The y axis represents 100% of news coverage;
;;; each story is displayed as a colored band the width of which
;;; represents its percentage of coverage at a given point in time.
;;;
;;; The Google News Atom feed is polled every ten minutes; purely
;;; for aesthetic purposes we perform a naive interpolation to
;;; fit a curve to the resulting points on the graph. This
;;; interpolation should not be considered as having any mathematical
;;; validity.
;;;
;;; There is almost certainly a better method of presenting this
;;; information than we've used here, as this method is somewhat
;;; misleading with its y axis; future revision may be required.

(defun wall-time-to-x-offset (wall-time end-time)
  "Derives an SVG x coordinate from WALL-TIME using *SECONDS-PER-UNIT*, where
x = 0 is the current time."
                                        ; CLSQL durations are unsigned, but the coordinate should always be negative
  (- (max
      1 ;;; err divide by zero wot?
      (/ (clsql:duration-reduce (clsql:time-difference wall-time end-time) :second) 
         *seconds-per-unit*))))

(defun quantity-to-y-offset (quantity &optional total)
  "Derives an SVG y coordinate from QUANTITY using *STORIES-PER-UNIT*."
  #-(and)
  (- (/ quantity *stories-per-unit*))
  (- (* (/ quantity total) *units-per-whole*)))

(flet ((x (point) (car point)) ; to make things a little easier to read....
       (y (point) (cdr point)))

  (defun get-coordinates-for-cluster (cluster-id start-time end-time)
    "Retrieve a list of cons cells the car and cdr of which represent the x and
y offsets for each occurrence of CLUSTER-ID between START-TIME and END-TIME."
    (trim-sequence-if
     #'zerop 
     (mapcar (lambda (cluster-time)
               (cons (wall-time-to-x-offset (clsql:parse-timestring (elt cluster-time 0)) end-time)
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

  (defun points-to-rects (cluster-id points return-points)
    (let ((color (get-color cluster-id)))
      (loop 
         for (this next) on points by #'cdr
         for (previous-this previous-next) on return-points by #'cdr
         when (and this next previous-this previous-next)
         collecting
           #-(and)
           (cxml:with-element "svg:path"
             (cxml:attribute "stroke" (format nil "#~x" color))
             (cxml:attribute "d"
                             (let ((start (cons
                                           (x this)
                                           (/ (+ (y this) (y previous-this))
                                              2)))
                                   (end (cons       
                                         (x next)
                                         (/ (+ (y next) (y previous-next))
                                        2)))
                                   (radius 10))
                               (format nil "M ~,1f,~,1f C ~,1f,~,1f ~,1f,~,1f ~,1f,~,1f"
                                       (+ (x start) radius)
                                       (y start)
                                       (+ (x start) (* 2 radius))
                                       (y end)
                                       (- (x end) (* 2 radius))
                                       (y start)
                                       (- (x end) radius)
                                       (y end)))))
           (cxml:with-element "svg:line"
                      (cxml:attribute "stroke" (format nil "#~x" color))
                      (cxml:attribute "x1" (format nil "~,1f" (+ 15 (x this))))
                      (cxml:attribute "y1" (format nil "~,2f" (/ (+ (y this) (y previous-this))
                                                                 2)))
                      (cxml:attribute "x2" (format nil "~,1f" (- (x next) 15)))
                      (cxml:attribute "y2" (format nil "~,2f" (/ (+ (y next) (y previous-next))
                                                                 2)))))
      (loop 
         for center in points
         for previous-center in return-points
           do
           (cxml:with-element "svg:rect"
                      (cxml:attribute "fill" (format nil "#~x" color))
                      (cxml:attribute "x" (format nil "~,1f" (- (x center) 15)))
                      (cxml:attribute "y" (format nil "~,1f" (y center)))
                      (cxml:attribute "width" "30")
                      (cxml:attribute "rx" "1")
                      (cxml:attribute "ry" "30")
                      (cxml:attribute "height" (format nil "~,1f" (- (y previous-center) (y center) .5))))
           #-(and)
           (cxml:with-element "svg:circle"
             (cxml:attribute "fill" (format nil "#~x" color))
             (cxml:attribute "cx" (format nil "~,1f" (x center)))
             (cxml:attribute "cy" (format nil "~,2f" (/ (+ (y center) (y previous-center))
                                                        2)))
             
             (cxml:attribute "r" (format nil "~,2f"   (/ (- (y previous-center) (y center))
                                                                2)
#-(and)                                         (* 3 (sqrt  (/ (/ (- (y previous-center) (y center))
                                                           2)
                                                        pi)))))))))
 
  
  (defun simplest-svg-chart (stream start-time end-time)
    (let ((sink (cxml:make-character-stream-sink stream :indentation 2)))
      (cxml:with-xml-output sink
        (sax:processing-instruction sink "xml-stylesheet" "type=\"text/css\" href=\"newsstream.css\"")
        (cxml:with-namespace ("xlink" "http://www.w3.org/1999/xlink")
          (cxml:with-namespace ("svg" "http://www.w3.org/2000/svg")
            (cxml:with-element "svg:svg"
              (cxml:attribute "viewBox" (format nil "~,1f -400 ~,1f 500"
                                                (- (/ 561600 *seconds-per-unit*))
                                                (/ 604800 *seconds-per-unit*)))
              (cxml:attribute "preserveAspectRatio" "xMidYMid")
              (cxml:attribute "version" "1.1")              
              (let ((adjusted-baseline (make-hash-table)))
                (mapcar (lambda (cluster-id)
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
                                  (points-to-rects cluster-id                                                
                                                   adjusted-coordinates
                                                   return-path))
                                (loop for ((title publication time)) on (clsql:select [title] [publication] [min [received-time]]
                                                                                      :from [cluster-time]
                                                                                      :where [and [= [cluster-id] cluster-id]
                                                                                                  [between [received-time] start-time end-time]]
                                                                                      :group-by (list [title] [publication]))
                                   do
                                   (let ((units-per-day (/ 86400 *seconds-per-unit*)))
                                     (cxml:with-element "svg:text"
                                       (cxml:attribute "class" "story-title")
                                       (cxml:attribute "x" (format nil "~,1f" (- (wall-time-to-x-offset (clsql:parse-timestring time) end-time)
                                                                                 (/ units-per-day 3))))
                                       (cxml:attribute "y" (- (+ *units-per-whole* 100)))
                                       (let ((last-wrap-point 0))
                                         (loop while (<= last-wrap-point (length title))
                                            do
                                            (cxml:with-element "svg:tspan"
                                              (cxml:attribute "x" (format nil "~,1f" (- (wall-time-to-x-offset (clsql:parse-timestring time) end-time)
                                                                                        (/ units-per-day 3))))
                                              (cxml:attribute "dy" "8")
                                              (let ((wrap-point
                                                     (or
                                                      (position
                                                       #\Space title
                                                       :from-end t
                                                       :start last-wrap-point
                                                       :end (min
                                                             (length title)
                                                             (+ last-wrap-point (floor (/ units-per-day 4)))))
                                                      (position #\Space title :start last-wrap-point)
                                                      (length title))))
                                                (cxml:unescaped (subseq title last-wrap-point (min (1+ wrap-point) (length title))))
                                                (setf last-wrap-point (1+ wrap-point))))))
                                       (cxml:with-element "svg:tspan"
                                         (cxml:attribute "x" (format nil "~,1f" (- (wall-time-to-x-offset (clsql:parse-timestring time) end-time)
                                                                                   (/ units-per-day 3))))
                                         (cxml:attribute "dy" "8")
                                         (cxml:attribute "font-size" "6")
                                         (cxml:text publication))))
                                   
                                   )))))
                        (get-clusters-in-order start-time end-time)))  
              (draw-category-labels)
              (draw-day-labels start-time end-time)
              (values)))))))

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
         (cxml:attribute "font-family" "Century Gothic, sans-serif")
         (cxml:attribute "fill" "white")
         (cxml:attribute "font-size" "7")
         (cxml:text (string-downcase category)))))



  (defun draw-day-labels (start-time end-time)
    (loop
       for (time) in (clsql:select [distinct [received-time]]
                                   :from [cluster-time]
                                   :where [between [received-time] start-time end-time])
       and y = (- *units-per-whole*)
       do
       (let* ((time (clsql:parse-timestring time))
              (x (wall-time-to-x-offset time end-time)))
         (cxml:with-element "svg:text"
           (cxml:attribute "x" (format nil "~,1f" x))
           (cxml:attribute "y" (format nil "~,1f" (- y 5)))
; (cxml:attribute "transform" (format nil "rotate(45 ~,1f ~,1f)" x y))
           (cxml:attribute "font-family" "Century Gothic, sans-serif")
           (cxml:attribute "fill" "white")
           (cxml:attribute "text-anchor" "middle")
           (cxml:attribute "font-size" "7")
           (cxml:text (format nil "~d ~a ~d"
                              (clsql:time-element time :day-of-month)
                              (string-downcase (clsql:month-name (clsql:time-element time :month)))
                              (clsql:time-element time :year)))))))

  (defun chart-for-past-week (stream)
    (simplest-svg-chart stream                     
                        (clsql:parse-timestring (caar (clsql:select [max [received-time]] :from [cluster-time] :where [< [received-time] (clsql:time- (clsql:get-time) (clsql:make-duration :day 6))]))) 
                        (clsql:parse-timestring (caar (clsql:select [max [received-time]] :from [cluster-time]))))))