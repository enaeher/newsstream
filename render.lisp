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
  (- (/ quantity *stories-per-unit*))
  #-(and)
  (- (* (/ quantity total) *units-per-whole*)))

(flet ((x (point) (car point)) ; to make things a little easier to read....
       (y (point) (cdr point)))

  (defun get-coordinates-for-cluster (cluster-id start-time end-time)
    "Retrieve a list of cons cells the car and cdr of which represent the x and
y offsets for each occurrence of CLUSTER-ID between START-TIME and END-TIME."
    (flet ((remove-redundant-zeroes (tuples)
             "Return TUPLES in which tuples with quantity 0 have been removed if
they are preceded and succeeded by tuples of quantity 0, or are the first or last
tuple. FIXME."
             (loop for (previous current next) on tuples                  
                when (and previous current
                          (or (elt current 1)
                              (elt previous 1)
                              (when next
                                (elt next 1))))
                collect current)))
      (mapcar (lambda (cluster-time)
                (cons (wall-time-to-x-offset (clsql:parse-timestring (elt cluster-time 0)) end-time)
                      (quantity-to-y-offset (or (elt cluster-time 1) 0)
                                            (elt cluster-time 2))))
              (clsql:select ["cluster" received-time]
                            ["inner-cluster" quantity]

                            ["(select sum(quantity) from cluster_time where received_time = \"cluster\".received_time having max(quantity) > 100)"]
                            :from      [cluster-time "cluster"]	
                            :left-join [cluster-time "inner-cluster"]
                            :on        [and [= ["cluster" received-time]
                            ["inner-cluster" received-time]]
                            [= ["inner-cluster" cluster-id]
                            cluster-id]]
                            :where     [between ["cluster" received-time] start-time end-time]
                            :group-by  '(["cluster" received-time] ["inner-cluster" quantity])
                            :order-by  ["cluster" received-time]))))

  (defun generate-bezier-interpolation (points)
    "See http://www.antigrain.com/research/bezier_interpolation/"
    (flet ((find-length (first second)
             "Given two sets of coordinates, find the distance between them."
             (sqrt (+ (expt (- (x second) (x first)) 2)
                      (expt (- (y second) (y first)) 2)))))
      (loop for (previous current-start current-end next)
         ;; so we repeat the first point twice...
         on (cons (car points) points)
         while (and previous current-start current-end)
         collecting         
         (let* ( ;; ...and the last point
                (next                  (or next current-end))
                ;; L1, L2, L3 in diagrams on page
                (previous-start-length (find-length previous current-start))
                (start-end-length      (find-length current-start current-end))
                (end-next-length       (find-length current-end next))

                ;; L1/L2, L2/L3 "                "
                (first-ratio           (/ previous-start-length
                                          (+ previous-start-length start-end-length)))
                (second-ratio          (/ start-end-length
                                          (+ start-end-length end-next-length)))                  
                ;; A1, A2, A3   "                "
                (previous-midpoint (find-midpoint previous current-start))
                (current-midpoint (find-midpoint current-start current-end))
                (next-midpoint (find-midpoint current-end next))
                ;; B1, B2       "                "
                (first-ratio-point (point+ previous-midpoint
                                           (point* (point- current-midpoint previous-midpoint)
                                                   first-ratio)))
                (second-ratio-point (point+ current-midpoint
                                            (point* (point- next-midpoint current-midpoint)
                                                    second-ratio)))
                ;; Control points
                (first-control-point (point+
                                      (point+ first-ratio-point
                                              (point* (point- current-midpoint first-ratio-point)
                                                      .1))
                                      (point- current-start first-ratio-point)))
                (second-control-point (point+
                                       (point+ second-ratio-point
                                               (point* (point- current-midpoint second-ratio-point)
                                                       .1))
                                       (point- current-end second-ratio-point))))
           ;; we don't need the start point for each curve, since SVG has a "current location"           
           (list first-control-point second-control-point current-end)))))
  
  (defun find-midpoint (beginning end)
    "Find the midpoint between BEGINNING and END."
    (let ((total (point+ beginning end)))
      (cons
       (/ (x total) 2)
       (/ (y total) 2))))

  (defun point- (first second)
    (cons (- (x first) (x second))
          (- (y first) (y second))))

  (defun point+ (&rest points)
    (cons (apply #'+ (mapcar #'x points))
          (apply #'+ (mapcar #'y points))))

  (defun point* (point ratio)
    (cons (* (x point) ratio)
          (* (y point) ratio)))

  (defun point/ (point divisor)
    (cons (/ (x point) divisor)
          (/ (y point) divisor)))

  (defun path-centroid (path)
    (point/ (apply #'point+ path) (length path)))

  (defun simple-svg-chart (stream start-time end-time)
    (cxml:with-xml-output (cxml:make-character-stream-sink stream)
      (cxml:with-namespace ("svg" "http://www.w3.org/2000/svg")
        (cxml:with-element "svg:svg"
          (cxml:attribute "viewBox" "-1000 -1500 1000 1500")
          (cxml:attribute "version" "1.1")
          (mapcar (lambda (cluster-id)
                    (cxml:with-element "svg:path"
                      (cxml:attribute "stroke-width" "10")
                      (cxml:attribute "stroke" "black")
                      (cxml:attribute "fill" "none")
                      (let ((coordinates (get-coordinates-for-cluster cluster-id start-time end-time)))
                        (cxml:attribute "d"
                                        (format nil "M ~,1f,~,1f ~{L ~a ~}"
                                                (x (car coordinates))
                                                (y (car coordinates))
                                                (mapcar (lambda (coordinate-pair)
                                                          (format nil "~,1f,~,1f"
                                                                  (x coordinate-pair)
                                                                  (y coordinate-pair)))
                                                        coordinates))))))
                  (get-clusters-in-order))
          (values)))))

  (let (lightp)
    (defun get-color (cluster-id)
      (let ((cluster (caar (clsql:select 'cluster-time
                                         :where [= [cluster-time cluster-id] cluster-id]
                                         :order-by '(([received-time] :asc))
                                         :limit 1))))
        (setf lightp (null lightp))
        (if-bind (color (getf *category-colors*
                              (intern (string-upcase (category cluster)) :keyword)))
            (+ color (if lightp #x151515 0)) ; alternate lighter and darker 
            "black"))))

  (defun points-to-path (cluster-id start-point &rest points)
    (cxml:with-element "svg:path"
      (cxml:attribute "id" (format nil "~d" cluster-id))
      (cxml:attribute "stroke-width" ".5")
      (cxml:attribute "stroke" "black")
      (cxml:attribute "fill" (format nil "#~x" (get-color cluster-id)))
      (cxml:attribute "d"
                      (apply #'concatenate 
                             `(string
                               ,(format nil "M ~,1f,~,1f " (x start-point)
                                        (y start-point))
                               ,@(loop for point
                                    in (apply #'nconc points)
                                    collecting                                              
                                    ;; move to start-point, draw cubic Bezier curve to end-point
                                    (format nil "L ~,1f,~,1f"
                                            (x point) (y point)))
                               ,(format nil "Z"))))))

  (defun bezier-interpolation-to-path (cluster-id start-point &rest interpolations)
    (cxml:with-element "svg:path"
      (cxml:attribute "id" (format nil "~d" cluster-id))
      (cxml:attribute "stroke-width" ".5")
      (cxml:attribute "stroke" "black")
      (cxml:attribute "fill" (format nil "#~x" (get-color cluster-id)))
      (cxml:attribute "d"
                      (apply #'concatenate 
                             `(string
                               ,(format nil "M ~,1f,~,1f " (x start-point)
                                        (y start-point))
                               ,@(loop for (first-control-point
                                            second-control-point
                                            end-point)
                                    in (apply #'nconc interpolations)
                                    collecting                                              
                                    ;; move to start-point, draw cubic Bezier curve to end-point
                                    (format nil "C ~,1f,~,1f ~,1f,~,1f ~,1f,~,1f "
                                            (x first-control-point) (y first-control-point)
                                            (x second-control-point) (y second-control-point)
                                            (x end-point) (y end-point)))
                               ,(format nil "Z"))))))

  (defun adjust-baseline (coordinates adjusted-baseline)
    "with side effects"
    (loop for (x . y) in coordinates
       collect (cons x (incf (gethash x adjusted-baseline 0) y))))

  (defun get-adjusted-baseline (coordinates adjusted-baseline)
    "side-effect free"
    (loop for (x . y) in coordinates
       collect (cons x (gethash x adjusted-baseline 0))))

  (defun bezier-svg-chart (stream start-time end-time)
    (cxml:with-xml-output (cxml:make-character-stream-sink stream :indentation 2)
      (cxml:with-namespace ("xlink" "http://www.w3.org/1999/xlink")
        (cxml:with-namespace ("svg" "http://www.w3.org/2000/svg")
          (cxml:with-element "svg:svg"
            (cxml:attribute "width" "100%")
            (cxml:attribute "height" "100%")
            (cxml:attribute "style" "background-color: #1a1a1a; background-image: url('ephemera.png'); background-repeat: no-repeat; background-position: top right;")
            (cxml:attribute "viewBox" "-300 -400 300 500")
            (cxml:attribute "preserveAspectRatio" "xMaxYMid meet")
            (cxml:attribute "version" "1.1")
            (let ((adjusted-baseline (make-hash-table)))
              (mapcar (lambda (cluster-id)
                        ;; let's not pollute the DOM with empty paths...
                        (when-bind (coordinates (get-coordinates-for-cluster cluster-id start-time end-time))
                          (let* ((return-path (nreverse (get-adjusted-baseline coordinates adjusted-baseline)))
                                 (return-path (cons (cons 0 (y (car return-path))) return-path))
                                 (adjusted-coordinates (adjust-baseline coordinates adjusted-baseline)))
                            (bezier-interpolation-to-path cluster-id
                                                          (first adjusted-coordinates)
                                                          (generate-bezier-interpolation adjusted-coordinates)
                                                          (generate-bezier-interpolation return-path))
                            (when-bind (max-quantity
                                        (caar (clsql:select [max [quantity]]
                                                            :from [cluster-time]
                                                            :where [and [= [cluster-id] cluster-id]
                                                            [between [received-time] start-time end-time]])))
                              (when (> max-quantity 100)
                                (cxml:with-element "svg:path"
                                  (let ((left-edge (wall-time-to-x-offset start-time end-time))
                                        (lower-left-corner (car (last return-path)))
                                        (upper-left-corner (car adjusted-coordinates)))
                                    (cxml:attribute "d" 
                                                    (format nil "M ~,1f,~,1f C ~,1f,~,1f ~,1f,~,1f ~,1f,~,1f C ~,1f,~,1f ~,1f,~,1f ~,1f,~,1f"
                                                            ;; moveto
                                                            (- left-edge 5) (y lower-left-corner)
                                                            ;; bottom of {
                                                            (- left-edge 10) (y lower-left-corner)
                                                            (- left-edge 5) (/ (+ (y upper-left-corner)
                                                                                  (y lower-left-corner))
                                                                               2)
                                                            (- left-edge 10) (/ (+ (y upper-left-corner)
                                                                                   (y lower-left-corner))
                                                                                2)
                                                            ;; top of {
                                                            (- left-edge 5) (/ (+ (y upper-left-corner)
                                                                                  (y lower-left-corner))
                                                                               2)
                                                            (- left-edge 10) (y upper-left-corner)
                                                            (- left-edge 5) (y upper-left-corner))))
                                  (cxml:attribute "stroke-width" "1")
                                  (cxml:attribute "fill" "none")
                                  (cxml:attribute "stroke" "white")))))))
                      (get-clusters-in-order)))
            (draw-category-labels)
            (draw-day-labels start-time end-time)
            (values))))))

  (defun draw-category-labels ()
    (loop
       for (category color) on *category-colors* by #'cddr
       for iterator from (* -20 (/ (length *category-colors*) 2)) upto 0 by 20
       do
         (cxml:with-element "svg:rect"
           (cxml:attribute "stroke-width" "1")
           (cxml:attribute "stroke" "black")
           (cxml:attribute "fill" (format nil "#~x" color))
           (cxml:attribute "width" "10")
           (cxml:attribute "height" 10)
           (cxml:attribute "x" iterator)
           (cxml:attribute "y" 20))
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
       and y = (quantity-to-y-offset 1 1) ; eww
       do
         (let* ((time (clsql:parse-timestring time))
                (x (wall-time-to-x-offset time end-time)))
           (cxml:with-element "svg:text"
             (cxml:attribute "x" (format nil "~,1f" x))
             (cxml:attribute "y" (format nil "~,1f" y))
             (cxml:attribute "transform" (format nil "rotate(45 ~,1f ~,1f)" x y))
             (cxml:attribute "font-family" "Century Gothic, sans-serif")
             (cxml:attribute "fill" "white")
             (cxml:attribute "text-anchor" "end")
             (cxml:attribute "font-size" "7")
             (cxml:text (format nil "~d ~a ~d"
                                (clsql:time-element time :day-of-month)
                                (string-downcase (clsql:month-name (clsql:time-element time :month)))
                                (clsql:time-element time :year))))
           (cxml:with-element "svg:line"
             (cxml:attribute "x1" (format nil "~,1f" x))
             (cxml:attribute "y1" (format nil "~,1f" y))
             (cxml:attribute "x2" (format nil "~,1f" x))
             (cxml:attribute "y2" "0")
             (cxml:attribute "stroke" "black")
             (cxml:attribute "stroke-width" "1")))))

  (defun simpler-svg-chart (stream start-time end-time)
    (cxml:with-xml-output (cxml:make-character-stream-sink stream :indentation 2)
      (cxml:with-namespace ("xlink" "http://www.w3.org/1999/xlink")
        (cxml:with-namespace ("svg" "http://www.w3.org/2000/svg")
          (cxml:with-element "svg:svg"
            (cxml:attribute "width" "100%")
            (cxml:attribute "height" "100%")
            (cxml:attribute "style" "background-color: #1a1a1a; background-image: url('ephemera.png'); background-repeat: no-repeat; background-position: bottom left;")
            (cxml:attribute "viewBox" "-300 -400 300 500")
            (cxml:attribute "preserveAspectRatio" "xMaxYMin")
            (cxml:attribute "version" "1.1")
            (let ((adjusted-baseline (make-hash-table)))
              (mapcar (lambda (cluster-id)
                        ;; let's not pollute the DOM with empty paths...
                        (when-bind (coordinates (get-coordinates-for-cluster cluster-id start-time end-time))
                          (let* ((return-path (nreverse (get-adjusted-baseline coordinates adjusted-baseline)))
                                 (return-path (cons (cons 0 (y (car return-path))) return-path))
                                 (adjusted-coordinates (adjust-baseline coordinates adjusted-baseline)))
                            (points-to-path cluster-id
                                            (first adjusted-coordinates)
                                            adjusted-coordinates
                                            return-path)
                            (when-bind (max-quantity
                                        (caar (clsql:select [max [quantity]]
                                                            :from [cluster-time]
                                                            :where [and [= [cluster-id] cluster-id]
                                                            [between [received-time] start-time end-time]])))
                              (when (> max-quantity 1000)
                                (cxml:with-element "svg:path"
                                  (let ((left-edge (wall-time-to-x-offset start-time end-time))
                                        (lower-left-corner (car (last return-path)))
                                        (upper-left-corner (car adjusted-coordinates)))
                                    (cxml:attribute "d" 
                                                    (format nil "M ~,1f,~,1f C ~,1f,~,1f ~,1f,~,1f ~,1f,~,1f C ~,1f,~,1f ~,1f,~,1f ~,1f,~,1f"
                                                            ;; moveto
                                                            (- left-edge 5) (y lower-left-corner)
                                                            ;; bottom of {
                                                            (- left-edge 10) (y lower-left-corner)
                                                            (- left-edge 5) (/ (+ (y upper-left-corner)
                                                                                  (y lower-left-corner))
                                                                               2)
                                                            (- left-edge 10) (/ (+ (y upper-left-corner)
                                                                                   (y lower-left-corner))
                                                                                2)
                                                            ;; top of {
                                                            (- left-edge 5) (/ (+ (y upper-left-corner)
                                                                                  (y lower-left-corner))
                                                                               2)
                                                            (- left-edge 10) (y upper-left-corner)
                                                            (- left-edge 5) (y upper-left-corner))))
                                  (cxml:attribute "stroke-width" "1")
                                  (cxml:attribute "fill" "none")
                                  (cxml:attribute "stroke" "white")))))))
                      (get-clusters-in-order)))
            (values))))))

  )