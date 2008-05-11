(in-package :newshole)

;; Feed and DB configuration

(defparameter *atom-feed-uri* "http://news.google.com/?output=atom")
(defparameter *clsql-db-spec* '("localhost" "newshole" "eli" "tabesce"))

;; Scaling factors

(defparameter *stories-per-unit* 150 "Number of stories represented by each unit in the SVG y coordinate.")
(defparameter *units-per-whole* 250 "Number of stories represented by each unit in the SVG y coordinate.")
(defparameter *seconds-per-unit* 1500 "Number of seconds represented by each unit in the SVG x coordinate.")

;;

(defparameter *category-colors* '(:business      #x530000
                                  :elections     #xe2d90b
                                  :entertainment #x5e2b50
                                  :health        #x2b5e2b
                                  :sci/tech      #x5e4b2b
                                  :sports        #x2b5e5a
                                  :|TOP STORIES| #xea2c00 ; oof
                                  :u.s.          #x254d19
                                  :world         #xcbca97)
  "Color values for cluster categories.")
