(in-package :newshole)

(defvar *scene* nil "cl-svg scene.")

;; Feed and DB configuration

(defvar *atom-feed-uris* '("http://news.google.com/news?ned=us&topic=w&output=atom"
				 "http://news.google.com/news?ned=us&topic=n&output=atom"
				 "http://news.google.com/news?ned=us&topic=b&output=atom"
				 "http://news.google.com/news?ned=us&topic=t&output=atom"
				 "http://news.google.com/news?ned=us&topic=m&output=atom"
				 "http://news.google.com/news?ned=us&topic=s&output=atom"
				 "http://news.google.com/news?ned=us&topic=e&output=atom"))
(defvar *pomo-db-spec* '("newsstream" "newsstream" "tabesce" "localhost"))

;; Scaling factors

(defvar *units-per-whole* 250 "Height of the chart.")
(defvar *units-per-day* 69 "Width of each day.")

;;

(defvar *category-colors* '(:business      #x530000
			    :entertainment #x5e2b50
			    :health        #x416101
			    :sci/tech      #x5e4b2b
			    :sports        #x2b5e5a
			    :u.s.          #xe2d90b
			    :nation        #xe2d90b
			    :world         #xcbca97)
  "Color values for cluster categories.")
