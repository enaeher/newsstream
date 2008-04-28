(in-package :newshole)

(defun poll ()
  (drakma:http-request *atom-feed-uri* :redirect nil))

(defun parse (source)
  (let ((source (cxml:make-source source))
        ;; database writes are not instantaneous and all
        ;; entries from the same feed should share a received_time
        (received-time (clsql:get-time)))
    (loop while (klacks:find-element source "entry")
       for cluster = (klacks:serialize-element source (cxml-xmls:make-xmls-builder))
       do (store-entry cluster received-time))))

(defun store-entry (entry received-time)
  "Quick and dirty scrape to find pertinent info. -- FIXME"
  (format t "got here")
  (flet ((scrape-quantity (content)
           (ppcre:register-groups-bind (quantity)
               ("all ([0-9]+) news articles" (ppcre:regex-replace-all "," content ""))
             (when quantity
               (parse-integer quantity :radix 10)))))
    (let* ((title-string (car (nreverse (third entry))))
           (link         (cadaar (nreverse (fourth entry))))
           (id           (parse-integer (car (nreverse (ppcre:split "cluster=" (car (nreverse (fifth entry)))))) :radix 16))
           (category (car (nreverse (sixth entry))))
           (content (car (nreverse (ninth entry))))
           (quantity (scrape-quantity content)))
      (destructuring-bind (title publication &rest rest)
          ;; this will break if a title includes the string " - " as other than
          ;; a title/publication delimiter, but living with that is easier than
          ;; writing a non-broken regexp right now.
          (ppcre:split " - " title-string)
        (format t "~%Title: ~a~%   Publication: ~a, Link: ~a, Cluster ID: ~a, Qty: ~a, Category: ~a" title publication link id quantity category)
        (make-cluster-time :title title
                           :uri link
                           :publication publication
                           :cluster-id id
                           :category category
                           :quantity quantity
                           :received-time received-time)))))
