(in-package :newshole)

(defun poll ()
  (loop for uri in *atom-feed-uris*
       do
       (parse (drakma:http-request uri :redirect nil))))

(defun parse (source)
  (let ((source (cxml:make-source source)))
    (loop while (klacks:find-element source "entry")
       for cluster = (klacks:serialize-element source (cxml-xmls:make-xmls-builder))
       do (store-entry cluster (simple-date:universal-time-to-timestamp (get-universal-time))))))

(defun store-entry (entry received-time)
  "Quick and dirty scrape to find pertinent info. -- FIXME"
  (let ((entry (mapcar (lambda (element)
                         (cons (caar element)
                               (car (nreverse element))))
                       (rest (rest entry)))))
    (flet ((scrape-quantity (content)
             (ppcre:register-groups-bind (quantity)
                 ("all ([0-9]+) news articles" (ppcre:regex-replace-all "," content ""))
               (when quantity
                 (parse-integer quantity :radix 10))))
           (scrape-cluster-id (content)
             (ppcre:register-groups-bind (cluster-id)
                 ("ncl=([^&\"]*)" content)
               cluster-id)))
      (let* ((title-string (cdr (assoc "title" entry :test #'string=)))
             (link         (car (cdaddr (assoc "link" entry :test #'string=))))
             (category (cadadr (assoc "category" entry :test #'string=)))
             (content (cdr (assoc "content" entry :test #'string=)))
             (quantity (scrape-quantity content))
             (id (scrape-cluster-id content)))
        (destructuring-bind (title publication &rest rest)
            ;; this will break if a title includes the string " - " as other than
            ;; a title/publication delimiter, but living with that is easier than
            ;; writing a non-broken regexp right now.
            (ppcre:split " - " title-string)
          (declare (ignore rest))
          (format t "~%Cluster ID: ~a, Qty: ~a, Category: ~a" id quantity category)
          (make-cluster-time :title title
                             :uri link
                             :publication publication
                             :cluster-id id
                             :category (if (string= category "More Top Stories")
                                           "Top Stories"
                                           category)
                             :quantity quantity
                             :received-time received-time))))))

