(in-package :newshole)

;;; CSS styling element for SVG news map.

(defun svg-css-rules ()
  (cxml:with-element "defs"
    (cxml:with-element "style"
      (cxml:attribute "type" "text/css")
      (cxml:cdata
       "
svg {
  background-color: #1a1a1a;
}")
        )))