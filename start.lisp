(in-package :newshole)

(defun configure ()
  (load-configuration)
  (apply #'pomo:connect-toplevel *pomo-db-spec*))

(defun begin ()
  (poll)
  (draw-all-charts))

