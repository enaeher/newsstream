(in-package :newshole)

(defun start ()
  (load-configuration)
  (apply #'pomo:connect-toplevel *pomo-db-spec*)
  (poll)
  (draw-all-charts))

