(in-package :newshole)

(defun start ()
  (apply #'pomo:connect-toplevel *pomo-db-spec*)
  (poll)
  (draw-all-charts))

