(defun draw-circle (p)
  (command "CIRCLE" p 50)
)

(defun draw-triangle (p / p1 p2 p3 r)
  (setq r 70)

  (setq p1 (polar p (/ pi 2) r))
  (setq p2 (polar p (+ (/ pi 2) (* 2 (/ pi 3))) r))
  (setq p3 (polar p (+ (/ pi 2) (* 4 (/ pi 3))) r))

  (command "PLINE" p1 p2 p3 "C")
)