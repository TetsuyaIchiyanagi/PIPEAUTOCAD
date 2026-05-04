(defun pa-draw-circle (p)
  (entmakex
    (list
      '(0 . "CIRCLE")
      (cons 10 p)
      (cons 40 50.0)
    )
  )
)

(defun pa-draw-triangle (p / r p1 p2 p3)
  (setq r 70.0)
  (setq p1 (polar p (/ pi 2.0) r))
  (setq p2 (polar p (+ (/ pi 2.0) (* 2.0 (/ pi 3.0))) r))
  (setq p3 (polar p (+ (/ pi 2.0) (* 4.0 (/ pi 3.0))) r))

  (entmakex
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(90 . 3)
      '(70 . 1)
      (cons 10 (list (car p1) (cadr p1)))
      (cons 10 (list (car p2) (cadr p2)))
      (cons 10 (list (car p3) (cadr p3)))
    )
  )
)