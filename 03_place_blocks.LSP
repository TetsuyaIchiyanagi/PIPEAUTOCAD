(defun pa-draw-square (p / s p1 p2 p3 p4)
  (setq s 70.0)

  (setq p1 (list (- (car p) s) (- (cadr p) s) 0.0))
  (setq p2 (list (+ (car p) s) (- (cadr p) s) 0.0))
  (setq p3 (list (+ (car p) s) (+ (cadr p) s) 0.0))
  (setq p4 (list (- (car p) s) (+ (cadr p) s) 0.0))

  (entmakex
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)
      (cons 10 (list (car p1) (cadr p1)))
      (cons 10 (list (car p2) (cadr p2)))
      (cons 10 (list (car p3) (cadr p3)))
      (cons 10 (list (car p4) (cadr p4)))
    )
  )
)