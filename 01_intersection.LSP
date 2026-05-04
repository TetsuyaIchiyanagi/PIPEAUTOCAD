;;; 02_judge_bend_join.lsp
;;; 曲点/合流点マーカー描画
;;; - 曲点: 円
;;; - 合流点: 三角

(vl-load-com)

(defun pbj:_to3d (pt /)
  (if (= (length pt) 2)
    (list (car pt) (cadr pt) 0.0)
    pt
  )
)

(defun pbj:_add-circle (pt radius / p3d)
  (setq p3d (pbj:_to3d pt))
  (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbCircle")
      (cons 10 p3d)
      (cons 40 radius)
    )
  )
)

(defun pbj:_add-triangle (pt size / p3d h p1 p2 p3)
  (setq p3d (pbj:_to3d pt))
  (setq h (* size 0.866025403784)) ; sqrt(3)/2
  (setq p1 (list (+ (car p3d) 0.0) (+ (cadr p3d) h) (caddr p3d)))
  (setq p2 (list (- (car p3d) size) (- (cadr p3d) h) (caddr p3d)))
  (setq p3 (list (+ (car p3d) size) (- (cadr p3d) h) (caddr p3d)))
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

(defun pbj:place-point-marker (pt pointType markerSize /)
  "pointType: 'bend または 'join"
  (cond
    ((eq pointType 'bend)
      (pbj:_add-circle pt markerSize)
    )
    ((eq pointType 'join)
      (pbj:_add-triangle pt markerSize)
    )
  )
)

(princ)
