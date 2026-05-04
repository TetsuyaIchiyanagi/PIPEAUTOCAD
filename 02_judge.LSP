<<<<<<< Updated upstream
(defun c:JUDGEPOINT (/ ss pts p dirs typ)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq pts (get-intersections ss))

      (foreach p pts
        (setq dirs (get-dirs ss p))
        (setq typ (classify-node dirs))

        (cond
          ((= typ "BEND") (draw-circle p))
          ((= typ "JOIN") (draw-triangle p))
        )
      )
    )
  )

  (princ)
)

(defun get-dirs (ss p / dirs i e p1 p2)
  (setq dirs '())
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq p1 (cdr (assoc 10 (entget e))))
    (setq p2 (cdr (assoc 11 (entget e))))

    (if (< (distance p p1) 1.0)
      (setq dirs (cons (dir p p2) dirs))
    )

    (if (< (distance p p2) 1.0)
      (setq dirs (cons (dir p p1) dirs))
    )

    (setq i (1+ i))
  )

  dirs
)

(defun dir (p1 p2 / dx dy)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))

  (cond
    ((> dx 0) "R")
    ((< dx 0) "L")
    ((> dy 0) "U")
    ((< dy 0) "D")
  )
)

(defun classify-node (dirs / n)
  (setq n (length dirs))

  (cond
    ((>= n 3) "JOIN")
    ((= n 2)
      (if (not (or (and (member "L" dirs) (member "R" dirs))
                   (and (member "U" dirs) (member "D" dirs))))
        "BEND"
      )
    )
  )
)
=======
;;; 02_judge_bend_join.lsp
;;; 曲点/合流点マーカー描画
;;; - 曲点: 円
;;; - 合流点: 三角
;;; - test

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
>>>>>>> Stashed changes
