(defun c:INSERTPIPEPARTS (/ ss nodes item p dirs info blk ang count flowPt)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq flowPt (getpoint "\n流れ先の点を指定してください: "))
      (setq nodes (pa-get-node-types ss))
      (setq count 0)

      (foreach item nodes
        (setq p (car item))
        (setq dirs (pa-get-node-dirs ss p 10.0))

        ;; ここが重要：Y45だけでなく、エルボ・チーズ・45曲げも判定
        (setq info (pa-dirs-to-insert-info dirs ss p flowPt))

        (if info
          (progn
            (setq blk (car info))
            (setq ang (cadr info))

            (pa-insert-block p blk ang)
            (setq count (1+ count))
          )
        )
      )

      (princ (strcat "\n挿入数: " (itoa count)))
    )
    (princ "\nLINEを選択してください。")
  )

  (princ)
)
(defun pa-insert-block (p blkName angleDeg / angleRad)

  ;; 度 → ラジアン
  (setq angleRad (* pi (/ angleDeg 180.0)))

  (if (tblsearch "BLOCK" blkName)
    (entmakex
      (list
        '(0 . "INSERT")
        (cons 2 blkName)
        (cons 10 p)

        ;; ★ スケールを100に変更
        (cons 41 100.0)
        (cons 42 100.0)
        (cons 43 100.0)

        (cons 50 angleRad)
      )
    )
    (princ (strcat "\nブロックが見つかりません: " blkName))
  )
)

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
