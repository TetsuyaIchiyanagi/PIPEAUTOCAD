(defun pa-ms-gothic-text-style (/ styleName)
  (setq styleName "ＭＳ ゴシック")

  (if (not (tblsearch "STYLE" styleName))
    (entmakex
      (list
        '(0 . "STYLE")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbTextStyleTableRecord")
        (cons 2 styleName)
        '(70 . 0)
        '(40 . 0.0)
        '(41 . 1.0)
        '(50 . 0.0)
        '(71 . 0)
        '(42 . 0.0)
        (cons 3 "ＭＳ ゴシック")
        '(4 . "")
      )
    )
  )

  styleName
)

(defun c:PIPESIZE (/ e ed p1 p2 mid size txtH offset txtPt dx dy ang)

  (setq e (car (entsel "\n管径を記載するLINEを選択: ")))

  (if e
    (progn
      (setq ed (entget e))

      (if (= (cdr (assoc 0 ed)) "LINE")
        (progn
          (setq p1 (cdr (assoc 10 ed)))
          (setq p2 (cdr (assoc 11 ed)))

          (setq size (getstring T "\n管径を入力 <100>: "))
          (if (= size "") (setq size "100"))

          ;; 中点
          (setq mid
            (list
              (/ (+ (car p1) (car p2)) 2.0)
              (/ (+ (cadr p1) (cadr p2)) 2.0)
              0.0
            )
          )

          ;; 文字高さ：グローバル線種尺度の2倍
          (setq txtH (* 2.0 (getvar "LTSCALE")))

          ;; オフセット量：文字高さ基準
          (setq offset (* txtH 0.6))

          ;; 線方向判定
          (setq dx (- (car p2) (car p1)))
          (setq dy (- (cadr p2) (cadr p1)))

          (if (>= (abs dx) (abs dy))
            (progn
              ;; 横線：線の上、左から右に読める
              (setq ang 0.0)
              (setq txtPt (list (car mid) (+ (cadr mid) offset) 0.0))
            )
            (progn
              ;; 縦線：線の左、下から上に読める
              (setq ang (/ pi 2.0))
              (setq txtPt (list (- (car mid) offset) (cadr mid) 0.0))
            )
          )

          ;; 中央揃えTEXT作成
          (entmakex
            (list
              '(0 . "TEXT")

              ;; 画層
              (cons 8 "USR2")

              '(100 . "AcDbEntity")
              '(100 . "AcDbText")
              (cons 10 txtPt)
              (cons 40 txtH)
              (cons 1 size)
              (cons 50 ang)
              (cons 7 (pa-ms-gothic-text-style))
              '(72 . 1) ; 水平中央
              '(73 . 2) ; 垂直中央
              (cons 11 txtPt)
            )
          )
        )
        (princ "\nLINEを選択してください。")
      )
    )
  )

  (princ)
)
