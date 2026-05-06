(defun c:INSERTPIPEPARTS (/ ss lineSs partSs nodes item p dirs info blk ang count flowPt)

  (setq ss
    (ssget
      '(
        (-4 . "<OR")
          (0 . "LINE")
          (-4 . "<AND")
            (0 . "INSERT")
            (2 . "z02021001B")
          (-4 . "AND>")
        (-4 . "OR>")
      )
    )
  )

  (if ss
    (progn
      (setq lineSs (pa-filter-lines ss))
      (setq partSs (pa-filter-blocks-by-name ss "z02021001B"))
      (if (null lineSs)
        (setq lineSs (ssadd))
      )
      (setq flowPt (getpoint "\n流れ先の点を指定してください: "))
      (setq nodes (pa-get-node-types lineSs))
      (setq count 0)

      (foreach item nodes
        (setq p (car item))
        (setq dirs (pa-get-node-dirs lineSs p 10.0))
        (setq info (pa-dirs-to-insert-info dirs lineSs p flowPt))

        ;; ここが重要：Y45だけでなく、エルボ・チーズ・45曲げも判定

        (if info
          (progn
            (setq blk (car info))
            (setq ang (cadr info))

            (pa-insert-block p blk ang)
            (setq count (1+ count))
          )
        )
      )

      (pa-replace-01-to-16)

      (princ (strcat "\n挿入数: " (itoa count)))
    )
    (princ "\nLINEを選択してください。")
  )

  (princ)
)
(defun pa-filter-lines (ss / result i e)

  (setq result (ssadd))
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))

    (if (= (cdr (assoc 0 (entget e))) "LINE")
      (ssadd e result)
    )

    (setq i (1+ i))
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-filter-blocks-by-name (ss blockName / result i e ed)

  (setq result (ssadd))
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq ed (entget e))

    (if (and (= (cdr (assoc 0 ed)) "INSERT")
             (= (strcase (cdr (assoc 2 ed))) (strcase blockName))
        )
      (ssadd e result)
    )

    (setq i (1+ i))
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-replace-01-to-16 (/ ss lineSs i e ed p dirs ang)

  (princ "\n[DBG 01to16] start")

  ;; Get all z02021001B blocks.
  (setq ss
    (ssget "_X"
      '(
        (0 . "INSERT")
        (2 . "z02021001B")
      )
    )
  )

  (if ss
    (princ (strcat "\n[DBG 01to16] target blocks: " (itoa (sslength ss))))
    (princ "\n[DBG 01to16] target blocks: 0")
  )

  ;; Get all lines used for direction detection.
  (setq lineSs (ssget "_X" '((0 . "LINE"))))

  (if lineSs
    (princ (strcat "\n[DBG 01to16] lines: " (itoa (sslength lineSs))))
    (princ "\n[DBG 01to16] lines: 0")
  )

  (if (tblsearch "BLOCK" "z01051016B")
    (princ "\n[DBG 01to16] block z01051016B: found")
    (princ "\n[DBG 01to16] block z01051016B: NOT FOUND")
  )

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))

        (setq e (ssname ss i))
        (setq ed (entget e))
        (setq p (cdr (assoc 10 ed)))

        (princ
          (strcat
            "\n[DBG 01to16] item "
            (itoa (1+ i))
            " point=("
            (rtos (car p) 2 2)
            ","
            (rtos (cadr p) 2 2)
            ")"
          )
        )

        (if lineSs
          (setq dirs (pa-get-nearest-line-end-dir lineSs p))
          (setq dirs nil)
        )

        (princ
          (strcat
            "\n[DBG 01to16] dirs: "
            (if dirs (pa-dirs-to-string dirs) "nil")
          )
        )

        (setq ang (pa-dirs-to-01to16-angle dirs))

        (princ
          (strcat
            "\n[DBG 01to16] angle: "
            (if ang (rtos ang 2 0) "nil")
          )
        )

        (if ang
          (progn
            (pa-insert-block p "z01051016B" ang)
            (entdel e)
            (princ "\n[DBG 01to16] replaced")
          )
          (princ "\n[DBG 01to16] skipped: angle is nil")
        )

        (setq i (1+ i))
      )
    )
  )

  (princ "\n[DBG 01to16] end")
)

(defun pa-get-nearest-line-end-dir (lineSs p / i e p1 p2 d1 d2 minDist nearestPt dir)

  (setq i 0)
  (setq minDist nil)
  (setq nearestPt nil)

  (while (< i (sslength lineSs))
    (setq e (ssname lineSs i))
    (setq p1 (cdr (assoc 10 (entget e))))
    (setq p2 (cdr (assoc 11 (entget e))))

    (setq d1 (distance p p1))
    (setq d2 (distance p p2))

    (if (or (null minDist) (< d1 minDist))
      (progn
        (setq minDist d1)
        (setq nearestPt p1)
      )
    )

    (if (< d2 minDist)
      (progn
        (setq minDist d2)
        (setq nearestPt p2)
      )
    )

    (setq i (1+ i))
  )

  (if nearestPt
    (progn
      (setq dir (pa-dir8 p nearestPt))

      (princ
        (strcat
          "\n[DBG 01to16] nearest endpoint=("
          (rtos (car nearestPt) 2 2)
          ","
          (rtos (cadr nearestPt) 2 2)
          ") distance="
          (rtos minDist 2 2)
          " dir="
          (if dir dir "nil")
        )
      )

      (if dir
        (list dir)
        nil
      )
    )
    nil
  )
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
