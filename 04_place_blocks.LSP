(defun c:INSERTPIPEPARTS (/ ss nodes item p dirs info blk ang count)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq nodes (pa-get-node-types ss))
      (setq count 0)

      (foreach item nodes
        (setq p (car item))
        (setq dirs (pa-get-node-dirs ss p 10.0))

        ;; ここが重要：Y45だけでなく、エルボ・チーズ・45曲げも判定
        (setq info (pa-dirs-to-insert-info dirs))

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