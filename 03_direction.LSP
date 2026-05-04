(defun pa-get-node-dirs (ss p tol / dirs i e p1 p2)

  (setq dirs '())
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq p1 (cdr (assoc 10 (entget e))))
    (setq p2 (cdr (assoc 11 (entget e))))

    (cond
      ;; p が p1 に近い → p1からp2方向
      ((< (distance p p1) tol)
        (setq dirs (cons (pa-dir8 p p2) dirs))
      )

      ;; p が p2 に近い → p2からp1方向
      ((< (distance p p2) tol)
        (setq dirs (cons (pa-dir8 p p1) dirs))
      )

      ;; p が線の途中にある → 両方向
      ((pa-point-on-seg2 p p1 p2 tol)
        (setq dirs (cons (pa-dir8 p p1) dirs))
        (setq dirs (cons (pa-dir8 p p2) dirs))
      )
    )

    (setq i (1+ i))
  )

  (pa-unique-list2 dirs)
)
(defun pa-line-dirs-from-point (p p1 p2 tol / result)

  (setq result '())

  (cond
    ;; 始点付近
    ((< (distance p p1) tol)
      (setq result (cons (pa-dir8 p p2) result))
    )

    ;; 終点付近
    ((< (distance p p2) tol)
      (setq result (cons (pa-dir8 p p1) result))
    )

    ;; 線の途中
    (T
      (setq result (cons (pa-dir8 p p1) result))
      (setq result (cons (pa-dir8 p p2) result))
    )
  )

  (vl-remove-if 'null result)
)

(defun pa-dir8 (p1 p2 / dx dy adx ady)

  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq adx (abs dx))
  (setq ady (abs dy))

  (if (< (distance p1 p2) 1.0)
    nil
    (cond
      ;; 横方向
      ((> adx (* ady 3.0))
        (if (> dx 0) "R" "L")
      )

      ;; 縦方向
      ((> ady (* adx 3.0))
        (if (> dy 0) "U" "D")
      )

      ;; 斜め方向
      ((and (> dx 0) (> dy 0)) "RU")
      ((and (< dx 0) (> dy 0)) "LU")
      ((and (> dx 0) (< dy 0)) "RD")
      ((and (< dx 0) (< dy 0)) "LD")

      (T nil)
    )
  )
)

(defun pa-point-on-seg2 (p a b tol / ab ap ratio q ab2)

  (setq ab (mapcar '- b a))
  (setq ap (mapcar '- p a))
  (setq ab2 (apply '+ (mapcar '* ab ab)))

  (if (= ab2 0.0)
    nil
    (progn
      (setq ratio (/ (apply '+ (mapcar '* ap ab)) ab2))

      (if (and (>= ratio 0.0) (<= ratio 1.0))
        (progn
          (setq q
            (mapcar '+
              a
              (mapcar '(lambda (x) (* x ratio)) ab)
            )
          )
          (< (distance p q) tol)
        )
        nil
      )
    )
  )
)

(defun pa-unique-list2 (lst / result x)

  (setq result '())

  (foreach x lst
    (if (and x (not (member x result)))
      (setq result (cons x result))
    )
  )

  (reverse result)
)

(defun pa-dirs-to-string (dirs / s d)

  (setq s "")

  (foreach d dirs
    (if (= s "")
      (setq s d)
      (setq s (strcat s "," d))
    )
  )

  s
)

(defun c:CHECKDIR (/ ss nodes item p dirs txt)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq nodes (pa-get-node-types ss))

      (foreach item nodes
        (setq p (car item))
        (setq dirs (pa-get-node-dirs ss p 10.0))
        (setq txt (pa-dirs-to-string dirs))

        (entmakex
          (list
            '(0 . "TEXT")
            (cons 10 (list (+ (car p) 120.0) (+ (cadr p) 120.0) 0.0))
            (cons 40 120.0)
            (cons 1 txt)
            '(50 . 0.0)
          )
        )
      )

      (princ "\n方向判定文字を作図しました。")
    )
    (princ "\nLINEを選択してください。")
  )

  (princ)
)