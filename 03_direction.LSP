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
(defun c:CHECKY45 (/ ss nodes item p dirs info txt)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq nodes (pa-get-node-types ss))

      (foreach item nodes
        (setq p (car item))
        (setq dirs (pa-get-node-dirs ss p 10.0))
        (setq info (pa-dirs-to-y45 dirs))

        (if info
          (progn
            (setq txt
              (strcat
                (car info)
                " / "
                (rtos (cadr info) 2 0)
                "deg"
              )
            )

            (entmakex
              (list
                '(0 . "TEXT")
                (cons 10 (list (+ (car p) 150.0) (- (cadr p) 150.0) 0.0))
                (cons 40 120.0)
                (cons 1 txt)
                '(50 . 0.0)
              )
            )
          )
        )
      )

      (princ "\n45度合流のブロック名と角度を表示しました。")
    )
    (princ "\nLINEを選択してください。")
  )

  (princ)
)
(defun pa-dirs-to-y45 (dirs / d)

  (setq d (pa-sort-dirs dirs))

  (cond
    ;; 水平主管
    ((pa-same-dirs d '("L" "R" "LD")) (list "z01051017B" 270.0))
    ((pa-same-dirs d '("L" "R" "RU")) (list "z01051017B" 90.0))
    ((pa-same-dirs d '("L" "R" "RD")) (list "z01051010B" 90.0))
    ((pa-same-dirs d '("L" "R" "LU")) (list "z01051010B" 270.0))

    ;; 縦主管
    ((pa-same-dirs d '("U" "D" "RD")) (list "z01051017B" 0.0))
    ((pa-same-dirs d '("U" "D" "LU")) (list "z01051017B" 180.0))
    ((pa-same-dirs d '("U" "D" "RU")) (list "z01051010B" 180.0))
    ((pa-same-dirs d '("U" "D" "LD")) (list "z01051010B" 0.0))

    (T nil)
  )
)
(defun pa-sort-dirs (dirs)
  (vl-sort dirs '<)
)

(defun pa-same-dirs (a b)
  (equal (pa-sort-dirs a) (pa-sort-dirs b))
)
(defun pa-dirs-to-insert-info (dirs ss p / y45 elbow tee bend45)

  (setq y45 (pa-dirs-to-y45 dirs))
  (setq elbow (pa-dirs-to-elbow dirs ss p))
  (setq tee (pa-dirs-to-tee dirs))
  (setq bend45 (pa-dirs-to-bend45 dirs))

  (cond
    (y45 y45)
    (elbow elbow)
    (tee tee)
    (bend45 bend45)
    (T nil)
  )
)
(defun pa-dirs-to-elbow (dirs ss p / d minLen blk angleTable angle item)

  (setq d (pa-sort-dirs dirs))

  ;; 長さ取得
  (setq minLen (pa-get-min-length-at-node ss p 10.0))

  ;; ブロック選定
  (if (and minLen (> minLen 200.0))
    (setq blk "z01051009B") ;; 長い
    (setq blk "z01051006B") ;; 短い
  )

  (setq angleTable
    (if (= blk "z01051009B")
      (list
        (list '("R" "U") 180.0)
        (list '("L" "U") 270.0)
        (list '("L" "D") 0.0)
        (list '("R" "D") 90.0)
      )
      (list
        (list '("R" "U") 270.0)
        (list '("L" "U") 0.0)
        (list '("L" "D") 90.0)
        (list '("R" "D") 180.0)
      )
    )
  )

  (foreach item angleTable
    (if (pa-same-dirs d (car item))
      (setq angle (cadr item))
    )
  )

  (if angle
    (list blk angle)
    nil
  )
)
(defun pa-dirs-to-tee (dirs / d)
  (setq d (pa-sort-dirs dirs))

  (cond
    ((pa-same-dirs d '("L" "R" "U")) (list "z01051007B" 0.0))
    ((pa-same-dirs d '("U" "D" "R")) (list "z01051007B" 270.0))
    ((pa-same-dirs d '("L" "R" "D")) (list "z01051007B" 180.0))
    ((pa-same-dirs d '("U" "D" "L")) (list "z01051007B" 90.0))
    (T nil)
  )
)
(defun pa-dirs-to-bend45 (dirs / d)
  (setq d (pa-sort-dirs dirs))

  (cond
    ((pa-same-dirs d '("R" "LU")) (list "z01051008B" 315.0))
    ((pa-same-dirs d '("U" "LD")) (list "z01051008B" 45.0))
    ((pa-same-dirs d '("L" "RD")) (list "z01051008B" 135.0))
    ((pa-same-dirs d '("D" "RU")) (list "z01051008B" 225.0))

    ((pa-same-dirs d '("R" "LD")) (list "z01051008B" 0.0))
    ((pa-same-dirs d '("D" "LU")) (list "z01051008B" 90.0))
    ((pa-same-dirs d '("L" "RU")) (list "z01051008B" 180.0))
    ((pa-same-dirs d '("U" "RD")) (list "z01051008B" 270.0))

    (T nil)
  )
)
(defun pa-get-min-length-at-node (ss p tol / i e p1 p2 len minLen)

  (setq i 0)
  (setq minLen nil)

  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq p1 (cdr (assoc 10 (entget e))))
    (setq p2 (cdr (assoc 11 (entget e))))

    ;; この交点に接続している線だけ対象
    (if (or (< (distance p p1) tol)
            (< (distance p p2) tol)
            (pa-point-on-seg2 p p1 p2 tol)
        )
      (progn
        (setq len (distance p1 p2))

        (if (or (null minLen) (< len minLen))
          (setq minLen len)
        )
      )
    )

    (setq i (1+ i))
  )

  minLen
)
