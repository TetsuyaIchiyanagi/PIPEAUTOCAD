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

(defun pa-dirs-to-01to16-angle (dirs / d)

  (setq d (pa-sort-dirs dirs))

  (cond

    ;; 右
    ((member "R" d) 0.0)

    ;; 上
    ((member "U" d) 90.0)

    ;; 左
    ((member "L" d) 180.0)

    ;; 下
    ((member "D" d) 270.0)

    (T nil)
  )
)

(defun pa-get-cut-dirs (blkName dirs / d)

  (setq d (pa-sort-dirs dirs))

  (cond

    ;; 長尺エルボ
    ((= blkName "z01051009B")
      d
    )

    ;; 長尺チーズ
    ((member blkName '("z01051018B" "z01051011B"))

      (cond
        ((pa-same-dirs d '("L" "R" "U")) '("U"))
        ((pa-same-dirs d '("L" "R" "D")) '("D"))
        ((pa-same-dirs d '("U" "D" "L")) '("L"))
        ((pa-same-dirs d '("U" "D" "R")) '("R"))
        (T nil)
      )
    )

    (T nil)
  )
)

(defun pa-dirs-to-insert-info (dirs ss p flowPt / y45 elbow tee bend45)

  (setq y45 (pa-dirs-to-y45 dirs))
  (setq elbow (pa-dirs-to-elbow dirs ss p))
  (setq tee (pa-dirs-to-tee dirs ss p flowPt))
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
(defun pa-dirs-to-tee (dirs ss p flowPt / d minLen blk ang branchDir flowDir)

  (setq d (pa-sort-dirs dirs))
  (setq minLen (pa-get-min-length-at-node ss p 10.0))

  ;; 通常チーズ：最短長さ200以下
  (if (or (null minLen) (<= minLen 200.0))
    (setq blk "z01051007B")
    (progn
      ;; 長尺チーズ
      (setq branchDir (pa-get-tee-branch-dir d))
      (setq flowDir (pa-get-tee-flow-dir d p flowPt))
      (setq blk (pa-get-long-tee-block flowDir branchDir))
    )
  )

  (if (= blk "z01051007B")
    ;; 通常チーズはチーズ形状で角度を決める
    (cond
      ((pa-same-dirs d '("L" "R" "U")) (setq ang 0.0))
      ((pa-same-dirs d '("U" "D" "R")) (setq ang 270.0))
      ((pa-same-dirs d '("L" "R" "D")) (setq ang 180.0))
      ((pa-same-dirs d '("U" "D" "L")) (setq ang 90.0))
      (T (setq ang nil))
    )
    ;; 長尺チーズはブロックタイプごとに基準向きが異なる
    (cond
      ((= blk "z01051011B")
        (cond
          ((= flowDir "R") (setq ang 270.0))
          ((= flowDir "U") (setq ang 0.0))
          ((= flowDir "L") (setq ang 90.0))
          ((= flowDir "D") (setq ang 180.0))
          (T (setq ang nil))
        )
      )
      ((= blk "z01051018B")
        (cond
          ((= flowDir "R") (setq ang 270.0))
          ((= flowDir "U") (setq ang 0.0))
          ((= flowDir "L") (setq ang 90.0))
          ((= flowDir "D") (setq ang 180.0))
          (T (setq ang nil))
        )
      )
      (T (setq ang nil))
    )
  )

  (if (and blk ang)
    (list blk ang)
    nil
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

(defun pa-get-tee-branch-dir (dirs)

  (cond
    ((and (member "L" dirs) (member "R" dirs) (member "U" dirs)) "U")
    ((and (member "L" dirs) (member "R" dirs) (member "D" dirs)) "D")
    ((and (member "U" dirs) (member "D" dirs) (member "L" dirs)) "L")
    ((and (member "U" dirs) (member "D" dirs) (member "R" dirs)) "R")
    (T nil)
  )
)
(defun pa-get-flow-dir (p flowPt / dx dy)

  (setq dx (- (car flowPt) (car p)))
  (setq dy (- (cadr flowPt) (cadr p)))

  (if (> (abs dx) (abs dy))
    (if (> dx 0) "R" "L")
    (if (> dy 0) "U" "D")
  )
)
(defun pa-get-tee-flow-dir (dirs p flowPt / dx dy)

  (setq dx (- (car flowPt) (car p)))
  (setq dy (- (cadr flowPt) (cadr p)))

  (cond
    ;; 横主管
    ((and (member "L" dirs) (member "R" dirs))
      (if (> dx 0) "R" "L")
    )

    ;; 縦主管
    ((and (member "U" dirs) (member "D" dirs))
      (if (> dy 0) "U" "D")
    )

    (T
      (pa-get-flow-dir p flowPt)
    )
  )
)
(defun pa-get-long-tee-block (flowDir branchDir)

  (cond
    ;; 流れ右
    ((and (= flowDir "R") (= branchDir "U")) "z01051011B")
    ((and (= flowDir "R") (= branchDir "D")) "z01051018B")

    ;; 流れ左
    ((and (= flowDir "L") (= branchDir "U")) "z01051018B")
    ((and (= flowDir "L") (= branchDir "D")) "z01051011B")

    ;; 流れ下
    ((and (= flowDir "D") (= branchDir "R")) "z01051011B")
    ((and (= flowDir "D") (= branchDir "L")) "z01051018B")

    ;; 流れ上
    ((and (= flowDir "U") (= branchDir "R")) "z01051018B")
    ((and (= flowDir "U") (= branchDir "L")) "z01051011B")

    (T "z01051007B")
  )
)
