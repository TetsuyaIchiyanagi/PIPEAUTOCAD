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

(defun pa-pipe-size-text-height ()
  (* 2.0 (getvar "LTSCALE"))
)

(defun pa-pipe-size-text-offset (txtH)
  (* txtH 0.6)
)

(defun pa-pipe-size-min-length (txtH)
  (* txtH 1.5)
)

(defun pa-pipe-size-midpoint (p1 p2)
  (list
    (/ (+ (car p1) (car p2)) 2.0)
    (/ (+ (cadr p1) (cadr p2)) 2.0)
    0.0
  )
)

(defun pa-pipe-size-text-info (p1 p2 mid offset / lineAng deg ang txtPt)
  ;; 線角度を取得
  (setq lineAng (angle p1 p2))
  (setq deg (* 180.0 (/ lineAng pi)))

  ;; 0～360に正規化
  (if (< deg 0.0)
    (setq deg (+ deg 360.0))
  )

  (cond
    ;; 横線：左から右
    ((or (< deg 22.5) (> deg 337.5)
         (and (> deg 157.5) (< deg 202.5)))
      (setq ang 0.0)
      (setq txtPt (list (car mid) (+ (cadr mid) offset) 0.0))
    )

    ;; 縦線：下から上
    ((or (and (> deg 67.5) (< deg 112.5))
         (and (> deg 247.5) (< deg 292.5)))
      (setq ang (/ pi 2.0))
      (setq txtPt (list (- (car mid) offset) (cadr mid) 0.0))
    )

    ;; 45° or 225° → 45°
    ((or (and (> deg 22.5) (< deg 67.5))
         (and (> deg 202.5) (< deg 247.5)))
      (setq ang (/ pi 4.0)) ; 45°
      (setq txtPt (polar mid (+ ang (/ pi 2.0)) offset))
    )

    ;; 135° or 315° → 315°
    ((or (and (> deg 112.5) (< deg 157.5))
         (and (> deg 292.5) (< deg 337.5)))
      (setq ang (* 7.0 (/ pi 4.0))) ; 315°
      (setq txtPt (polar mid (+ ang (/ pi 2.0)) offset))
    )

    ;; 念のため
    (T
      (setq ang 0.0)
      (setq txtPt (list (car mid) (+ (cadr mid) offset) 0.0))
    )
  )

  (list txtPt ang)
)

(defun pa-make-pipe-size-text (txtPt txtH size ang)
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

(defun pa-place-pipe-size-on-segment (p1 p2 size / mid txtH offset info minLen)
  (setq txtH (pa-pipe-size-text-height))
  (setq minLen (pa-pipe-size-min-length txtH))

  (if (> (distance p1 p2) minLen)
    (progn
      (setq mid (pa-pipe-size-midpoint p1 p2))
      (setq offset (pa-pipe-size-text-offset txtH))
      (setq info (pa-pipe-size-text-info p1 p2 mid offset))
      (pa-make-pipe-size-text (car info) txtH size (cadr info))
    )
  )
)

(defun pa-same-point-p (a b tol)
  (< (distance a b) tol)
)

(defun pa-add-unique-point (p pts tol / exists q)
  (setq exists nil)

  (foreach q pts
    (if (pa-same-point-p p q tol)
      (setq exists T)
    )
  )

  (if exists
    pts
    (cons p pts)
  )
)

(defun pa-line-point-param (p p1 p2 / dx dy)
  (setq dx (abs (- (car p2) (car p1))))
  (setq dy (abs (- (cadr p2) (cadr p1))))

  (if (>= dx dy)
    (if (= (car p1) (car p2))
      0.0
      (/ (- (car p) (car p1)) (- (car p2) (car p1)))
    )
    (if (= (cadr p1) (cadr p2))
      0.0
      (/ (- (cadr p) (cadr p1)) (- (cadr p2) (cadr p1)))
    )
  )
)

(defun pa-pipe-size-node-gap-tol (/ scale)
  (setq scale (pa-get-block-scale))

  (max 500.0 (* 2.0 scale))
)

(defun pa-point-line-distance (p p1 p2 / ab ap ab2 ratio q)
  (setq ab (mapcar '- p2 p1))
  (setq ap (mapcar '- p p1))
  (setq ab2 (apply '+ (mapcar '* ab ab)))

  (if (= ab2 0.0)
    (distance p p1)
    (progn
      (setq ratio (/ (apply '+ (mapcar '* ap ab)) ab2))
      (setq q
        (mapcar '+
          p1
          (mapcar '(lambda (x) (* x ratio)) ab)
        )
      )
      (distance p q)
    )
  )
)

(defun pa-block-node-on-line-p (nodePt p1 p2 tol gapTol / ratio)
  (setq ratio (pa-line-point-param nodePt p1 p2))

  (and
    (< (pa-point-line-distance nodePt p1 p2) tol)
    (or
      (and (>= ratio 0.0) (<= ratio 1.0))
      (< (distance nodePt p1) gapTol)
      (< (distance nodePt p2) gapTol)
    )
  )
)

(defun pa-nearest-block-node-to-line-end (endPt nodes p1 p2 tol gapTol / nearestDist nearestPt item nodePt nodeType d)
  (setq nearestDist nil)
  (setq nearestPt nil)

  (foreach item nodes
    (setq nodePt (car item))
    (setq nodeType (cadr item))

    (if (and (= nodeType "BLOCK")
             (pa-block-node-on-line-p nodePt p1 p2 tol gapTol)
        )
      (progn
        (setq d (distance endPt nodePt))

        (if (and (< d gapTol)
                 (or (null nearestDist) (< d nearestDist))
            )
          (progn
            (setq nearestDist d)
            (setq nearestPt nodePt)
          )
        )
      )
    )
  )

  nearestPt
)

(defun pa-sort-nodes-on-line (pts p1 p2)
  (vl-sort
    pts
    '(lambda (a b)
       (< (pa-line-point-param a p1 p2)
          (pa-line-point-param b p1 p2))
     )
  )
)

(defun pa-get-pipe-size-block-nodes (/ ss i e ed p result)
  (setq result '())
  (setq ss
    (ssget
      "_X"
      '(
        (0 . "INSERT")
        (2 . "z01051011B,z01051009B,z01051018B")
      )
    )
  )

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq ed (entget e))
        (setq p (cdr (assoc 10 ed)))
        (setq result (cons (list p "BLOCK") result))
        (setq i (1+ i))
      )
    )
  )

  result
)

(defun pa-get-nodes-on-line (nodes p1 p2 tol / pts item nodePt nodeType gapTol startNode endNode dStart dEnd)
  (setq pts '())
  (setq gapTol (pa-pipe-size-node-gap-tol))
  (setq startNode (pa-nearest-block-node-to-line-end p1 nodes p1 p2 tol gapTol))
  (setq endNode (pa-nearest-block-node-to-line-end p2 nodes p1 p2 tol gapTol))

  ;; 同じブロック中心で両端を置き換えると点が1個だけになるため、
  ;; 近い側だけをブロック中心にして、反対側はLINE端点を残す。
  (if (and startNode endNode (pa-same-point-p startNode endNode tol))
    (progn
      (setq dStart (distance p1 startNode))
      (setq dEnd (distance p2 endNode))

      (if (<= dStart dEnd)
        (setq endNode nil)
        (setq startNode nil)
      )
    )
  )

  (if startNode
    (setq pts (pa-add-unique-point startNode pts tol))
    (setq pts (pa-add-unique-point p1 pts tol))
  )

  (if endNode
    (setq pts (pa-add-unique-point endNode pts tol))
    (setq pts (pa-add-unique-point p2 pts tol))
  )

  (foreach item nodes
    (setq nodePt (car item))
    (setq nodeType (cadr item))

    (cond
      ((= nodeType "BLOCK")
        (if (pa-block-node-on-line-p nodePt p1 p2 tol gapTol)
          (setq pts (pa-add-unique-point nodePt pts tol))
        )
      )

      ((pa-point-on-seg2 nodePt p1 p2 tol)
        (setq pts (pa-add-unique-point nodePt pts tol))
      )
    )
  )

  (pa-sort-nodes-on-line pts p1 p2)
)

(defun pa-place-size-between-nodes (p1 p2 lineNodes size / prevPt nextPts nextPt count segIndex txtH minLen)
  (setq count 0)
  (setq segIndex 0)
  (setq txtH (pa-pipe-size-text-height))
  (setq minLen (pa-pipe-size-min-length txtH))

  (if (> (length lineNodes) 1)
    (progn
      (setq prevPt (car lineNodes))
      (setq nextPts (cdr lineNodes))

      (while nextPts
        (setq nextPt (car nextPts))
        (setq segIndex (1+ segIndex))
        (princ
          (strcat
            "\n[DBG PIPESIZEAUTO] segment "
            (itoa segIndex)
            " mid=("
            (rtos (/ (+ (car prevPt) (car nextPt)) 2.0) 2 2)
            ","
            (rtos (/ (+ (cadr prevPt) (cadr nextPt)) 2.0) 2 2)
            ") len="
            (rtos (distance prevPt nextPt) 2 2)
          )
        )
        (if (> (distance prevPt nextPt) minLen)
          (progn
            (pa-place-pipe-size-on-segment prevPt nextPt size)
            (setq count (1+ count))
          )
          (princ
            (strcat
              " skipped: length <= "
              (rtos minLen 2 2)
            )
          )
        )
        (setq prevPt nextPt)
        (setq nextPts (cdr nextPts))
      )
    )
  )

  count
)

(defun c:PIPESIZEAUTO (/ ss size nodes blockNodes allNodes i e ed p1 p2 lineNodes placed totalPlaced)

  (princ "\n[DBG PIPESIZEAUTO] start")

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (princ
        (strcat
          "\n[DBG PIPESIZEAUTO] selected LINE count: "
          (itoa (sslength ss))
        )
      )

      (setq size (getstring T "\n管径を入力 <100>: "))
      (if (= size "") (setq size "100"))

      (princ
        (strcat
          "\n[DBG PIPESIZEAUTO] pipe size: "
          size
        )
      )

      ;; INSERTPIPEPARTSと同じノード取得
      (setq nodes (pa-get-node-types ss))
      (setq blockNodes (pa-get-pipe-size-block-nodes))
      (setq allNodes (append nodes blockNodes))

      (princ
        (strcat
          "\n[DBG PIPESIZEAUTO] node count from pa-get-node-types: "
          (itoa (length nodes))
        )
      )

      (princ
        (strcat
          "\n[DBG PIPESIZEAUTO] target block node count: "
          (itoa (length blockNodes))
        )
      )

      (princ
        (strcat
          "\n[DBG PIPESIZEAUTO] total node count: "
          (itoa (length allNodes))
        )
      )

      ;; 各LINEを処理
      (setq i 0)
      (setq totalPlaced 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq ed (entget e))
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))

        (princ
          (strcat
            "\n[DBG PIPESIZEAUTO] line "
            (itoa (1+ i))
            " p1=("
            (rtos (car p1) 2 2)
            ","
            (rtos (cadr p1) 2 2)
            ") p2=("
            (rtos (car p2) 2 2)
            ","
            (rtos (cadr p2) 2 2)
            ")"
          )
        )

        ;; このLINE上のノードを取得
        (setq lineNodes (pa-get-nodes-on-line allNodes p1 p2 10.0))

        (princ
          (strcat
            "\n[DBG PIPESIZEAUTO] line nodes count: "
            (itoa (length lineNodes))
          )
        )

        ;; ノード間に文字を書く
        (setq placed (pa-place-size-between-nodes p1 p2 lineNodes size))
        (setq totalPlaced (+ totalPlaced placed))

        (princ
          (strcat
            "\n[DBG PIPESIZEAUTO] line placed count: "
            (itoa placed)
          )
        )

        (setq i (1+ i))
      )

      (princ
        (strcat
          "\n[DBG PIPESIZEAUTO] total placed count: "
          (itoa totalPlaced)
        )
      )
    )
    (princ "\n[DBG PIPESIZEAUTO] no LINE selected")
  )

  (princ "\n[DBG PIPESIZEAUTO] end")
  (princ)
)
