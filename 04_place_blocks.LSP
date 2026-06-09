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
             (wcmatch (strcase (cdr (assoc 2 ed))) (strcase blockName))
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

(defun pa-rad-to-deg (ang)
  (* 180.0 (/ ang pi))
)

(defun pa-normalize-angle-deg (ang)
  (while (< ang 0.0)
    (setq ang (+ ang 360.0))
  )

  (while (>= ang 360.0)
    (setq ang (- ang 360.0))
  )

  ang
)

(defun pa-01to16-angle-from-block (ed / oldName angleRad)
  (setq oldName (strcase (cdr (assoc 2 ed))))
  (setq angleRad (cdr (assoc 50 ed)))

  (if (and angleRad
           (wcmatch oldName "Z01051001B,Z02021001B")
      )
    (pa-normalize-angle-deg (pa-rad-to-deg angleRad))
    nil
  )
)

(defun pa-replace-01-to-16 (targetSs sourceLineSs / ss lineSs allLineSs i e ed p dirs ang)

  (princ "\n[DBG 01to16] start")

  ;; Use only the blocks selected by the user.
  (setq ss targetSs)

  (if ss
    (princ (strcat "\n[DBG 01to16] selected target blocks: " (itoa (sslength ss))))
    (princ "\n[DBG 01to16] selected target blocks: 0")
  )

  ;; Use the selected center lines for direction detection.
  (setq lineSs sourceLineSs)
  (if (null lineSs)
    (setq lineSs (ssget "_X" '((0 . "LINE"))))
  )
  (setq allLineSs (ssget "_X" '((0 . "LINE"))))

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
        (setq ang nil)

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

        (if (and (null dirs) allLineSs)
          (progn
            (princ "\n[DBG 01to16] retry dirs with current drawing lines")
            (setq dirs (pa-get-nearest-line-end-dir allLineSs p))
          )
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
            (pa-insert-block p "z01051016B" ang 100.0)
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

(defun pa-cardinal-dir-p (dir)
  (member dir '("R" "U" "L" "D"))
)

(defun pa-01to16-endpoint-candidate-score (dist dir lineDir / score)
  (setq score dist)

  (if (not (pa-cardinal-dir-p dir))
    (setq score (+ score 100000.0))
  )

  (if (not (= dir lineDir))
    (setq score (+ score 10000.0))
  )

  score
)

(defun pa-01to16-consider-endpoint (p ep other best / dir lineDir dist score bestScore)
  (setq dir (pa-dir8 p ep))
  (setq lineDir (pa-dir8 ep other))
  (setq dist (distance p ep))
  (setq score (pa-01to16-endpoint-candidate-score dist dir lineDir))

  (princ
    (strcat
      "\n[DBG 01to16] candidate dir="
      (if dir dir "nil")
      " lineDir="
      (if lineDir lineDir "nil")
      " dist="
      (rtos dist 2 2)
      " score="
      (rtos score 2 2)
    )
  )

  (setq bestScore (if best (cadr best) nil))

  (if (or (null bestScore) (< score bestScore))
    (list dir score ep dist lineDir)
    best
  )
)

(defun pa-get-nearest-line-end-dir (lineSs p / i e ed p1 p2 best dir score nearestPt minDist lineDir)

  (setq i 0)
  (setq best nil)

  (while (< i (sslength lineSs))
    (setq e (ssname lineSs i))
    (setq ed (entget e))

    (if ed
      (progn
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))

        (if (and p1 p2)
          (progn
            (setq best (pa-01to16-consider-endpoint p p1 p2 best))
            (setq best (pa-01to16-consider-endpoint p p2 p1 best))
          )
        )
      )
    )

    (setq i (1+ i))
  )

  (if best
    (progn
      (setq dir (car best))
      (setq score (cadr best))
      (setq nearestPt (caddr best))
      (setq minDist (cadddr best))
      (setq lineDir (nth 4 best))

      (princ
        (strcat
          "\n[DBG 01to16] selected endpoint=("
          (rtos (car nearestPt) 2 2)
          ","
          (rtos (cadr nearestPt) 2 2)
          ") distance="
          (rtos minDist 2 2)
          " dir="
          (if dir dir "nil")
          " lineDir="
          (if lineDir lineDir "nil")
          " score="
          (rtos score 2 2)
        )
      )

      (if (pa-cardinal-dir-p dir)
        (list dir)
        nil
      )
    )
    nil
  )
)

(defun pa-get-block-scale (/ ltScale)

  (setq ltScale (getvar "LTSCALE"))

  ;; Linear interpolation:
  ;; LTSCALE 50.0  -> insert scale 75.0
  ;; LTSCALE 100.0 -> insert scale 100.0
  ;; LTSCALE 200.0 -> insert scale 150.0
  (+ 50.0 (* 0.5 ltScale))
)

(defun pa-queue-line-cut (p gap cutDirs)
  (setq pa-line-cut-queue (cons (list p gap cutDirs pa-active-line-ss) pa-line-cut-queue))
)

(defun pa-apply-deferred-line-cuts (/ item)
  (princ
    (strcat
      "\n[DBG INSERTPIPEPARTS] deferred line cuts: "
      (itoa (length pa-line-cut-queue))
    )
  )

  (foreach item (reverse pa-line-cut-queue)
    (pa-cut-lines-around-point (car item) (cadr item) (caddr item) (nth 3 item))
  )

  (setq pa-line-cut-queue '())
)

(defun pa-line-matches-cut-dirs-p (p p1 p2 cutDirs / lineDirs d matched)

  (setq lineDirs (pa-line-dirs-from-point p p1 p2 10.0))
  (setq matched nil)

  (foreach d lineDirs
    (if (member d cutDirs)
      (setq matched T)
    )
  )

  matched
)

(defun pa-cut-line-segment (a b)

  (if (> (distance a b) 1.0)
    (entmakex
      (list
        '(0 . "LINE")
        (cons 10 a)
        (cons 11 b)
      )
    )
  )
)

(defun pa-closest-point-on-line (p a b / ab ap ab2 ratio)

  (setq ab (mapcar '- b a))
  (setq ap (mapcar '- p a))
  (setq ab2 (apply '+ (mapcar '* ab ab)))

  (if (= ab2 0.0)
    a
    (progn
      (setq ratio (/ (apply '+ (mapcar '* ap ab)) ab2))
      (mapcar '+
        a
        (mapcar '(lambda (x) (* x ratio)) ab)
      )
    )
  )
)

(defun pa-cut-lines-around-point (p gap cutDirs targetSs / ss i e ed p1 p2 q dir1 dir2 cutSide1 cutSide2 cut1 cut2 tol)

  (setq tol 25.0)

  (if cutDirs
    (progn
      (setq ss targetSs)
      (if (null ss)
        (setq ss (ssget "_X" '((0 . "LINE"))))
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq e (ssname ss i))
            (setq ed (entget e))
            (setq p1 (cdr (assoc 10 ed)))
            (setq p2 (cdr (assoc 11 ed)))

            (cond
              ((pa-point-on-seg2 p p1 p2 tol)
              (progn
                (setq q (pa-closest-point-on-line p p1 p2))
                (setq dir1 (pa-dir8 q p1))
                (setq dir2 (pa-dir8 q p2))
                (setq cutSide1 (and dir1 (member dir1 cutDirs)))
                (setq cutSide2 (and dir2 (member dir2 cutDirs)))

                (if (or cutSide1 cutSide2)
                  (progn
                    (entdel e)

                    ;; p -> p1 side
                    (if dir1
                      (progn
                        (setq cut1
                          (if cutSide1
                            (polar q (angle q p1) gap)
                            q
                          )
                        )
                        (pa-cut-line-segment p1 cut1)
                      )
                    )

                    ;; p -> p2 side
                    (if dir2
                      (progn
                        (setq cut2
                          (if cutSide2
                            (polar q (angle q p2) gap)
                            q
                          )
                        )
                        (pa-cut-line-segment cut2 p2)
                      )
                    )
                  )
                )
              )
              )

              ((< (distance p p1) tol)
                (setq dir2 (pa-dir8 p p2))
                (if (and dir2 (member dir2 cutDirs))
                  (progn
                    (entdel e)
                    (setq cut2 (polar p (angle p p2) gap))
                    (pa-cut-line-segment cut2 p2)
                  )
                )
              )

              ((< (distance p p2) tol)
                (setq dir1 (pa-dir8 p p1))
                (if (and dir1 (member dir1 cutDirs))
                  (progn
                    (entdel e)
                    (setq cut1 (polar p (angle p p1) gap))
                    (pa-cut-line-segment p1 cut1)
                  )
                )
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )
)

(defun pa-filter-inserts (ss / result i e)

  (setq result (ssadd))
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))

    (if (= (cdr (assoc 0 (entget e))) "INSERT")
      (ssadd e result)
    )

    (setq i (1+ i))
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-fillet-lines-at-node (ss p rad / i e ed p1 p2 hitLines)

  (setq hitLines '())
  (setq i 0)

  ;; pに接続するLINEを取得
  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq ed (entget e))
    (setq p1 (cdr (assoc 10 ed)))
    (setq p2 (cdr (assoc 11 ed)))

    (if (or (< (distance p p1) 10.0)
            (< (distance p p2) 10.0))
      (setq hitLines (cons e hitLines))
    )

    (setq i (1+ i))
  )

  ;; 2本あればフィレット
  (if (= (length hitLines) 2)
    (progn
      (setvar "FILLETRAD" rad)
      (command "_.FILLET" (car hitLines) (cadr hitLines))
    )
  )
)

(defun pa-insert-block (p blkName angleDeg dia / angleRad scale ent dirs cutDirs gap)

  (setq angleRad (* pi (/ angleDeg 180.0)))
  (if (or (null dia) (<= dia 0.0))
    (setq dia 100.0)
  )
  (setq scale (* (pa-get-block-scale) (/ dia 100.0)))
  (setq dirs pa-current-insert-dirs)
  (setq cutDirs (pa-get-cut-dirs blkName dirs))
  (setq gap
    (cond
      ((= blkName "z01051009B") (* 1.32 scale))
      ((= blkName "z01051011B") (* 1.6 scale))
      (T (* 1.2 scale))
    )
  )

  (if (tblsearch "BLOCK" blkName)
    (progn
      (setq ent
        (entmakex
          (list
            '(0 . "INSERT")
            (cons 2 blkName)
            (cons 10 p)
            (cons 41 scale)
            (cons 42 scale)
            (cons 43 scale)
            (cons 50 angleRad)
          )
        )
      )

      (if cutDirs
        (if pa-defer-line-cuts
          (pa-queue-line-cut p gap cutDirs)
          (pa-cut-lines-around-point p gap cutDirs nil)
        )
      )

      ent
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

(defun c:INSERTPIPEPARTS (/ *error* olderr ss lineSs textSs partSs selectedPartSs autoPartSs nodes item p dirs info blk ang count flowPt systemType waterType drainLineMode)

  (setq olderr *error*)
  (setq pa-debug-step "command start")

  (defun *error* (msg)
    (princ
      (strcat
        "\n[DBG INSERTPIPEPARTS] ERROR step: "
        (if pa-debug-step pa-debug-step "unknown")
      )
    )
    (princ (strcat "\n[DBG INSERTPIPEPARTS] ERROR message: " msg))
    (setq pa-defer-line-cuts nil)
    (setq *error* olderr)
    (princ)
  )

  (princ "\n[DBG INSERTPIPEPARTS] using debug command definition")

  (initget "D W")
  (setq systemType (getkword "\n処理種別を選択 [排水(D)/給水(W)] <D>: "))

  (if (null systemType)
    (setq systemType "D")
    (setq systemType (strcase systemType))
  )

  ;; 給水の場合だけ、さらに種別選択
  (if (= systemType "W")
    (progn
      (initget "H P")
      (setq waterType (getkword "\n給水種別を選択 [配管(H)/ポリエチレン管(P)] <H>: "))

      (if (null waterType)
        (setq waterType "H")
        (setq waterType (strcase waterType))
      )
    )
  )

  (if (= systemType "D")
    (progn
      (initget "S D")
      (setq drainLineMode (getkword "\nDrain mode [Single S/Double D] <S>: "))

      (if (null drainLineMode)
        (setq drainLineMode "S")
        (setq drainLineMode (strcase drainLineMode))
      )
    )
  )

  (princ
    (strcat
      "\n[DBG INSERTPIPEPARTS] system type: "
      systemType
      " water type: "
      (if waterType waterType "-")
      " drain mode: "
      (if drainLineMode drainLineMode "-")
    )
  )

  (if (and (= systemType "D") (= drainLineMode "D"))
    (princ "\n[DBG INSERTPIPEPARTS] double-line drain mode selected")
  )

  (setq pa-debug-step "ssget targets")
  (if (and (= systemType "D") (= drainLineMode "D"))
    (progn
      (princ "\n[DBG INSERTPIPEPARTS] select LINE, pipe-size TEXT, and blocks for double-line conversion")
      (setq ss
        (ssget
          '(
            (-4 . "<OR")
              (0 . "LINE")
              (0 . "TEXT,MTEXT")
              (0 . "INSERT")
            (-4 . "OR>")
          )
        )
      )
    )
    (setq ss
      (ssget
        '(
          (-4 . "<OR")
            (0 . "LINE")
            (-4 . "<AND")
              (0 . "INSERT")
              (2 . "z01051001B,z02021001B")
            (-4 . "AND>")
          (-4 . "OR>")
        )
      )
    )
  )

  (if ss
    (progn
      (princ (strcat "\n[DBG INSERTPIPEPARTS] selected total: " (itoa (sslength ss))))

      (setq pa-debug-step "filter selected LINE")
      (setq lineSs (pa-filter-lines ss))
      (if (and (= systemType "D") (= drainLineMode "D") lineSs)
        (setq lineSs (pa-filter-double-drain-layer lineSs))
      )
      (if lineSs
        (princ (strcat "\n[DBG INSERTPIPEPARTS] selected lines: " (itoa (sslength lineSs))))
        (princ "\n[DBG INSERTPIPEPARTS] selected lines: 0")
      )

      (setq textSs (pa-filter-texts ss))
      (if (and (= systemType "D") (= drainLineMode "D"))
        (if textSs
          (princ (strcat "\n[DBG INSERTPIPEPARTS] selected pipe-size texts: " (itoa (sslength textSs))))
          (princ "\n[DBG INSERTPIPEPARTS] selected pipe-size texts: 0")
        )
      )

      (setq pa-debug-step "filter selected target blocks")
      (if (and (= systemType "D") (= drainLineMode "D"))
        (progn
          (setq selectedPartSs (pa-filter-inserts ss))
          (pa-debug-double-insert-selection "selected" selectedPartSs)

          (if selectedPartSs
            (setq selectedPartSs (pa-filter-double-convertible-blocks selectedPartSs))
          )
          (pa-debug-double-insert-selection "selected convertible" selectedPartSs)

          (if lineSs
            (setq autoPartSs (pa-get-double-blocks-on-lines lineSs))
            (setq autoPartSs nil)
          )
          (pa-debug-double-insert-selection "auto on drain lines" autoPartSs)

          (setq partSs (pa-merge-selection-sets selectedPartSs autoPartSs))
        )
        (setq partSs (pa-filter-blocks-by-name ss "z01051001B,z02021001B"))
      )
      (if partSs
        (princ (strcat "\n[DBG INSERTPIPEPARTS] selected target blocks: " (itoa (sslength partSs))))
        (princ "\n[DBG INSERTPIPEPARTS] selected target blocks: 0")
      )

      (setq count 0)

      (if lineSs
        (progn
          (if (and (= systemType "D") (not (= drainLineMode "D")))
            (progn
              (setq pa-debug-step "get flow point")
              (princ "\n[DBG INSERTPIPEPARTS] asking flow point")
              (setq flowPt (getpoint "\n流れ先の点を指定してください: "))
            )
          )

          (cond
            ((and (= systemType "D") (= drainLineMode "D"))
              (cond
                (textSs
                  (setq pa-debug-step "convert double pipe")
                  (pa-convert-double-pipe-run lineSs textSs partSs)
                )

                (partSs
                  (setq pa-debug-step "convert double blocks without pipe-size text")
                  (princ "\n[DBG INSERTPIPEPARTS] double-line drain mode has no pipe-size TEXT; convert selected blocks only")
                  (pa-convert-double-blocks partSs nil)
                )

                (T
                  (princ "\n[DBG INSERTPIPEPARTS] double-line drain mode requires pipe-size TEXT")
                )
              )
            )

            ((or (= systemType "W") flowPt)
              (progn
              (if flowPt
                (princ
                  (strcat
                    "\n[DBG INSERTPIPEPARTS] flow point=("
                    (rtos (car flowPt) 2 2)
                    ","
                    (rtos (cadr flowPt) 2 2)
                    ")"
                  )
                )
              )

              (setq pa-debug-step "get node types")
              (setq nodes (pa-get-node-types lineSs))
              (princ (strcat "\n[DBG INSERTPIPEPARTS] nodes: " (itoa (length nodes))))
              (setq pa-defer-line-cuts T)
              (setq pa-line-cut-queue '())
              (setq pa-active-line-ss lineSs)

              (foreach item nodes
                (setq pa-debug-step "process node")
                (setq p (car item))

                (princ
                  (strcat
                    "\n[DBG INSERTPIPEPARTS] node point=("
                    (rtos (car p) 2 2)
                    ","
                    (rtos (cadr p) 2 2)
                    ")"
                  )
                )

                (setq pa-debug-step "get node dirs")
                (setq dirs (pa-get-node-dirs lineSs p 10.0))
                (princ
                  (strcat
                    "\n[DBG INSERTPIPEPARTS] node dirs: "
                    (if dirs (pa-dirs-to-string dirs) "nil")
                  )
                )

                (setq pa-debug-step "dirs to insert info")
                (princ
                  (strcat
                    "\n[DBG INSERTPIPEPARTS] dispatch systemType="
                    systemType
                    " waterType="
                    (if waterType waterType "-")
                  )
                )
                (setq info
                  (cond
                    ((= systemType "W")
                      (pa-dirs-to-insert-info-water dirs lineSs p flowPt waterType)
                    )
                    (T
                      (pa-dirs-to-insert-info dirs lineSs p flowPt)
                    )
                  )
                )
                (if info
                  (progn
                    (setq blk (car info))
                    (setq ang (cadr info))
                    (princ
                      (strcat
                        "\n[DBG INSERTPIPEPARTS] insert "
                        blk
                        " angle="
                        (rtos ang 2 0)
                      )
                    )

                    (setq pa-debug-step "insert block")
                    (setq pa-current-insert-dirs dirs)
                    (if (= blk "__FILLET__")
                      (pa-fillet-lines-at-node lineSs p (getvar "LTSCALE"))
                      (pa-insert-block p blk ang 100.0)
                    )
                    (setq pa-current-insert-dirs nil)
                    (setq count (1+ count))
                  )
                  (princ "\n[DBG INSERTPIPEPARTS] insert info: nil")
                )
              )

              (if (not (and (= systemType "D") (= drainLineMode "D")))
                (progn
                  (setq pa-debug-step "replace selected blocks before line cuts")
                  (pa-replace-01-to-16 partSs lineSs)
                )
              )

              (setq pa-debug-step "apply deferred line cuts")
              (setq pa-defer-line-cuts nil)
              (pa-apply-deferred-line-cuts)
              (setq pa-active-line-ss nil)
              )
            )

            (T
              (princ "\n[DBG INSERTPIPEPARTS] flow point is nil; skip LINE insert")
            )
          )
        )
        (princ "\n[DBG INSERTPIPEPARTS] no selected LINE; skip flow point")
      )

      (princ (strcat "\n挿入数: " (itoa count)))
    )
    (princ "\nLINEまたは対象ブロックを選択してください。")
  )

  (setq pa-debug-step "command end")
  (setq *error* olderr)
  (princ)
)
