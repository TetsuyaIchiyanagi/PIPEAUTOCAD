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

(defun pa-replace-01-to-16 (targetSs / ss lineSs i e ed p dirs ang)

  (princ "\n[DBG 01to16] start")

  ;; Use only the blocks selected by the user.
  (setq ss targetSs)

  (if ss
    (princ (strcat "\n[DBG 01to16] selected target blocks: " (itoa (sslength ss))))
    (princ "\n[DBG 01to16] selected target blocks: 0")
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

(defun pa-get-block-scale (/ ltScale)

  (setq ltScale (getvar "LTSCALE"))

  ;; Linear interpolation:
  ;; LTSCALE 50.0  -> insert scale 75.0
  ;; LTSCALE 100.0 -> insert scale 100.0
  ;; LTSCALE 200.0 -> insert scale 150.0
  (+ 50.0 (* 0.5 ltScale))
)

(defun pa-queue-line-cut (p gap cutDirs)
  (setq pa-line-cut-queue (cons (list p gap cutDirs) pa-line-cut-queue))
)

(defun pa-apply-deferred-line-cuts (/ item)
  (princ
    (strcat
      "\n[DBG INSERTPIPEPARTS] deferred line cuts: "
      (itoa (length pa-line-cut-queue))
    )
  )

  (foreach item (reverse pa-line-cut-queue)
    (pa-cut-lines-around-point (car item) (cadr item) (caddr item))
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

(defun pa-cut-lines-around-point (p gap cutDirs / ss i e ed p1 p2 q dir1 dir2 cutSide1 cutSide2 cut1 cut2 tol)

  (setq tol 25.0)

  (if cutDirs
    (progn
      (setq ss (ssget "_X" '((0 . "LINE"))))

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

(defun pa-insert-block (p blkName angleDeg / angleRad scale ent dirs cutDirs gap)

  (setq angleRad (* pi (/ angleDeg 180.0)))
  (setq scale (pa-get-block-scale))
  (setq dirs pa-current-insert-dirs)
  (setq cutDirs (pa-get-cut-dirs blkName dirs))
  (setq gap
    (if (= blkName "z01051009B")
      (* 1.32 scale)
      (* 1.2 scale)
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
          (pa-cut-lines-around-point p gap cutDirs)
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

(defun c:INSERTPIPEPARTS (/ *error* olderr ss lineSs partSs nodes item p dirs info blk ang count flowPt)

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

  (setq pa-debug-step "ssget LINE or target block")
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

  (if ss
    (progn
      (princ (strcat "\n[DBG INSERTPIPEPARTS] selected total: " (itoa (sslength ss))))

      (setq pa-debug-step "filter selected LINE")
      (setq lineSs (pa-filter-lines ss))
      (if lineSs
        (princ (strcat "\n[DBG INSERTPIPEPARTS] selected lines: " (itoa (sslength lineSs))))
        (princ "\n[DBG INSERTPIPEPARTS] selected lines: 0")
      )

      (setq pa-debug-step "filter selected target blocks")
      (setq partSs (pa-filter-blocks-by-name ss "z01051001B,z02021001B"))
      (if partSs
        (princ (strcat "\n[DBG INSERTPIPEPARTS] selected target blocks: " (itoa (sslength partSs))))
        (princ "\n[DBG INSERTPIPEPARTS] selected target blocks: 0")
      )

      (setq count 0)

      (if lineSs
        (progn
          (setq pa-debug-step "get flow point")
          (princ "\n[DBG INSERTPIPEPARTS] asking flow point")
          (setq flowPt (getpoint "\n流れ先の点を指定してください: "))

          (if flowPt
            (progn
              (princ
                (strcat
                  "\n[DBG INSERTPIPEPARTS] flow point=("
                  (rtos (car flowPt) 2 2)
                  ","
                  (rtos (cadr flowPt) 2 2)
                  ")"
                )
              )

              (setq pa-debug-step "get node types")
              (setq nodes (pa-get-node-types lineSs))
              (princ (strcat "\n[DBG INSERTPIPEPARTS] nodes: " (itoa (length nodes))))
              (setq pa-defer-line-cuts T)
              (setq pa-line-cut-queue '())

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
                (setq info (pa-dirs-to-insert-info dirs lineSs p flowPt))
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
                    (pa-insert-block p blk ang)
                    (setq pa-current-insert-dirs nil)
                    (setq count (1+ count))
                  )
                  (princ "\n[DBG INSERTPIPEPARTS] insert info: nil")
                )
              )

              (setq pa-debug-step "apply deferred line cuts")
              (setq pa-defer-line-cuts nil)
              (pa-apply-deferred-line-cuts)
            )
            (princ "\n[DBG INSERTPIPEPARTS] flow point is nil; skip LINE insert")
          )
        )
        (princ "\n[DBG INSERTPIPEPARTS] no selected LINE; skip flow point")
      )

      (setq pa-debug-step "replace selected blocks")
      (pa-replace-01-to-16 partSs)

      (princ (strcat "\n挿入数: " (itoa count)))
    )
    (princ "\nLINEまたは対象ブロックを選択してください。")
  )

  (setq pa-debug-step "command end")
  (setq *error* olderr)
  (princ)
)
