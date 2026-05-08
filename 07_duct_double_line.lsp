;; PipeAutoCAD 07: round duct double-line workflow
;;
;; Current scope:
;; DUCTDOUBLE -> read duct diameter -> select LINE -> insert duct blocks,
;; draw double lines, and trim around inserted blocks.

(defun pa-duct-node-tol ()
  10.0
)

(defun pa-duct-valid-dia-p (dia)
  (member dia '(100 150 200 300 400))
)

(defun pa-duct-read-dia (/ dia done)
  (setq done nil)

  (while (not done)
    (setq dia (getint "\nDuct diameter [100/150/200/300/400] <200>: "))

    (cond
      ((null dia)
        (setq dia 200)
        (setq done T)
      )
      ((pa-duct-valid-dia-p dia)
        (setq done T)
      )
      (T
        (princ "\nEnter 100, 150, 200, 300, or 400.")
      )
    )
  )

  dia
)

(defun pa-duct-scale-from-dia (dia)
  (float dia)
)

(defun pa-duct-offset-from-dia (dia)
  (/ (float dia) 2.0)
)

(defun pa-duct-layer-locked-p (layerName / layerInfo flags)
  (if layerName
    (progn
      (setq layerInfo (tblsearch "LAYER" layerName))
      (if layerInfo
        (progn
          (setq flags (cdr (assoc 70 layerInfo)))
          (= (logand flags 4) 4)
        )
        nil
      )
    )
    nil
  )
)

(defun pa-duct-filter-unlocked-lines (ss / result skipped i e ed layer)
  (setq result (ssadd))
  (setq skipped 0)
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq ed (entget e))
    (setq layer (cdr (assoc 8 ed)))

    (if (pa-duct-layer-locked-p layer)
      (setq skipped (1+ skipped))
      (ssadd e result)
    )

    (setq i (1+ i))
  )

  (if (> skipped 0)
    (princ (strcat "\nSkipped locked-layer LINEs: " (itoa skipped)))
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-duct-add-node (p result tol / found item newResult)
  (setq found nil)
  (setq newResult '())

  (foreach item result
    (if (< (distance p item) tol)
      (progn
        (setq found T)
        (setq newResult (cons item newResult))
      )
      (setq newResult (cons item newResult))
    )
  )

  (if found
    newResult
    (cons p result)
  )
)

(defun pa-duct-get-line-intersections (ss / result tol i j e1 e2 o1 o2 ip p)
  (setq result '())
  (setq tol (pa-duct-node-tol))
  (setq i 0)

  (while (< i (sslength ss))
    (setq e1 (ssname ss i))
    (setq o1 (vlax-ename->vla-object e1))
    (setq j (1+ i))

    (while (< j (sslength ss))
      (setq e2 (ssname ss j))
      (setq o2 (vlax-ename->vla-object e2))
      (setq ip (vlax-invoke o1 'IntersectWith o2 acExtendNone))

      (while ip
        (setq p (list (car ip) (cadr ip) (caddr ip)))
        (setq result (pa-duct-add-node p result tol))
        (setq ip (cdddr ip))
      )

      (setq j (1+ j))
    )

    (setq i (1+ i))
  )

  (reverse result)
)

(defun pa-duct-dirs-to-block (dirs / d)
  (setq d (pa-sort-dirs dirs))

  (cond
    ;; 90-degree elbows
    ((pa-same-dirs d '("R" "U")) (list "z03103001B" 0.0))
    ((pa-same-dirs d '("L" "U")) (list "z03103001B" 90.0))
    ((pa-same-dirs d '("L" "D")) (list "z03103001B" 180.0))
    ((pa-same-dirs d '("R" "D")) (list "z03103001B" 270.0))

    ;; 45-degree elbows.
    ((pa-same-dirs d '("R" "RU")) (list "DUCT_EL45" 0.0))
    ((pa-same-dirs d '("U" "RU")) (list "DUCT_EL45" 0.0))

    ((pa-same-dirs d '("U" "LU")) (list "DUCT_EL45" 90.0))

    ((pa-same-dirs d '("L" "LU")) (list "DUCT_EL45" 180.0))
    ((pa-same-dirs d '("L" "LD")) (list "DUCT_EL45" 180.0))

    ((pa-same-dirs d '("D" "LD")) (list "DUCT_EL45" 270.0))
    ((pa-same-dirs d '("D" "RD")) (list "DUCT_EL45" 270.0))
    ((pa-same-dirs d '("R" "RD")) (list "DUCT_EL45" 0.0))

    ;; 135-degree elbows.
    ((pa-same-dirs d '("R" "LU")) (list "z03104001B" 0.0))
    ((pa-same-dirs d '("R" "LD")) (list "z03104001B" 0.0))

    ((pa-same-dirs d '("U" "RD")) (list "z03104001B" 90.0))
    ((pa-same-dirs d '("U" "LD")) (list "z03104001B" 225.0))

    ((pa-same-dirs d '("L" "RU")) (list "z03104001B" 180.0))
    ((pa-same-dirs d '("L" "RD")) (list "z03104001B" 180.0))

    ((pa-same-dirs d '("D" "RU")) (list "z03104001B" 270.0))
    ((pa-same-dirs d '("D" "LU")) (list "z03104001B" 270.0))

    ;; 135-degree crossing points. These are debug classifications for STEP1;
    ;; insertion rules can be split later if a crossing duct block is needed.
    ((pa-same-dirs d '("L" "R" "LU" "RD")) (list "DUCT_CROSS135" 0.0))
    ((pa-same-dirs d '("L" "R" "RU" "LD")) (list "DUCT_CROSS135" 0.0))
    ((pa-same-dirs d '("U" "D" "LU" "RD")) (list "DUCT_CROSS135" 90.0))
    ((pa-same-dirs d '("U" "D" "RU" "LD")) (list "DUCT_CROSS135" 90.0))

    ;; T branches
    ((pa-same-dirs d '("L" "R" "U")) (list "DUCT_TEE" 0.0))
    ((pa-same-dirs d '("U" "D" "R")) (list "DUCT_TEE" 90.0))
    ((pa-same-dirs d '("L" "R" "D")) (list "DUCT_TEE" 180.0))
    ((pa-same-dirs d '("U" "D" "L")) (list "DUCT_TEE" 270.0))

    (T nil)
  )
)

(defun pa-duct-insert-angle (blkName angleDeg)
  (if (= blkName "z03104001B")
    (+ angleDeg 90.0)
    (+ angleDeg 180.0)
  )
)

(defun pa-duct-insert-block (p blkName angleDeg dia / angleRad scale ent)
  (setq angleRad (* pi (/ (pa-duct-insert-angle blkName angleDeg) 180.0)))
  (setq scale (pa-duct-scale-from-dia dia))

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
      ent
    )
    (progn
      (princ (strcat "\nBlock not found: " blkName))
      nil
    )
  )
)

(defun pa-duct-block-extents (ent p / obj minPt maxPt err mn mx)
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq err (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPt 'maxPt)))

      (if (vl-catch-all-error-p err)
        nil
        (progn
          (setq mn (vlax-safearray->list minPt))
          (setq mx (vlax-safearray->list maxPt))
          (list
            (max 0.0 (- (cadr mx) (cadr p))) ; U
            (max 0.0 (- (cadr p) (cadr mn))) ; D
            (max 0.0 (- (car p) (car mn)))   ; L
            (max 0.0 (- (car mx) (car p)))   ; R
          )
        )
      )
    )
    nil
  )
)

(defun pa-duct-insert-node-block (p info dia / blk ang ent extents)
  (if info
    (progn
      (setq blk (car info))
      (setq ang (cadr info))

      (if (member blk '("z03103001B" "z03104001B"))
        (progn
          (setq ent (pa-duct-insert-block p blk ang dia))
          (if ent
            (progn
              (setq extents (pa-duct-block-extents ent p))
              (list p ent blk extents)
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

(defun pa-duct-process-nodes (lineSs dia / nodes p dirs info inserted node nodeCount insertCount skippedCount)
  (setq nodes (pa-duct-get-line-intersections lineSs))
  (setq inserted '())
  (setq nodeCount 0)
  (setq insertCount 0)
  (setq skippedCount 0)

  (foreach p nodes
    (setq dirs (pa-get-node-dirs lineSs p (pa-duct-node-tol)))
    (setq info (pa-duct-dirs-to-block dirs))
    (setq nodeCount (1+ nodeCount))

    (setq node (pa-duct-insert-node-block p info dia))
    (if node
      (progn
        (setq inserted (cons node inserted))
        (setq insertCount (1+ insertCount))
      )
      (setq skippedCount (1+ skippedCount))
    )
  )

  (list nodeCount insertCount skippedCount nodes (reverse inserted))
)

(defun pa-duct-line-point-param (p a b / ab ap ab2)
  (setq ab (mapcar '- b a))
  (setq ap (mapcar '- p a))
  (setq ab2 (apply '+ (mapcar '* ab ab)))

  (if (= ab2 0.0)
    0.0
    (/ (apply '+ (mapcar '* ap ab)) ab2)
  )
)

(defun pa-duct-sort-points-on-line (pts p1 p2)
  (vl-sort
    pts
    '(lambda (a b)
      (< (pa-duct-line-point-param a p1 p2)
         (pa-duct-line-point-param b p1 p2))
    )
  )
)

(defun pa-duct-line-split-points (p1 p2 nodes / pts tol p)
  (setq pts '())
  (setq tol (pa-duct-node-tol))
  (setq pts (pa-duct-add-node p1 pts tol))
  (setq pts (pa-duct-add-node p2 pts tol))

  (foreach p nodes
    (if (pa-point-on-seg2 p p1 p2 tol)
      (setq pts (pa-duct-add-node p pts tol))
    )
  )

  (pa-duct-sort-points-on-line pts p1 p2)
)

(defun pa-duct-draw-double-line (p1 p2 offset lineLayer / ang p1a p2a p1b p2b)
  (setq ang (angle p1 p2))

  (setq p1a (polar p1 (+ ang (/ pi 2.0)) offset))
  (setq p2a (polar p2 (+ ang (/ pi 2.0)) offset))
  (setq p1b (polar p1 (- ang (/ pi 2.0)) offset))
  (setq p2b (polar p2 (- ang (/ pi 2.0)) offset))

  (entmakex
    (list
      '(0 . "LINE")
      (cons 8 (if lineLayer lineLayer "0"))
      (cons 10 p1a)
      (cons 11 p2a)
    )
  )

  (entmakex
    (list
      '(0 . "LINE")
      (cons 8 (if lineLayer lineLayer "0"))
      (cons 10 p1b)
      (cons 11 p2b)
    )
  )
)

(defun pa-duct-block-gap-factor (nodeItem dir / blk)
  (setq blk (nth 2 nodeItem))

  (cond
    ((and (= blk "z03104001B") (= dir "LD")) 0.55)
    (T 1.0)
  )
)

(defun pa-duct-block-gap-by-dir (nodeItem dir dia / extents gap)
  (setq extents (cadddr nodeItem))

  (if extents
    (setq gap
      (cond
        ((= dir "U") (nth 0 extents))
        ((= dir "D") (nth 1 extents))
        ((= dir "L") (nth 2 extents))
        ((= dir "R") (nth 3 extents))
        ((= dir "RU") (max (nth 0 extents) (nth 3 extents)))
        ((= dir "LU") (max (nth 0 extents) (nth 2 extents)))
        ((= dir "RD") (max (nth 1 extents) (nth 3 extents)))
        ((= dir "LD") (max (nth 1 extents) (nth 2 extents)))
        (T dia)
      )
    )
    (setq gap dia)
  )

  (* gap (pa-duct-block-gap-factor nodeItem dir))
)

(defun pa-duct-end-gap (p otherPt blockNodes dia / tol nodeItem item nodePt dir gap)
  (setq tol (pa-duct-node-tol))
  (setq nodeItem nil)

  (foreach item blockNodes
    (setq nodePt (car item))
    (if (< (distance p nodePt) tol)
      (setq nodeItem item)
    )
  )

  (if nodeItem
    (progn
      (setq dir (pa-dir8 p otherPt))
      (setq gap (pa-duct-block-gap-by-dir nodeItem dir dia))
    )
    (setq gap 0.0)
  )

  gap
)

(defun pa-duct-draw-double-line-gap (p1 p2 offset lineLayer gap1 gap2 / len maxGap q1 q2)
  (setq len (distance p1 p2))
  (setq maxGap (/ len 2.5))

  (if (> gap1 maxGap) (setq gap1 maxGap))
  (if (> gap2 maxGap) (setq gap2 maxGap))

  (setq q1 (polar p1 (angle p1 p2) gap1))
  (setq q2 (polar p2 (angle p2 p1) gap2))

  (if (> (distance q1 q2) 1.0)
    (pa-duct-draw-double-line q1 q2 offset lineLayer)
  )
)

(defun pa-duct-draw-double-lines-for-line (e nodes blockNodes dia / ed p1 p2 layer pts pA pB gapA gapB count)
  (setq ed (entget e))
  (setq p1 (cdr (assoc 10 ed)))
  (setq p2 (cdr (assoc 11 ed)))
  (setq layer (cdr (assoc 8 ed)))
  (setq pts (pa-duct-line-split-points p1 p2 nodes))
  (setq count 0)

  (while (> (length pts) 1)
    (setq pA (car pts))
    (setq pB (cadr pts))

    (if (> (distance pA pB) 1.0)
      (progn
        (setq gapA (pa-duct-end-gap pA pB blockNodes dia))
        (setq gapB (pa-duct-end-gap pB pA blockNodes dia))
        (pa-duct-draw-double-line-gap pA pB (pa-duct-offset-from-dia dia) layer gapA gapB)
        (setq count (1+ count))
      )
    )

    (setq pts (cdr pts))
  )

  count
)

(defun pa-duct-delete-line-ss (lineSs / i e)
  (setq i 0)

  (while (< i (sslength lineSs))
    (setq e (ssname lineSs i))
    (entdel e)
    (setq i (1+ i))
  )
)

(defun pa-duct-convert-center-lines (lineSs dia nodes blockNodes / i e count)
  (setq i 0)
  (setq count 0)

  (while (< i (sslength lineSs))
    (setq e (ssname lineSs i))
    (setq count
      (+ count (pa-duct-draw-double-lines-for-line e nodes blockNodes dia))
    )
    (setq i (1+ i))
  )

  (pa-duct-delete-line-ss lineSs)
  count
)

(defun c:DUCTDOUBLE (/ dia selectedSs lineSs result nodeCount insertCount skippedCount nodes blockNodes doubleCount)
  (vl-load-com)

  (setq dia (pa-duct-read-dia))
  (princ
    (strcat
      "\nDuct diameter="
      (itoa dia)
      " offset="
      (rtos (pa-duct-offset-from-dia dia) 2 1)
      " block scale="
      (rtos (pa-duct-scale-from-dia dia) 2 1)
    )
  )

  (princ "\nSelect duct center LINEs: ")
  (setq selectedSs (ssget '((0 . "LINE"))))

  (if selectedSs
    (setq lineSs (pa-duct-filter-unlocked-lines selectedSs))
    (setq lineSs nil)
  )

  (if lineSs
    (progn
      (setq result (pa-duct-process-nodes lineSs dia))
      (setq nodeCount (car result))
      (setq insertCount (cadr result))
      (setq skippedCount (caddr result))
      (setq nodes (nth 3 result))
      (setq blockNodes (nth 4 result))
      (setq doubleCount (pa-duct-convert-center-lines lineSs dia nodes blockNodes))
      (princ
        (strcat
          "\nDUCTDOUBLE nodes: "
          (itoa nodeCount)
          " inserted blocks: "
          (itoa insertCount)
          " skipped: "
          (itoa skippedCount)
          " double-line segments: "
          (itoa doubleCount)
        )
      )
    )
    (princ "\nNo unlocked LINE selected.")
  )

  (princ)
)
(princ)
