;; PipeAutoCAD 06: double-line conversion
;;
;; Planned command: DOUBLEPIPE
;;
;; Flow:
;; 1. PIPESIZEAUTO places pipe-size TEXT.
;; 2. User edits the pipe-size TEXT.
;; 3. DOUBLEPIPE reads LINE and pipe-size TEXT.
;; 4. Determine the pipe size for each segment.
;; 5. Convert single LINEs to double lines based on pipe diameter.
;; 6. Replace existing part blocks with size-scaled blocks.
;; 7. Insert increasers at pipe-size change points.

(defun pa-filter-texts (ss / result i e)
  (setq result (ssadd))
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))

    (if (wcmatch (cdr (assoc 0 (entget e))) "TEXT,MTEXT")
      (ssadd e result)
    )

    (setq i (1+ i))
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-pipe-seg-text-search-radius (/ txtH)
  (setq txtH (pa-pipe-size-text-height))
  (max 500.0 (* txtH 5.0))
)

(defun pa-text-point (ed / p)
  (setq p (cdr (assoc 11 ed)))

  (if p
    p
    (cdr (assoc 10 ed))
  )
)

(defun pa-double-pipe-break-block-names ()
  "z00010002B,z01051011B,z01051009B,z01051018B,z02021009B,z02021011B,z02021015B"
)

(defun pa-double-pipe-block-node-tol (/ scale)
  (setq scale (pa-get-block-scale))
  (max 100.0 (* scale 1.5))
)

(defun pa-double-pipe-add-block-node (p handle result /)
  (if p
    (cons (list p "BLOCK" handle) result)
    result
  )
)

(defun pa-double-pipe-block-bbox-center (e / obj minPt maxPt err mn mx)
  (setq obj (vlax-ename->vla-object e))
  (setq err (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPt 'maxPt)))

  (if (vl-catch-all-error-p err)
    nil
    (progn
      (setq mn (vlax-safearray->list minPt))
      (setq mx (vlax-safearray->list maxPt))
      (list
        (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0)
        (/ (+ (caddr mn) (caddr mx)) 2.0)
      )
    )
  )
)

(defun pa-get-double-pipe-block-nodes (/ ss i e ed p center handle result)
  (setq result '())
  (setq ss
    (ssget
      "_X"
      (list
        '(0 . "INSERT")
        (cons 2 (pa-double-pipe-break-block-names))
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
        (setq handle (cdr (assoc 5 ed)))
        (setq center (pa-double-pipe-block-bbox-center e))

        (setq result (pa-double-pipe-add-block-node p handle result))
        (if (and center (or (null p) (not (pa-same-point-p p center 1.0))))
          (setq result (pa-double-pipe-add-block-node center handle result))
        )

        (setq i (1+ i))
      )
    )
  )

  result
)

(defun pa-double-pipe-block-node-on-line (nodePt p1 p2 / q ratio tol d)
  (setq tol (pa-double-pipe-block-node-tol))
  (setq q (pa-closest-point-on-line nodePt p1 p2))
  (setq ratio (pa-line-point-param q p1 p2))
  (setq d (distance nodePt q))

  (if (and (<= d tol)
           (>= ratio 0.0)
           (<= ratio 1.0)
      )
    (list q d)
    nil
  )
)

(defun pa-put-block-hit (handle q d hits / found result item oldD)
  (setq found nil)
  (setq result '())

  (foreach item hits
    (if (= (car item) handle)
      (progn
        (setq found T)
        (setq oldD (caddr item))

        (if (< d oldD)
          (setq result (cons (list handle q d) result))
          (setq result (cons item result))
        )
      )
      (setq result (cons item result))
    )
  )

  (if found
    result
    (cons (list handle q d) hits)
  )
)

(defun pa-get-double-pipe-nodes-on-line (nodes p1 p2 tol / pts blockHits item nodePt nodeType handle hit q)
  (setq pts '())
  (setq blockHits '())
  (setq pts (pa-add-unique-point p1 pts tol))
  (setq pts (pa-add-unique-point p2 pts tol))

  (foreach item nodes
    (setq nodePt (car item))
    (setq nodeType (cadr item))
    (setq handle (caddr item))

    (cond
      ((= nodeType "BLOCK")
        (setq hit (pa-double-pipe-block-node-on-line nodePt p1 p2))
        (if hit
          (setq blockHits
            (pa-put-block-hit
              handle
              (car hit)
              (cadr hit)
              blockHits
            )
          )
        )
      )

      ((pa-point-on-seg2 nodePt p1 p2 tol)
        (setq pts (pa-add-unique-point nodePt pts tol))
      )
    )
  )

  (foreach hit blockHits
    (setq q (cadr hit))
    (setq pts (pa-add-unique-point q pts tol))
  )

  (pa-sort-nodes-on-line pts p1 p2)
)

(defun pa-pipe-size-text-hit-score (txtPt p1 p2 mid radius / q ratio lineD midD)
  (setq q (pa-closest-point-on-line txtPt p1 p2))
  (setq ratio (pa-line-point-param q p1 p2))
  (setq lineD (distance txtPt q))

  (if (and (<= lineD radius)
           (>= ratio 0.0)
           (<= ratio 1.0)
      )
    (progn
      (setq midD (distance q mid))
      (+ lineD (* midD 0.2))
    )
    nil
  )
)

(defun pa-find-nearest-pipe-size-text (p1 p2 textSs / mid radius i e ed txtPt txt score bestScore bestSize)
  (setq mid (pa-pipe-size-midpoint p1 p2))
  (setq radius (pa-pipe-seg-text-search-radius))
  (setq bestScore nil)
  (setq bestSize nil)

  (if textSs
    (progn
      (setq i 0)

      (while (< i (sslength textSs))
        (setq e (ssname textSs i))
        (setq ed (entget e))
        (setq txtPt (pa-text-point ed))
        (setq txt (cdr (assoc 1 ed)))

        (if (and txtPt
                 txt
                 (not (wcmatch (strcase txt) "SEG=*"))
                 (not (wcmatch (strcase txt) "NODE=*"))
            )
          (progn
            (setq score (pa-pipe-size-text-hit-score txtPt p1 p2 mid radius))

            (if (and score
                     (or (null bestScore) (< score bestScore))
                )
              (progn
                (setq bestScore score)
                (setq bestSize txt)
              )
            )
          )
        )

        (setq i (1+ i))
      )
    )
  )

  bestSize
)

(defun pa-add-pipe-segments-on-line (lineNodes textSs result lineLayer / prevPt nextPts nextPt size)
  (if (> (length lineNodes) 1)
    (progn
      (setq prevPt (car lineNodes))
      (setq nextPts (cdr lineNodes))

      (while nextPts
        (setq nextPt (car nextPts))
        (setq size (pa-find-nearest-pipe-size-text prevPt nextPt textSs))

        (setq result
          (cons
            (list
              prevPt
              nextPt
              (if size size "")
              lineLayer
            )
            result
          )
        )

        (setq prevPt nextPt)
        (setq nextPts (cdr nextPts))
      )
    )
  )

  result
)

(defun pa-build-pipe-segments (lineSs textSs / nodes blockNodes allNodes result i e ed p1 p2 lineLayer lineNodes)
  (setq result '())

  (if lineSs
    (progn
      (setq nodes (pa-get-node-types lineSs))
      (setq blockNodes (pa-get-double-pipe-block-nodes))
      (setq allNodes (append nodes blockNodes))
      (setq i 0)

      (while (< i (sslength lineSs))
        (setq e (ssname lineSs i))
        (setq ed (entget e))
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))
        (setq lineLayer (cdr (assoc 8 ed)))
        (setq lineNodes (pa-get-double-pipe-nodes-on-line allNodes p1 p2 10.0))
        (setq result (pa-add-pipe-segments-on-line lineNodes textSs result lineLayer))
        (setq i (1+ i))
      )
    )
  )

  (reverse result)
)

(defun pa-place-check-pipe-seg-text (seg / p1 p2 size mid txtH info)
  (setq p1 (car seg))
  (setq p2 (cadr seg))
  (setq size (caddr seg))
  (setq mid (pa-pipe-size-midpoint p1 p2))
  (setq txtH (pa-pipe-size-text-height))
  (setq info (pa-pipe-size-text-info p1 p2 mid 0.0))

  (pa-make-pipe-size-text
    (car info)
    txtH
    (strcat "SEG=" (if (= size "") "?" size))
    (cadr info)
  )
)

(defun pa-place-check-pipe-segments (segments / seg count)
  (setq count 0)

  (foreach seg segments
    (pa-place-check-pipe-seg-text seg)
    (setq count (1+ count))
  )

  count
)

(defun pa-put-node-size (nodePt dia nodeSizes tol / found result item oldPt oldDia)
  (setq found nil)
  (setq result '())

  (foreach item nodeSizes
    (setq oldPt (car item))
    (setq oldDia (cadr item))

    (if (pa-same-point-p nodePt oldPt tol)
      (progn
        (setq found T)

        (if (> dia oldDia)
          (setq result (cons (list oldPt dia) result))
          (setq result (cons item result))
        )
      )
      (setq result (cons item result))
    )
  )

  (if found
    result
    (cons (list nodePt dia) nodeSizes)
  )
)

(defun pa-build-node-sizes (segments / nodeSizes seg p1 p2 size dia tol)
  (setq nodeSizes '())
  (setq tol 10.0)

  (foreach seg segments
    (setq p1 (nth 0 seg))
    (setq p2 (nth 1 seg))
    (setq size (nth 2 seg))
    (setq dia (atof size))

    (if (> dia 0.0)
      (progn
        (setq nodeSizes (pa-put-node-size p1 dia nodeSizes tol))
        (setq nodeSizes (pa-put-node-size p2 dia nodeSizes tol))
      )
    )
  )

  (reverse nodeSizes)
)

(defun pa-format-dia (dia)
  (if (= dia (fix dia))
    (itoa (fix dia))
    (rtos dia 2 2)
  )
)

(defun pa-place-node-size-text (nodeInfo / p dia txtH offset txtPt)
  (setq p (car nodeInfo))
  (setq dia (cadr nodeInfo))
  (setq txtH (pa-pipe-size-text-height))
  (setq offset (pa-pipe-size-text-offset txtH))
  (setq txtPt (list (+ (car p) offset) (+ (cadr p) offset) 0.0))

  (pa-make-pipe-size-text
    txtPt
    txtH
    (strcat "NODE=" (pa-format-dia dia))
    0.0
  )
)

(defun pa-place-node-size-texts (nodeSizes / item count)
  (setq count 0)

  (foreach item nodeSizes
    (pa-place-node-size-text item)
    (setq count (1+ count))
  )

  count
)

(defun pa-get-node-max-dia (p nodeDiaList / result item nodePt dia)
  (setq result 100.0)

  (foreach item nodeDiaList
    (setq nodePt (car item))
    (setq dia (cadr item))

    (if (pa-same-point-p p nodePt 10.0)
      (setq result dia)
    )
  )

  result
)

(defun c:INSERTPIPEPARTS_DIA_TEST (/ ss lineSs textSs flowPt segments nodeDiaList item p dirs info blk ang dia count)
  (princ "\n[DBG INSERTPIPEPARTS_DIA_TEST] start")
  (princ "\nSelect LINE and pipe-size TEXT: ")

  (setq ss
    (ssget
      '(
        (-4 . "<OR")
          (0 . "LINE")
          (0 . "TEXT,MTEXT")
        (-4 . "OR>")
      )
    )
  )

  (if ss
    (progn
      (setq lineSs (pa-filter-lines ss))
      (setq textSs (pa-filter-texts ss))

      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS_DIA_TEST] selected LINE count: "
          (if lineSs (itoa (sslength lineSs)) "0")
        )
      )

      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS_DIA_TEST] selected TEXT count: "
          (if textSs (itoa (sslength textSs)) "0")
        )
      )

      (if (and lineSs textSs)
        (progn
          (setq segments (pa-build-pipe-segments lineSs textSs))
          (setq nodeDiaList (pa-build-node-sizes segments))
          (setq count 0)

          (princ
            (strcat
              "\n[DBG INSERTPIPEPARTS_DIA_TEST] segment count: "
              (itoa (length segments))
            )
          )

          (princ
            (strcat
              "\n[DBG INSERTPIPEPARTS_DIA_TEST] node dia count: "
              (itoa (length nodeDiaList))
            )
          )

          (foreach item nodeDiaList
            (setq p (car item))
            (setq dia (cadr item))
            (setq dirs (pa-get-node-dirs lineSs p 10.0))

            (princ
              (strcat
                "\n[DBG INSERTPIPEPARTS_DIA_TEST] node dia="
                (pa-format-dia dia)
                " dirs="
                (if dirs (pa-dirs-to-string dirs) "nil")
              )
            )
          )

          (setq flowPt (getpoint "\nFlow point for tee direction (Enter to only check dia): "))
          (if flowPt
            (progn
              (princ
                (strcat
                  "\n[DBG INSERTPIPEPARTS_DIA_TEST] insert node count: "
                  (itoa (length nodeDiaList))
                )
              )

              (setq pa-defer-line-cuts T)
              (setq pa-line-cut-queue '())

              (foreach item nodeDiaList
                (setq p (car item))
                (setq dia (cadr item))
                (setq dirs (pa-get-node-dirs lineSs p 10.0))
                (setq info (pa-dirs-to-insert-info dirs lineSs p flowPt))

                (if info
                  (progn
                    (setq blk (car info))
                    (setq ang (cadr info))

                    (princ
                      (strcat
                        "\n[DBG INSERTPIPEPARTS_DIA_TEST] insert "
                        blk
                        " dia="
                        (pa-format-dia dia)
                        " angle="
                        (rtos ang 2 0)
                      )
                    )

                    (setq pa-current-insert-dirs dirs)
                    (if (= blk "__FILLET__")
                      (pa-fillet-lines-at-node lineSs p (* (getvar "LTSCALE") (/ dia 100.0)))
                      (pa-insert-block p blk ang dia)
                    )
                    (setq pa-current-insert-dirs nil)
                    (setq count (1+ count))
                  )
                  (princ "\n[DBG INSERTPIPEPARTS_DIA_TEST] insert info: nil")
                )
              )

              (setq pa-defer-line-cuts nil)
              (pa-apply-deferred-line-cuts)

              (princ
                (strcat
                  "\n[DBG INSERTPIPEPARTS_DIA_TEST] inserted count: "
                  (itoa count)
                )
              )
            )
            (princ "\n[DBG INSERTPIPEPARTS_DIA_TEST] flow point is nil; dia check only")
          )
        )
        (princ "\n[DBG INSERTPIPEPARTS_DIA_TEST] select LINE and pipe-size TEXT")
      )
    )
    (princ "\n[DBG INSERTPIPEPARTS_DIA_TEST] nothing selected")
  )

  (setq pa-current-insert-dirs nil)
  (setq pa-defer-line-cuts nil)
  (princ "\n[DBG INSERTPIPEPARTS_DIA_TEST] end")
  (princ)
)

(defun c:CHECKNODESIZE (/ ss lineSs textSs segments nodeSizes count)
  (princ "\n[DBG CHECKNODESIZE] start")

  (setq ss
    (ssget
      '(
        (-4 . "<OR")
          (0 . "LINE")
          (0 . "TEXT,MTEXT")
        (-4 . "OR>")
      )
    )
  )

  (if ss
    (progn
      (setq lineSs (pa-filter-lines ss))
      (setq textSs (pa-filter-texts ss))

      (princ
        (strcat
          "\n[DBG CHECKNODESIZE] selected LINE count: "
          (if lineSs (itoa (sslength lineSs)) "0")
        )
      )

      (princ
        (strcat
          "\n[DBG CHECKNODESIZE] selected TEXT count: "
          (if textSs (itoa (sslength textSs)) "0")
        )
      )

      (if (and lineSs textSs)
        (progn
          (setq segments (pa-build-pipe-segments lineSs textSs))
          (setq nodeSizes (pa-build-node-sizes segments))
          (setq count (pa-place-node-size-texts nodeSizes))

          (princ
            (strcat
              "\n[DBG CHECKNODESIZE] node count: "
              (itoa count)
            )
          )
        )
        (princ "\n[DBG CHECKNODESIZE] select LINE and pipe-size TEXT")
      )
    )
    (princ "\n[DBG CHECKNODESIZE] nothing selected")
  )

  (princ "\n[DBG CHECKNODESIZE] end")
  (princ)
)

(defun c:CHECKPIPESEG (/ ss lineSs textSs segments count)
  (princ "\n[DBG CHECKPIPESEG] start")

  (setq ss
    (ssget
      '(
        (-4 . "<OR")
          (0 . "LINE")
          (0 . "TEXT,MTEXT")
        (-4 . "OR>")
      )
    )
  )

  (if ss
    (progn
      (setq lineSs (pa-filter-lines ss))
      (setq textSs (pa-filter-texts ss))

      (princ
        (strcat
          "\n[DBG CHECKPIPESEG] selected LINE count: "
          (if lineSs (itoa (sslength lineSs)) "0")
        )
      )

      (princ
        (strcat
          "\n[DBG CHECKPIPESEG] selected TEXT count: "
          (if textSs (itoa (sslength textSs)) "0")
        )
      )

      (if (and lineSs textSs)
        (progn
          (setq segments (pa-build-pipe-segments lineSs textSs))
          (princ
            (strcat
              "\n[DBG CHECKPIPESEG] break block count: "
              (itoa (length (pa-get-double-pipe-block-nodes)))
            )
          )
          (setq count (pa-place-check-pipe-segments segments))

          (princ
            (strcat
              "\n[DBG CHECKPIPESEG] segment count: "
              (itoa count)
            )
          )
        )
        (princ "\n[DBG CHECKPIPESEG] select LINE and pipe-size TEXT")
      )
    )
    (princ "\n[DBG CHECKPIPESEG] nothing selected")
  )

  (princ "\n[DBG CHECKPIPESEG] end")
  (princ)
)

(defun pa-draw-double-line (p1 p2 offset lineLayer / ang p1a p2a p1b p2b)
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

(defun c:DOUBLEPIPE (/ ss lineSs textSs segs seg p1 p2 size lineLayer dia offset count skipped)
  (princ "\n複線化するLINEと管径TEXTを選択: ")
  (setq ss
    (ssget
      '(
        (-4 . "<OR")
          (0 . "LINE")
          (0 . "TEXT,MTEXT")
        (-4 . "OR>")
      )
    )
  )

  (if ss
    (progn
      (setq lineSs (pa-filter-lines ss))
      (setq textSs (pa-filter-texts ss))
    )
  )

  (if (and lineSs textSs)
    (progn
      (setq segs (pa-build-pipe-segments lineSs textSs))
      (setq count 0)
      (setq skipped 0)

      (foreach seg segs
        (setq p1 (nth 0 seg))
        (setq p2 (nth 1 seg))
        (setq size (nth 2 seg))
        (setq lineLayer (nth 3 seg))
        (setq dia (atof size))

        (if (> dia 0.0)
          (progn
            (setq offset (/ dia 2.0))
            (pa-draw-double-line p1 p2 offset lineLayer)
            (setq count (1+ count))
          )
          (progn
            (setq skipped (1+ skipped))
            (princ
              (strcat
                "\n[DBG DOUBLEPIPE] skipped segment: size="
                (if size size "")
              )
            )
          )
        )
      )

      (princ
        (strcat
          "\n複線図を作図しました。 count="
          (itoa count)
          " skipped="
          (itoa skipped)
        )
      )
    )
    (princ "\nLINEと管径TEXTを選択してください。")
  )

  (princ)
)
