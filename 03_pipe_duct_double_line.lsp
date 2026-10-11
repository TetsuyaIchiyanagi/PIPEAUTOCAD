;; PipeAutoCAD pipe and duct double-line conversion. Load after modules 01 and 02.
(vl-load-com)

;; ===== 06_double_line.lsp =====
;; PipeAutoCAD 06: double-line conversion
;;
;; Double-line conversion is launched from INSERTPIPEPARTS.
;;
;; Flow:
;; 1. PIPESIZEAUTO places pipe-size TEXT.
;; 2. User edits the pipe-size TEXT.
;; 3. INSERTPIPEPARTS reads LINE and pipe-size TEXT in double-line mode.
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

(defun pa-double-drain-layer-name ()
  "MEP_排水"
)

(defun pa-entity-on-double-drain-layer-p (e / ed layer)
  (setq ed (entget e))
  (setq layer (cdr (assoc 8 ed)))

  (= layer (pa-double-drain-layer-name))
)

(defun pa-filter-double-drain-layer (ss / result i e)
  (setq result (ssadd))
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))

    (if (pa-entity-on-double-drain-layer-p e)
      (ssadd e result)
    )

    (setq i (1+ i))
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-merge-selection-sets (a b / result i e)
  (setq result (ssadd))

  (if a
    (progn
      (setq i 0)
      (while (< i (sslength a))
        (ssadd (ssname a i) result)
        (setq i (1+ i))
      )
    )
  )

  (if b
    (progn
      (setq i 0)
      (while (< i (sslength b))
        (ssadd (ssname b i) result)
        (setq i (1+ i))
      )
    )
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-insert-block-name (e / ed obj name result)
  (setq ed (entget e))
  (setq name (cdr (assoc 2 ed)))

  (if e
    (progn
      (setq obj (vlax-ename->vla-object e))
      (setq result (vl-catch-all-apply 'vlax-get-property (list obj 'EffectiveName)))

      (if (not (vl-catch-all-error-p result))
        (setq name result)
      )
    )
  )

  name
)

(defun pa-insert-block-name-candidates (e / ed obj names value)
  (setq ed (entget e))
  (setq obj (vlax-ename->vla-object e))
  (setq names '())

  (setq value (cdr (assoc 2 ed)))
  (if value
    (setq names (cons value names))
  )

  (setq value (vl-catch-all-apply 'vlax-get-property (list obj 'Name)))
  (if (and (not (vl-catch-all-error-p value)) value)
    (setq names (cons value names))
  )

  (setq value (vl-catch-all-apply 'vlax-get-property (list obj 'EffectiveName)))
  (if (and (not (vl-catch-all-error-p value)) value)
    (setq names (cons value names))
  )

  (pa-unique-list2 names)
)

(defun pa-double-block-name-from-candidates (names / result name)
  (setq result nil)

  (foreach name names
    (if (and (null result) (pa-double-block-name name))
      (setq result (pa-double-block-name name))
    )
  )

  result
)

(defun pa-debug-name-list (names / text name)
  (setq text "")

  (foreach name names
    (if (= text "")
      (setq text name)
      (setq text (strcat text "," name))
    )
  )

  text
)

(defun pa-debug-double-insert-selection (label ss / i e ed layer handle names newName)
  (princ
    (strcat
      "\n[DBG INSERTPIPEPARTS DOUBLE] "
      label
      " insert count: "
      (if ss (itoa (sslength ss)) "0")
    )
  )

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq ed (entget e))
        (setq layer (cdr (assoc 8 ed)))
        (setq handle (cdr (assoc 5 ed)))
        (setq names (pa-insert-block-name-candidates e))
        (setq newName (pa-double-block-name-from-candidates names))

        (princ
          (strcat
            "\n[DBG INSERTPIPEPARTS DOUBLE] "
            label
            " insert "
            (itoa (1+ i))
            " handle="
            (if handle handle "")
            " layer="
            (if layer layer "")
            " names="
            (pa-debug-name-list names)
            " target="
            (if newName newName "-")
          )
        )

        (setq i (1+ i))
      )
    )
  )
)

(defun pa-filter-double-convertible-blocks (ss / result i e)
  (setq result (ssadd))

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))
        (setq e (ssname ss i))

        (if (pa-double-block-name-from-candidates (pa-insert-block-name-candidates e))
          (ssadd e result)
        )

        (setq i (1+ i))
      )
    )
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-point-on-line-ss-p (p lineSs tol / i e ed p1 p2 hit)
  (setq hit nil)

  (if (and p lineSs)
    (progn
      (setq i 0)

      (while (and (< i (sslength lineSs)) (not hit))
        (setq e (ssname lineSs i))
        (setq ed (entget e))
        (setq p1 (cdr (assoc 10 ed)))
        (setq p2 (cdr (assoc 11 ed)))

        (if (and p1 p2 (pa-point-on-seg2 p p1 p2 tol))
          (setq hit T)
        )

        (setq i (1+ i))
      )
    )
  )

  hit
)

(defun pa-double-block-on-line-ss-p (e lineSs / ed p center tol)
  (setq ed (entget e))
  (setq p (cdr (assoc 10 ed)))
  (setq center (pa-double-pipe-block-bbox-center e))
  (setq tol (pa-double-pipe-block-node-tol))

  (or
    (pa-point-on-line-ss-p p lineSs tol)
    (pa-point-on-line-ss-p center lineSs tol)
  )
)

(defun pa-filter-double-blocks-on-lines (ss lineSs / result i e)
  (setq result (ssadd))

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))
        (setq e (ssname ss i))

        (if (pa-double-block-on-line-ss-p e lineSs)
          (ssadd e result)
        )

        (setq i (1+ i))
      )
    )
  )

  (if (> (sslength result) 0)
    result
    nil
  )
)

(defun pa-get-double-blocks-on-lines (lineSs / ss)
  (setq ss
    (ssget
      "_X"
      '((0 . "INSERT"))
    )
  )

  (if ss
    (pa-filter-double-blocks-on-lines
      (pa-filter-double-convertible-blocks ss)
      lineSs
    )
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
  "z00010002B,z01051016B,z01051006B,z01051009B,z01051007B,z01051008B,z01051010B,z01051017B,z01051011B,z01051018B,z02021001B,z02021013B,z02021006B,z02021009B,z02021007B,z02021008B,z02021010B,z02021014B,z02021011B,z02021015B,z02101001B,z02102002B,z02102001B,z02103001B,z02104001B,z02105001B,z02105002B,z02106001B,z02106002B"
)

(defun pa-double-pipe-block-node-tol (/ scale)
  (setq scale (pa-get-block-scale))
  (max 100.0 (* scale 1.5))
)

(defun pa-double-pipe-block-node-extents (p mn mx / u d l r)
  (if (and p mn mx)
    (progn
      (setq u (max 0.0 (- (cadr mx) (cadr p))))
      (setq d (max 0.0 (- (cadr p) (cadr mn))))
      (setq l (max 0.0 (- (car p) (car mn))))
      (setq r (max 0.0 (- (car mx) (car p))))
      (list u d l r)
    )
    nil
  )
)

(defun pa-double-pipe-add-block-node (p handle blockName bboxInfo result / mn mx extents)
  (if p
    (progn
      (if bboxInfo
        (progn
          (setq mn (cadr bboxInfo))
          (setq mx (caddr bboxInfo))
          (setq extents (pa-double-pipe-block-node-extents p mn mx))
        )
      )
      (cons (list p "BLOCK" handle extents blockName) result)
    )
    result
  )
)

(defun pa-double-pipe-block-bbox-info (e / obj minPt maxPt err mn mx center)
  (setq obj (vlax-ename->vla-object e))
  (setq err (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPt 'maxPt)))

  (if (vl-catch-all-error-p err)
    nil
    (progn
      (setq mn (vlax-safearray->list minPt))
      (setq mx (vlax-safearray->list maxPt))
      (setq center
        (list
          (/ (+ (car mn) (car mx)) 2.0)
          (/ (+ (cadr mn) (cadr mx)) 2.0)
          (/ (+ (caddr mn) (caddr mx)) 2.0)
        )
      )
      (list
        center
        mn
        mx
      )
    )
  )
)

(defun pa-double-pipe-block-bbox-center (e / info)
  (setq info (pa-double-pipe-block-bbox-info e))

  (if info
    (car info)
    nil
  )
)

(defun pa-get-double-pipe-block-nodes (/ ss i e ed p bboxInfo center handle blockName result)
  (setq result '())
  (setq ss
    (ssget
      "_X"
      '((0 . "INSERT"))
    )
  )
  (if ss
    (setq ss (pa-filter-double-convertible-blocks ss))
  )

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq ed (entget e))
        (setq p (cdr (assoc 10 ed)))
        (setq handle (cdr (assoc 5 ed)))
        (setq blockName (pa-insert-block-name e))
        (setq bboxInfo (pa-double-pipe-block-bbox-info e))
        (if bboxInfo
          (setq center (car bboxInfo))
          (setq center nil)
        )

        (setq result (pa-double-pipe-add-block-node p handle blockName bboxInfo result))
        (if (and center (or (null p) (not (pa-same-point-p p center 1.0))))
          (setq result (pa-double-pipe-add-block-node center handle blockName bboxInfo result))
        )

        (setq i (1+ i))
      )
    )
  )

  result
)

(defun pa-obsolete-double-pipe-block-bbox-center (e / obj minPt maxPt err mn mx)
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

(defun pa-insert-pipeparts-dia-run (lineSs textSs flowPt logPrefix / segments nodeDiaList insertNodes item p dirs info blk ang dia count)
  (setq segments (pa-build-pipe-segments lineSs textSs))
  (setq nodeDiaList (pa-build-node-sizes segments))
  (setq insertNodes (pa-get-node-types lineSs))
  (setq count 0)

  (princ
    (strcat
      "\n"
      logPrefix
      " segment count: "
      (itoa (length segments))
    )
  )

  (princ
    (strcat
      "\n"
      logPrefix
      " node dia count: "
      (itoa (length nodeDiaList))
    )
  )

  (foreach item nodeDiaList
    (setq p (car item))
    (setq dia (cadr item))
    (setq dirs (pa-get-node-dirs lineSs p 10.0))

    (princ
      (strcat
        "\n"
        logPrefix
        " node dia="
        (pa-format-dia dia)
        " dirs="
        (if dirs (pa-dirs-to-string dirs) "nil")
      )
    )
  )

  (if flowPt
    (progn
      (princ
        (strcat
          "\n"
          logPrefix
          " insert node count: "
          (itoa (length insertNodes))
        )
      )

      (setq pa-defer-line-cuts T)
      (setq pa-line-cut-queue '())

      (foreach item insertNodes
        (setq p (car item))
        (setq dia (pa-get-node-max-dia p nodeDiaList))
        (setq dirs (pa-get-node-dirs lineSs p 10.0))
        (setq info (pa-dirs-to-insert-info dirs lineSs p flowPt))

        (if info
          (progn
            (setq blk (car info))
            (setq ang (cadr info))

            (princ
              (strcat
                "\n"
                logPrefix
                " insert "
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
          (princ
            (strcat
              "\n"
              logPrefix
              " insert info: nil"
            )
          )
        )
      )

      (setq pa-defer-line-cuts nil)
      (pa-apply-deferred-line-cuts)

      (princ
        (strcat
          "\n"
          logPrefix
          " inserted count: "
          (itoa count)
        )
      )
    )
    (princ
      (strcat
        "\n"
        logPrefix
        " flow point is nil; dia check only"
      )
    )
  )

  count
)

(defun pa-obsolete-insertpipeparts-dia-test (/ ss lineSs textSs flowPt segments nodeDiaList item p dirs info blk ang dia count)
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

(defun pa-delete-line-ss (lineSs / i e)
  (if lineSs
    (progn
      (setq i 0)

      (while (< i (sslength lineSs))
        (setq e (ssname lineSs i))
        (entdel e)
        (setq i (1+ i))
      )
    )
  )
)

(defun pa-point-near-double-block-node-p (p blockNodes tol / hit item nodePt)
  (setq hit nil)

  (foreach item blockNodes
    (setq nodePt (car item))

    (if (< (distance p nodePt) tol)
      (setq hit T)
    )
  )

  hit
)

(defun pa-find-near-double-block-node (p blockNodes tol / best bestDist item nodePt d)
  (setq best nil)
  (setq bestDist nil)

  (foreach item blockNodes
    (setq nodePt (car item))
    (setq d (distance p nodePt))

    (if (and (< d tol)
             (or (null bestDist) (< d bestDist))
        )
      (progn
        (setq best item)
        (setq bestDist d)
      )
    )
  )

  best
)

(defun pa-axis-dir-from-points (p otherPt / dx dy)
  (setq dx (- (car otherPt) (car p)))
  (setq dy (- (cadr otherPt) (cadr p)))

  (if (>= (abs dx) (abs dy))
    (if (>= dx 0.0) "R" "L")
    (if (>= dy 0.0) "U" "D")
  )
)

(defun pa-double-block-cut-factor (blockName dir / name)
  (setq name (strcase blockName))

  (cond
    ((= name "Z02102001B") 0.55)
    (T 1.0)
  )
)

(defun pa-double-block-node-gap-by-dir (nodeItem dir / extents blockName factor gap)
  (setq extents (cadddr nodeItem))
  (setq blockName (nth 4 nodeItem))

  (if extents
    (progn
      (setq gap
        (cond
          ((= dir "U") (nth 0 extents))
          ((= dir "D") (nth 1 extents))
          ((= dir "L") (nth 2 extents))
          ((= dir "R") (nth 3 extents))
          (T 0.0)
        )
      )
      (setq factor (pa-double-block-cut-factor blockName dir))
      (* gap factor)
    )
    0.0
  )
)

(defun pa-double-pipe-end-gap (p otherPt dia blockNodes / tol nodeItem dir gap)
  (setq tol (pa-double-pipe-block-node-tol))

  (setq nodeItem (pa-find-near-double-block-node p blockNodes tol))
  (if nodeItem
    (progn
      (setq dir (pa-axis-dir-from-points p otherPt))
      (setq gap (pa-double-block-node-gap-by-dir nodeItem dir))
    )
    (setq gap 0.0)
  )

  gap
)

(defun pa-draw-double-line-gap (p1 p2 offset lineLayer gap1 gap2 / len maxGap q1 q2)
  (setq len (distance p1 p2))
  (setq maxGap (/ len 2.5))

  (if (> gap1 maxGap) (setq gap1 maxGap))
  (if (> gap2 maxGap) (setq gap2 maxGap))

  (setq q1 (polar p1 (angle p1 p2) gap1))
  (setq q2 (polar p2 (angle p2 p1) gap2))

  (if (> (distance q1 q2) 1.0)
    (pa-draw-double-line q1 q2 offset lineLayer)
  )
)

(defun pa-double-block-name (oldName / name)
  (setq name (strcase oldName))

  (cond
    ((= name "Z01051016B") "z02101001B")
    ((= name "Z01051006B") "z02102002B")
    ((= name "Z01051009B") "z02102001B")
    ((= name "Z01051007B") "z02103001B")
    ((= name "Z01051008B") "z02104001B")
    ((= name "Z01051010B") "z02105001B")
    ((= name "Z01051017B") "z02105002B")
    ((= name "Z01051011B") "z02106001B")
    ((= name "Z01051018B") "z02106002B")
    ((= name "Z02021001B") "z02101001B")
    ((= name "Z02021013B") "z02101001B")
    ((= name "Z02021006B") "z02102002B")
    ((= name "Z02021009B") "z02102001B")
    ((= name "Z02021007B") "z02103001B")
    ((= name "Z02021008B") "z02104001B")
    ((= name "Z02021010B") "z02105001B")
    ((= name "Z02021014B") "z02105002B")
    ((= name "Z02021011B") "z02106001B")
    ((= name "Z02021015B") "z02106002B")
    (T nil)
  )
)

(defun pa-nearest-node-dia (p nodeSizes / result bestDist item nodePt dia d)
  (setq result 100.0)
  (setq bestDist nil)

  (foreach item nodeSizes
    (setq nodePt (car item))
    (setq dia (cadr item))
    (setq d (distance p nodePt))

    (if (or (null bestDist) (< d bestDist))
      (progn
        (setq bestDist d)
        (setq result dia)
      )
    )
  )

  result
)

(defun pa-double-block-angle-offset (blkName / name)
  (setq name (strcase blkName))

  (cond
    ((= name "Z02101001B") 0.0)
    ((= name "Z02102002B") 270.0)
    ((= name "Z02102001B") 0.0)
    ((= name "Z02103001B") 270.0)
    ((= name "Z02104001B") 270.0)
    ((= name "Z02105001B") 180.0)
    ((= name "Z02105002B") 180.0)
    ((= name "Z02106001B") 180.0)
    ((= name "Z02106002B") 180.0)
    (T 0.0)
  )
)

(defun pa-normalize-angle-rad (ang / twoPi)
  (setq twoPi (* 2.0 pi))

  (while (< ang 0.0)
    (setq ang (+ ang twoPi))
  )

  (while (>= ang twoPi)
    (setq ang (- ang twoPi))
  )

  ang
)

(defun pa-double-block-insert-angle (blkName oldAngleRad / offsetDeg)
  (setq offsetDeg (pa-double-block-angle-offset blkName))
  (pa-normalize-angle-rad (+ oldAngleRad (* pi (/ offsetDeg 180.0))))
)

(defun pa-insert-double-block (p blkName angleRad dia oldEd / scale layer insertAngle)
  (setq scale dia)
  (setq layer (cdr (assoc 8 oldEd)))
  (setq insertAngle (pa-double-block-insert-angle blkName angleRad))

  (if (tblsearch "BLOCK" blkName)
    (entmakex
      (list
        '(0 . "INSERT")
        (cons 2 blkName)
        (cons 8 (if layer layer "0"))
        (cons 10 p)
        (cons 41 scale)
        (cons 42 scale)
        (cons 43 scale)
        (cons 50 insertAngle)
      )
    )
    (progn
      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS DOUBLE] missing block: "
          blkName
        )
      )
      nil
    )
  )
)

(defun pa-convert-double-blocks (blockSs nodeSizes / i e ed oldName names newName p angle dia inserted count skipped)
  (setq count 0)
  (setq skipped 0)

  (if blockSs
    (progn
      (setq i 0)

      (while (< i (sslength blockSs))
        (setq e (ssname blockSs i))
        (setq ed (entget e))
        (setq oldName (pa-insert-block-name e))
        (setq names (pa-insert-block-name-candidates e))
        (setq newName (pa-double-block-name-from-candidates names))

        (if newName
          (progn
            (setq p (cdr (assoc 10 ed)))
            (setq angle (cdr (assoc 50 ed)))
            (if (null angle) (setq angle 0.0))
            (setq dia (pa-nearest-node-dia p nodeSizes))

            (princ
              (strcat
                "\n[DBG INSERTPIPEPARTS DOUBLE] replace "
                oldName
                " -> "
                newName
                " dia="
                (pa-format-dia dia)
              )
            )

            (setq inserted (pa-insert-double-block p newName angle dia ed))

            (if inserted
              (progn
                (entdel e)
                (setq count (1+ count))
              )
              (setq skipped (1+ skipped))
            )
          )
          (progn
            (setq skipped (1+ skipped))
            (princ
              (strcat
                "\n[DBG INSERTPIPEPARTS DOUBLE] skip block: "
                (if oldName oldName "")
              )
            )
          )
        )

        (setq i (1+ i))
      )
    )
  )

  (princ
    (strcat
      "\n[DBG INSERTPIPEPARTS DOUBLE] converted blocks: "
      (itoa count)
      " skipped="
      (itoa skipped)
    )
  )

  count
)

(defun pa-draw-double-pipe-segments (segs / seg p1 p2 size dia offset lineLayer blockNodes gap1 gap2 count skipped)
  (setq count 0)
  (setq skipped 0)
  (setq blockNodes (pa-get-double-pipe-block-nodes))

  (foreach seg segs
    (setq p1 (nth 0 seg))
    (setq p2 (nth 1 seg))
    (setq size (nth 2 seg))
    (setq lineLayer (nth 3 seg))
    (setq dia (atof size))

    (if (> dia 0.0)
      (progn
        (setq offset (/ dia 2.0))
        (setq gap1 (pa-double-pipe-end-gap p1 p2 dia blockNodes))
        (setq gap2 (pa-double-pipe-end-gap p2 p1 dia blockNodes))
        (pa-draw-double-line-gap p1 p2 offset lineLayer gap1 gap2)
        (setq count (1+ count))
      )
      (progn
        (setq skipped (1+ skipped))
        (princ
          (strcat
            "\n[DBG INSERTPIPEPARTS DOUBLE] skipped segment: size="
            (if size size "")
          )
        )
      )
    )
  )

  (princ
    (strcat
      "\n[DBG INSERTPIPEPARTS DOUBLE] double-line segments: "
      (itoa count)
      " skipped="
      (itoa skipped)
    )
  )

  count
)

(defun pa-apply-default-pipe-size (segs defaultSize / result seg size)
  (setq result '())

  (foreach seg segs
    (setq size (nth 2 seg))
    (if (or (null size) (= size "") (<= (atof size) 0.0))
      (setq
        result
        (cons
          (list
            (nth 0 seg)
            (nth 1 seg)
            defaultSize
            (nth 3 seg)
          )
          result
        )
      )
      (setq result (cons seg result))
    )
  )

  (reverse result)
)

(defun pa-convert-double-pipe-run (lineSs textSs blockSs / segs nodeSizes)
  (if (and lineSs textSs)
    (progn
      (setq segs (pa-build-pipe-segments lineSs textSs))
      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS DOUBLE] segment count: "
          (itoa (length segs))
        )
      )

      (setq nodeSizes (pa-build-node-sizes segs))
      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS DOUBLE] node dia count: "
          (itoa (length nodeSizes))
        )
      )

      (if blockSs
        (pa-convert-double-blocks blockSs nodeSizes)
        (princ "\n[DBG INSERTPIPEPARTS DOUBLE] no blocks selected")
      )

      (pa-draw-double-pipe-segments segs)
      (pa-delete-line-ss lineSs)

      (princ "\n[DBG INSERTPIPEPARTS] double-line conversion done")
    )
    (princ "\nSelect LINE and pipe-size TEXT.")
  )

  (if (and lineSs textSs) T nil)
)

(defun pa-convert-double-pipe-run-default-dia (lineSs blockSs defaultDia / segs nodeSizes defaultSize)
  (if lineSs
    (progn
      (setq defaultSize (pa-format-dia defaultDia))
      (setq segs (pa-build-pipe-segments lineSs nil))
      (setq segs (pa-apply-default-pipe-size segs defaultSize))
      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS DOUBLE] no pipe-size TEXT; default dia="
          defaultSize
        )
      )
      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS DOUBLE] segment count: "
          (itoa (length segs))
        )
      )

      (setq nodeSizes (pa-build-node-sizes segs))
      (princ
        (strcat
          "\n[DBG INSERTPIPEPARTS DOUBLE] node dia count: "
          (itoa (length nodeSizes))
        )
      )

      (if blockSs
        (pa-convert-double-blocks blockSs nodeSizes)
        (princ "\n[DBG INSERTPIPEPARTS DOUBLE] no blocks selected")
      )

      (pa-draw-double-pipe-segments segs)
      (pa-delete-line-ss lineSs)

      (princ "\n[DBG INSERTPIPEPARTS] double-line conversion done")
    )
    (princ "\nSelect LINE.")
  )

  (if lineSs T nil)
)

(defun pa-obsolete-doublepipe-command (/ ss lineSs textSs segs seg p1 p2 size lineLayer dia offset count skipped)
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

(setq c:CONVERTDOUBLEPIPE nil)
(setq c:DOUBLEPIPE nil)
(setq c:INSERTPIPEPARTS_DIA_TEST nil)


;; ===== 07_duct_double_line.lsp =====
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
    ((pa-same-dirs d '("U" "RU")) (list "z03104001B" 125.0))

    ((pa-same-dirs d '("U" "LU")) (list "DUCT_EL45" 90.0))

    ((pa-same-dirs d '("L" "LU")) (list "DUCT_EL45" 180.0))
    ((pa-same-dirs d '("L" "LD")) (list "DUCT_EL45" 180.0))

    ((pa-same-dirs d '("D" "LD")) (list "DUCT_EL45" 270.0))
    ((pa-same-dirs d '("D" "RD")) (list "DUCT_EL45" 270.0))
    ((pa-same-dirs d '("R" "RD")) (list "DUCT_EL45" 0.0))

    ;; 135-degree elbows.
    ((pa-same-dirs d '("R" "LU")) (list "z03104001B" 135.0))
    ((pa-same-dirs d '("R" "LD")) (list "z03104001B" 0.0))

    ((pa-same-dirs d '("U" "RD")) (list "z03104001B" 90.0))
    ((pa-same-dirs d '("U" "LD")) (list "z03104001B" 225.0))

    ((pa-same-dirs d '("L" "RU")) (list "z03104001B" 180.0))
    ((pa-same-dirs d '("L" "RD")) (list "z03104001B" 315.0))

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

