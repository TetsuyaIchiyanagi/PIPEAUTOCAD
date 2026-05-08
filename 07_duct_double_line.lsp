;; PipeAutoCAD 07: round duct double-line workflow
;;
;; Current scope:
;; DUCTDOUBLE -> read duct diameter -> select LINE -> place direction/angle
;; debug TEXT at each LINE intersection.
;;
;; Later steps will reuse the same node and block table functions for block
;; insertion, double-line offset, and line trimming.

(defun pa-duct-node-tol ()
  10.0
)

(defun pa-duct-debug-text-height ()
  (* 2.0 (getvar "LTSCALE"))
)

(defun pa-duct-debug-text-offset ()
  (* 3.0 (getvar "LTSCALE"))
)

(defun pa-duct-valid-dia-p (dia)
  (member dia '(200 300 400))
)

(defun pa-duct-read-dia (/ dia done)
  (setq done nil)

  (while (not done)
    (setq dia (getint "\nDuct diameter [200/300/400] <200>: "))

    (cond
      ((null dia)
        (setq dia 200)
        (setq done T)
      )
      ((pa-duct-valid-dia-p dia)
        (setq done T)
      )
      (T
        (princ "\nEnter 200, 300, or 400.")
      )
    )
  )

  dia
)

(defun pa-duct-scale-from-dia (dia)
  (/ (float dia) 100.0)
)

(defun pa-duct-offset-from-dia (dia)
  (/ (float dia) 2.0)
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
    ((pa-same-dirs d '("R" "U")) (list "DUCT_EL90" 0.0))
    ((pa-same-dirs d '("L" "U")) (list "DUCT_EL90" 90.0))
    ((pa-same-dirs d '("L" "D")) (list "DUCT_EL90" 180.0))
    ((pa-same-dirs d '("R" "D")) (list "DUCT_EL90" 270.0))

    ;; 45-degree elbows.
    ;; Add both endpoint direction patterns because a 45-degree joint can be
    ;; picked from either side of the diagonal center line.
    ((pa-same-dirs d '("R" "RU")) (list "DUCT_EL45" 0.0))
    ((pa-same-dirs d '("U" "RU")) (list "DUCT_EL45" 0.0))
    ((pa-same-dirs d '("R" "LU")) (list "DUCT_EL45" 0.0))

    ((pa-same-dirs d '("U" "LU")) (list "DUCT_EL45" 90.0))
    ((pa-same-dirs d '("U" "RD")) (list "DUCT_EL45" 90.0))
    ((pa-same-dirs d '("L" "RU")) (list "DUCT_EL45" 90.0))

    ((pa-same-dirs d '("L" "LU")) (list "DUCT_EL45" 180.0))
    ((pa-same-dirs d '("L" "LD")) (list "DUCT_EL45" 180.0))
    ((pa-same-dirs d '("D" "RU")) (list "DUCT_EL45" 180.0))

    ((pa-same-dirs d '("D" "LD")) (list "DUCT_EL45" 270.0))
    ((pa-same-dirs d '("D" "RD")) (list "DUCT_EL45" 270.0))
    ((pa-same-dirs d '("R" "RD")) (list "DUCT_EL45" 0.0))
    ((pa-same-dirs d '("D" "LU")) (list "DUCT_EL45" 270.0))

    ;; 45-degree crossing points. These are debug classifications for STEP1;
    ;; insertion rules can be split later if a crossing duct block is needed.
    ((pa-same-dirs d '("L" "R" "LU" "RD")) (list "DUCT_CROSS45" 0.0))
    ((pa-same-dirs d '("L" "R" "RU" "LD")) (list "DUCT_CROSS45" 0.0))
    ((pa-same-dirs d '("U" "D" "LU" "RD")) (list "DUCT_CROSS45" 90.0))
    ((pa-same-dirs d '("U" "D" "RU" "LD")) (list "DUCT_CROSS45" 90.0))

    ;; T branches
    ((pa-same-dirs d '("L" "R" "U")) (list "DUCT_TEE" 0.0))
    ((pa-same-dirs d '("U" "D" "R")) (list "DUCT_TEE" 90.0))
    ((pa-same-dirs d '("L" "R" "D")) (list "DUCT_TEE" 180.0))
    ((pa-same-dirs d '("U" "D" "L")) (list "DUCT_TEE" 270.0))

    (T nil)
  )
)

(defun pa-duct-debug-label (dia dirs info / label)
  (setq label
    (strcat
      "DIA="
      (itoa dia)
      " DIR="
      (if dirs (pa-dirs-to-string dirs) "-")
    )
  )

  (if info
    (strcat
      label
      " BLOCK="
      (car info)
      " ANG="
      (rtos (cadr info) 2 0)
    )
    (strcat label " BLOCK=- ANG=-")
  )
)

(defun pa-duct-place-debug-text (p label / txtPt txtH)
  (setq txtH (pa-duct-debug-text-height))
  (setq txtPt (list (+ (car p) (pa-duct-debug-text-offset)) (cadr p) 0.0))

  (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(100 . "AcDbText")
      (cons 8 "USR2")
      (cons 10 txtPt)
      (cons 11 txtPt)
      (cons 40 txtH)
      (cons 1 label)
      (cons 50 0.0)
      (cons 7 (getvar "TEXTSTYLE"))
      '(72 . 0)
      '(73 . 2)
    )
  )
)

(defun pa-duct-place-node-debug-texts (lineSs dia / nodes p dirs info count)
  (setq nodes (pa-duct-get-line-intersections lineSs))
  (setq count 0)

  (foreach p nodes
    (setq dirs (pa-get-node-dirs lineSs p (pa-duct-node-tol)))
    (setq info (pa-duct-dirs-to-block dirs))
    (pa-duct-place-debug-text p (pa-duct-debug-label dia dirs info))
    (setq count (1+ count))
  )

  count
)

(defun c:DUCTDOUBLE (/ dia lineSs count)
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
  (setq lineSs (ssget '((0 . "LINE"))))

  (if lineSs
    (progn
      (setq count (pa-duct-place-node-debug-texts lineSs dia))
      (princ (strcat "\nDUCTDOUBLE STEP1 debug labels: " (itoa count)))
    )
    (princ "\nNo LINE selected.")
  )

  (princ)
)
(princ)
