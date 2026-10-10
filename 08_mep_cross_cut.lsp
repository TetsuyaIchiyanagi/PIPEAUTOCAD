;; PipeAutoCAD 08: MEP crossing cut
;;
;; MEPCROSSCUT cuts LINE/LWPOLYLINE entities around crossing points.

(defun pa-mep-cross-target-layers ()
  '("MEP_排水" "MEP_雨水" "MEP_ガス" "MEP_給湯" "MEP_給水")
)

(setq pa-mep-cross-debug-enabled nil)
(setq pa-mep-cross-apply-debug-enabled nil)
(setq pa-mep-cross-default-gap 100.0)
(setq pa-mep-cross-last-step "")
(setq pa-mep-cross-last-info "")

(defun pa-mep-cross-handle (e)
  (if (and e (entget e) (cdr (assoc 5 (entget e))))
    (cdr (assoc 5 (entget e)))
    "<nil>"
  )
)

(defun pa-mep-cross-type-text (value)
  (vl-princ-to-string (type value))
)

(defun pa-mep-cross-value-text (value / text)
  (setq text (vl-princ-to-string value))
  (if (> (strlen text) 180)
    (strcat (substr text 1 180) "...")
    text
  )
)

(defun pa-mep-cross-set-step (step info)
  (setq pa-mep-cross-last-step step)
  (setq pa-mep-cross-last-info info)
  (if pa-mep-cross-debug-enabled
    (princ (strcat "\n[MEPCROSSCUT DBG] " step " " info))
  )
)

(defun pa-mep-cross-set-apply-step (step info)
  (setq pa-mep-cross-last-step step)
  (setq pa-mep-cross-last-info info)
  (if pa-mep-cross-apply-debug-enabled
    (princ (strcat "\n[MEPCROSSCUT DBG] " step " " info))
  )
)

(defun pa-mep-cross-set-step-quiet (step info)
  (setq pa-mep-cross-last-step step)
  (setq pa-mep-cross-last-info info)
)

(defun pa-mep-cross-set-value-step (step value)
  (pa-mep-cross-set-step
    step
    (strcat
      "type="
      (pa-mep-cross-type-text value)
      " value="
      (pa-mep-cross-value-text value)
    )
  )
)

(defun pa-mep-cross-layer-priority (layer / layers index result)
  (setq layers (pa-mep-cross-target-layers))
  (setq index 0)
  (setq result nil)

  (while layers
    (if (= (strcase layer) (strcase (car layers)))
      (setq result index)
    )
    (setq index (1+ index))
    (setq layers (cdr layers))
  )

  result
)

(defun pa-mep-cross-target-entity-p (e / ed typ layer)
  (setq ed (entget e))
  (setq typ (cdr (assoc 0 ed)))
  (setq layer (cdr (assoc 8 ed)))

  (and
    (member typ '("LINE" "LWPOLYLINE"))
    (pa-mep-cross-layer-priority layer)
  )
)

(defun pa-mep-cross-list-p (value)
  (and value (eq (type value) 'LIST))
)

(defun pa-mep-cross-number-p (value)
  (member (type value) '(INT REAL))
)

(defun pa-mep-cross-point-p (value)
  (and
    (pa-mep-cross-list-p value)
    (pa-mep-cross-number-p (car value))
    (pa-mep-cross-number-p (cadr value))
  )
)

(defun pa-mep-cross-point3d (value / values)
  (setq values (pa-mep-cross-variant-list value))
  (if (pa-mep-cross-point-p values)
    (list
      (car values)
      (cadr values)
      (if (pa-mep-cross-number-p (caddr values)) (caddr values) 0.0)
    )
  )
)

(defun pa-mep-cross-variant-list (value / variantValue safeArray)
  (cond
    ((pa-mep-cross-list-p value) value)
    ((eq (type value) 'VARIANT)
      (setq variantValue (vlax-variant-value value))
      (pa-mep-cross-variant-list variantValue)
    )
    ((eq (type value) 'SAFEARRAY)
      (vlax-safearray->list value)
    )
  )
)

(defun pa-mep-cross-point-list (values / result)
  (setq values (pa-mep-cross-variant-list values))
  (setq result '())

  (cond
    ((null values) nil)
    ((pa-mep-cross-point-p (car values))
      values
    )
    (T
      (while
        (and
          values
          (pa-mep-cross-number-p (car values))
          (pa-mep-cross-number-p (cadr values))
        )
        (setq result
          (cons
            (list
              (car values)
              (cadr values)
              (if (pa-mep-cross-number-p (caddr values)) (caddr values) 0.0)
            )
            result
          )
        )
        (if (pa-mep-cross-number-p (caddr values))
          (setq values (cdddr values))
          (setq values nil)
        )
      )

      (reverse result)
    )
  )
)

(defun pa-mep-cross-flat-point-list (values / result x y z)
  (setq values (pa-mep-cross-variant-list values))
  (setq result '())

  (while (eq (type values) 'LIST)
    (setq x (car values))
    (setq values (cdr values))
    (setq y (if (eq (type values) 'LIST) (car values) nil))
    (setq values (if (eq (type values) 'LIST) (cdr values) nil))
    (setq z (if (eq (type values) 'LIST) (car values) 0.0))
    (setq values (if (eq (type values) 'LIST) (cdr values) nil))

    (if (and (pa-mep-cross-number-p x) (pa-mep-cross-number-p y))
      (setq result
        (append
          result
          (list
            (list
              x
              y
              (if (pa-mep-cross-number-p z) z 0.0)
            )
          )
        )
      )
    )
  )

  result
)

(defun pa-mep-cross-intersections (e1 e2 / obj1 obj2 values)
  (setq obj1 (vlax-ename->vla-object e1))
  (setq obj2 (vlax-ename->vla-object e2))
  (setq values (vl-catch-all-apply 'vlax-invoke (list obj1 'IntersectWith obj2 acExtendNone)))

  (if (or (vl-catch-all-error-p values) (null values))
    nil
    (progn
      (pa-mep-cross-set-value-step "intersect-raw" values)
      (pa-mep-cross-flat-point-list values)
    )
  )
)

(defun pa-mep-cross-closest-point (e p / obj result)
  (setq obj (vlax-ename->vla-object e))
  (setq result (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj p)))

  (if (vl-catch-all-error-p result)
    nil
    (pa-mep-cross-point3d result)
  )
)

(defun pa-mep-cross-dist-at-point (e p / obj q result)
  (setq obj (vlax-ename->vla-object e))
  (setq q (pa-mep-cross-closest-point e p))

  (if q
    (progn
      (setq result (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list obj q)))
      (if (vl-catch-all-error-p result)
        nil
        result
      )
    )
  )
)

(defun pa-mep-cross-curve-endpoints (e / obj startPt endPt)
  (setq obj (vlax-ename->vla-object e))
  (setq startPt (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj)))
  (if (vl-catch-all-error-p startPt)
    (setq startPt nil)
    (setq startPt (pa-mep-cross-point3d startPt))
  )
  (setq endPt (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj)))
  (if (vl-catch-all-error-p endPt)
    (setq endPt nil)
    (setq endPt (pa-mep-cross-point3d endPt))
  )
  (list startPt endPt)
)

(defun pa-mep-cross-endpoint-hit-p (e p / endpoints startPt endPt tol)
  (setq tol 0.001)
  (setq endpoints (pa-mep-cross-curve-endpoints e))
  (setq startPt (car endpoints))
  (setq endPt (cadr endpoints))
  (or
    (and startPt (pa-mep-cross-same-point-p p startPt tol))
    (and endPt (pa-mep-cross-same-point-p p endPt tol))
  )
)

(defun pa-mep-cross-endpoint-contact-p (e1 e2 p)
  (or
    (pa-mep-cross-endpoint-hit-p e1 p)
    (pa-mep-cross-endpoint-hit-p e2 p)
  )
)

(defun pa-mep-cross-vertical-score (e p / obj q param deriv dx dy)
  (setq obj (vlax-ename->vla-object e))
  (setq q (pa-mep-cross-closest-point e p))

  (if q
    (progn
      (setq param (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list obj q)))
      (if (vl-catch-all-error-p param)
        0.0
        (progn
          (setq deriv (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list obj param)))
          (if (vl-catch-all-error-p deriv)
            0.0
            (progn
              (setq dx (abs (car deriv)))
              (setq dy (abs (cadr deriv)))
              (- dy dx)
            )
          )
        )
      )
    )
    0.0
  )
)

(defun pa-mep-cross-cut-target (e1 e2 p / ed1 ed2 pr1 pr2 v1 v2)
  (setq ed1 (entget e1))
  (setq ed2 (entget e2))
  (setq pr1 (pa-mep-cross-layer-priority (cdr (assoc 8 ed1))))
  (setq pr2 (pa-mep-cross-layer-priority (cdr (assoc 8 ed2))))

  (cond
    ((< pr1 pr2) e1)
    ((< pr2 pr1) e2)
    (T
      (setq v1 (pa-mep-cross-vertical-score e1 p))
      (setq v2 (pa-mep-cross-vertical-score e2 p))
      (if (>= v1 v2) e1 e2)
    )
  )
)

(defun pa-mep-cross-put-cut (e dist cuts / handle item found result)
  (setq handle (pa-mep-cross-handle e))
  (setq found nil)
  (setq result '())

  (foreach item cuts
    (if (= handle (car item))
      (progn
        (setq found T)
        (setq result (cons (list handle e (cons dist (caddr item))) result))
      )
      (setq result (cons item result))
    )
  )

  (if found
    (reverse result)
    (cons (list handle e (list dist)) cuts)
  )
)

(defun pa-mep-cross-entity-props (ed / props)
  (setq props '())

  (foreach code '(8 6 39 48 62 67 210 370 390 410 420 430 440)
    (if (assoc code ed)
      (setq props (append props (list (assoc code ed))))
    )
  )

  props
)

(defun pa-mep-cross-remove-code (ed code / result item)
  (setq result '())
  (foreach item ed
    (if (/= (car item) code)
      (setq result (append result (list item)))
    )
  )
  result
)

(defun pa-mep-cross-put-dxf (ed pair / code)
  (setq code (car pair))
  (append (pa-mep-cross-remove-code ed code) (list pair))
)

(defun pa-mep-cross-copy-entity-props-to (newEnt oldEd / newEd code pair)
  (if newEnt
    (progn
      (setq newEd (entget newEnt))
      (foreach code '(8 6 39 48 62 67 210 370 390 410 420 430 440)
        (setq pair (assoc code oldEd))
        (if pair
          (setq newEd (pa-mep-cross-put-dxf newEd pair))
          (setq newEd (pa-mep-cross-remove-code newEd code))
        )
      )
      (entmod newEd)
      (entupd newEnt)
      newEnt
    )
  )
)

(defun pa-mep-cross-skip-line-code-p (code)
  (member code '(-1 5 330 10 11))
)

(defun pa-mep-cross-line-dxf-from-original (ed p1 p2 / result item code)
  (setq result '())

  (foreach item ed
    (setq code (car item))
    (if (not (pa-mep-cross-skip-line-code-p code))
      (setq result (append result (list item)))
    )
  )

  (append result (list (cons 10 p1) (cons 11 p2)))
)

(defun pa-mep-cross-point2d (p)
  (list (car p) (cadr p))
)

(defun pa-mep-cross-same-point-p (a b tol)
  (and
    (pa-mep-cross-point-p a)
    (pa-mep-cross-point-p b)
    (< (distance a b) tol)
  )
)

(defun pa-mep-cross-add-point (pts p / lastPt)
  (setq p (pa-mep-cross-point3d p))
  (if p
    (progn
      (setq lastPt (car (last pts)))
      (if (and lastPt (pa-mep-cross-same-point-p lastPt p 0.001))
        pts
        (append pts (list p))
      )
    )
    pts
  )
)

(defun pa-mep-cross-interval-points (obj startDist endDist / pts startParam endParam k p)
  (pa-mep-cross-set-step-quiet
    "interval-start"
    (strcat
      "start="
      (rtos startDist 2 3)
      " end="
      (rtos endDist 2 3)
    )
  )
  (setq p (pa-mep-cross-point3d (vlax-curve-getPointAtDist obj startDist)))
  (setq pts (if p (list p) '()))
  (setq startParam (vlax-curve-getParamAtDist obj startDist))
  (setq endParam (vlax-curve-getParamAtDist obj endDist))
  (pa-mep-cross-set-step-quiet
    "interval-param"
    (strcat
      "startParam="
      (pa-mep-cross-value-text startParam)
      " endParam="
      (pa-mep-cross-value-text endParam)
    )
  )
  (setq k (1+ (fix startParam)))

  (while (< k endParam)
    (setq p (pa-mep-cross-point3d (vlax-curve-getPointAtParam obj k)))
    (if p
      (setq pts (pa-mep-cross-add-point pts p))
    )
    (setq k (1+ k))
  )

  (setq p (pa-mep-cross-point3d (vlax-curve-getPointAtDist obj endDist)))
  (if p
    (pa-mep-cross-add-point pts p)
    pts
  )
)

(defun pa-mep-cross-line-ed-with-points (ed p1 p2)
  (setq ed (pa-mep-cross-remove-code ed 10))
  (setq ed (pa-mep-cross-remove-code ed 11))
  (append ed (list (cons 10 p1) (cons 11 p2)))
)

(defun pa-mep-cross-make-line (p1 p2 ed / sourceEnt sourceObj newObj newEnt newEd)
  (setq p1 (pa-mep-cross-point3d p1))
  (setq p2 (pa-mep-cross-point3d p2))
  (if (and p1 p2 (> (distance p1 p2) 1.0))
    (progn
      (setq sourceEnt (cdr (assoc -1 ed)))
      (if sourceEnt
        (progn
          (setq sourceObj (vlax-ename->vla-object sourceEnt))
          (setq newObj (vl-catch-all-apply 'vla-copy (list sourceObj)))
          (if (not (vl-catch-all-error-p newObj))
            (progn
              (setq newEnt (vlax-vla-object->ename newObj))
              (setq newEd (entget newEnt))
              (setq newEd (pa-mep-cross-line-ed-with-points newEd p1 p2))
              (entmod newEd)
              (entupd newEnt)
              newEnt
            )
          )
        )
        (progn
          (setq newEnt
            (entmakex (pa-mep-cross-line-dxf-from-original ed p1 p2))
          )
          (pa-mep-cross-copy-entity-props-to newEnt ed)
        )
      )
    )
  )
)

(defun pa-mep-cross-line-interval-points (obj startDist endDist / p1 p2)
  (setq p1 (pa-mep-cross-point3d (vlax-curve-getPointAtDist obj startDist)))
  (setq p2 (pa-mep-cross-point3d (vlax-curve-getPointAtDist obj endDist)))
  (if (and p1 p2)
    (list p1 p2)
    '()
  )
)

(defun pa-mep-cross-make-lwpolyline (pts ed / data filtered p)
  (setq filtered '())
  (foreach p pts
    (setq p (pa-mep-cross-point3d p))
    (if p
      (setq filtered (append filtered (list p)))
    )
  )
  (setq pts filtered)
  (if (> (length pts) 1)
    (progn
      (setq data
        (append
          (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity"))
          (pa-mep-cross-entity-props ed)
          (list '(100 . "AcDbPolyline") (cons 90 (length pts)) '(70 . 0))
        )
      )

      (foreach p pts
        (setq data (append data (list (cons 10 (pa-mep-cross-point2d p)))))
      )

      (pa-mep-cross-copy-entity-props-to (entmakex data) ed)
    )
  )
)

(defun pa-mep-cross-make-curve-part (typ pts ed)
  (cond
    ((equal typ "LINE")
      (pa-mep-cross-make-line (car pts) (car (last pts)) ed)
    )
    ((equal typ "LWPOLYLINE")
      (pa-mep-cross-make-lwpolyline pts ed)
    )
  )
)

(defun pa-mep-cross-normalize-dists (values / result value)
  (setq result '())

  (while (pa-mep-cross-list-p values)
    (setq value (car values))
    (if (pa-mep-cross-number-p value)
      (setq result (append result (list value)))
    )
    (setq values (cdr values))
  )

  (if (pa-mep-cross-number-p values)
    (setq result (append result (list values)))
  )

  result
)

(defun pa-mep-cross-sort-normalized-dists (values / sorted remaining value inserted result item)
  (setq sorted '())

  (while (pa-mep-cross-list-p values)
    (setq value (car values))
    (setq remaining sorted)
    (setq result '())
    (setq inserted nil)

    (while (pa-mep-cross-list-p remaining)
      (setq item (car remaining))
      (if (and (not inserted) (< value item))
        (progn
          (setq result (append result (list value)))
          (setq inserted T)
        )
      )
      (setq result (append result (list item)))
      (setq remaining (cdr remaining))
    )

    (if (not inserted)
      (setq result (append result (list value)))
    )

    (setq sorted result)
    (setq values (cdr values))
  )

  sorted
)

(defun pa-mep-cross-merge-cut-ranges (dists gap total / ranges d a b result item lastItem)
  (setq dists (pa-mep-cross-sort-normalized-dists (pa-mep-cross-normalize-dists dists)))
  (setq ranges '())

  (while (pa-mep-cross-list-p dists)
    (setq d (car dists))
    (setq a (max 0.0 (- d gap)))
    (setq b (min total (+ d gap)))
    (if (> b a)
      (setq ranges (append ranges (list (list a b))))
    )
    (setq dists (cdr dists))
  )

  (setq result '())
  (foreach item ranges
    (if (null result)
      (setq result (list item))
      (progn
        (setq lastItem (car (last result)))
        (if (<= (car item) (cadr lastItem))
          (setq result
            (append
              (reverse (cdr (reverse result)))
              (list (list (car lastItem) (max (cadr lastItem) (cadr item))))
            )
          )
          (setq result (append result (list item)))
        )
      )
    )
  )

  result
)

(defun pa-mep-cross-keep-ranges (cutRanges total / result cursor item)
  (setq result '())
  (setq cursor 0.0)

  (while (pa-mep-cross-list-p cutRanges)
    (setq item (car cutRanges))
    (if (> (car item) cursor)
      (setq result (append result (list (list cursor (car item)))))
    )
    (setq cursor (max cursor (cadr item)))
    (setq cutRanges (cdr cutRanges))
  )

  (if (< cursor total)
    (setq result (append result (list (list cursor total))))
  )

  result
)

(defun pa-mep-cross-insert-range (range ranges / result inserted item)
  (setq result '())
  (setq inserted nil)

  (while (pa-mep-cross-list-p ranges)
    (setq item (car ranges))
    (if (and (not inserted) (< (car range) (car item)))
      (progn
        (setq result (append result (list range)))
        (setq inserted T)
      )
    )
    (setq result (append result (list item)))
    (setq ranges (cdr ranges))
  )

  (if inserted
    result
    (append result (list range))
  )
)

(defun pa-mep-cross-simple-cut-ranges (dists gap total / result d a b range)
  (setq dists (pa-mep-cross-normalize-dists dists))
  (setq result '())

  (while (pa-mep-cross-list-p dists)
    (setq d (car dists))
    (if (pa-mep-cross-number-p d)
      (progn
        (setq a (max 0.0 (- d gap)))
        (setq b (min total (+ d gap)))
        (if (> b a)
          (progn
            (setq range (list a b))
            (setq result (pa-mep-cross-insert-range range result))
          )
        )
      )
    )
    (setq dists (cdr dists))
  )

  result
)

(defun pa-mep-cross-simple-merge-ranges (ranges / result item lastItem)
  (setq result '())

  (while (pa-mep-cross-list-p ranges)
    (setq item (car ranges))
    (if (null result)
      (setq result (list item))
      (progn
        (setq lastItem (car (last result)))
        (if (<= (car item) (cadr lastItem))
          (setq result
            (append
              (reverse (cdr (reverse result)))
              (list (list (car lastItem) (max (cadr lastItem) (cadr item))))
            )
          )
          (setq result (append result (list item)))
        )
      )
    )
    (setq ranges (cdr ranges))
  )

  result
)

(defun pa-mep-cross-line-distance-list (values / result value)
  (setq result '())

  (while (eq (type values) 'LIST)
    (setq value (car values))
    (if (pa-mep-cross-number-p value)
      (setq result (cons value result))
    )
    (setq values (cdr values))
  )

  (if (pa-mep-cross-number-p values)
    (setq result (cons values result))
  )

  (reverse result)
)

(defun pa-mep-cross-line-subtract-keep (keeps cutStart cutEnd / result item keepStart keepEnd)
  (setq result '())

  (while (eq (type keeps) 'LIST)
    (setq item (car keeps))
    (setq keepStart (car item))
    (setq keepEnd (cadr item))

    (cond
      ((or (<= cutEnd keepStart) (>= cutStart keepEnd))
        (setq result (cons item result))
      )
      (T
        (if (> cutStart keepStart)
          (setq result (cons (list keepStart cutStart) result))
        )
        (if (< cutEnd keepEnd)
          (setq result (cons (list cutEnd keepEnd) result))
        )
      )
    )

    (setq keeps (cdr keeps))
  )

  (reverse result)
)

(defun pa-mep-cross-apply-line-cuts (e dists gap / ed obj total item pts count cutStart cutEnd keeps)
  (pa-mep-cross-set-step-quiet
    "line-start"
    (strcat
      "handle="
      (pa-mep-cross-handle e)
      " dists-type="
      (pa-mep-cross-type-text dists)
      " dists="
      (pa-mep-cross-value-text dists)
    )
  )
  (setq ed (entget e))
  (pa-mep-cross-set-step-quiet "line-entget" (strcat "handle=" (pa-mep-cross-handle e)))
  (setq obj (vlax-ename->vla-object e))
  (pa-mep-cross-set-step-quiet "line-object" (strcat "handle=" (pa-mep-cross-handle e)))
  (setq total (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
  (pa-mep-cross-set-step-quiet "line-total" (strcat "total=" (rtos total 2 3) " gap=" (rtos gap 2 3)))
  (setq dists (pa-mep-cross-line-distance-list dists))
  (pa-mep-cross-set-step-quiet "line-dists" (pa-mep-cross-value-text dists))
  (setq keeps (list (list 0.0 total)))

  (while (eq (type dists) 'LIST)
    (setq item (car dists))
    (setq cutStart (max 0.0 (- item gap)))
    (setq cutEnd (min total (+ item gap)))
    (pa-mep-cross-set-step-quiet
      "line-subtract"
      (strcat
        "cutStart="
        (rtos cutStart 2 3)
        " cutEnd="
        (rtos cutEnd 2 3)
        " keeps="
        (pa-mep-cross-value-text keeps)
      )
    )
    (if (> cutEnd cutStart)
      (setq keeps (pa-mep-cross-line-subtract-keep keeps cutStart cutEnd))
    )
    (setq dists (cdr dists))
  )

  (pa-mep-cross-set-step-quiet "line-keeps" (pa-mep-cross-value-text keeps))
  (setq count 0)

  (while (eq (type keeps) 'LIST)
    (setq item (car keeps))
    (pa-mep-cross-set-step-quiet
      "line-keep-item"
      (strcat
        "type="
        (pa-mep-cross-type-text item)
        " value="
        (pa-mep-cross-value-text item)
      )
    )
    (if (> (- (cadr item) (car item)) 1.0)
      (progn
        (setq pts (pa-mep-cross-line-interval-points obj (car item) (cadr item)))
        (pa-mep-cross-set-step-quiet
          "line-pts"
          (strcat
            "count="
            (if (pa-mep-cross-list-p pts) (itoa (length pts)) "<not-list>")
            " pts="
            (pa-mep-cross-value-text pts)
          )
        )
        (if (pa-mep-cross-make-line (car pts) (cadr pts) ed)
          (setq count (1+ count))
        )
      )
    )
    (setq keeps (cdr keeps))
  )

  (entdel e)
  (pa-mep-cross-set-step-quiet "line-entdel" (strcat "count=" (itoa count)))
  count
)

(defun pa-mep-cross-apply-cuts (e dists gap / ed typ obj total cutRanges keepRanges item pts count made handle)
  (pa-mep-cross-set-apply-step
    "apply-start"
    (strcat
      "handle="
      (pa-mep-cross-handle e)
      " dists="
      (pa-mep-cross-value-text dists)
    )
  )
  (setq handle (pa-mep-cross-handle e))
  (setq ed (entget e))
  (setq typ (cdr (assoc 0 ed)))
  (if (equal typ "LINE")
    (progn
      (setq count (pa-mep-cross-apply-line-cuts e dists gap))
      (pa-mep-cross-set-apply-step
        "apply-line-done"
        (strcat
          "handle="
          handle
          " count="
          (itoa count)
        )
      )
      count
    )
    (progn
  (setq obj (vlax-ename->vla-object e))
  (setq total (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
  (pa-mep-cross-set-apply-step
    "apply-total"
    (strcat
      "handle="
      (pa-mep-cross-handle e)
      " typ="
      typ
      " total="
      (rtos total 2 3)
      " gap="
      (rtos gap 2 3)
    )
  )
  (setq cutRanges (pa-mep-cross-merge-cut-ranges dists gap total))
  (pa-mep-cross-set-apply-step "cut-ranges" (pa-mep-cross-value-text cutRanges))
  (setq keepRanges (pa-mep-cross-keep-ranges cutRanges total))
  (pa-mep-cross-set-apply-step "keep-ranges" (pa-mep-cross-value-text keepRanges))
  (setq count 0)

  (foreach item keepRanges
    (pa-mep-cross-set-apply-step
      "keep-item"
      (strcat
        "type="
        (pa-mep-cross-type-text item)
        " value="
        (pa-mep-cross-value-text item)
      )
    )
    (if (> (- (cadr item) (car item)) 1.0)
      (progn
        (setq pts
          (if (equal typ "LINE")
            (pa-mep-cross-line-interval-points obj (car item) (cadr item))
            (pa-mep-cross-interval-points obj (car item) (cadr item))
          )
        )
        (pa-mep-cross-set-apply-step
          "make-part"
          (strcat
            "typ="
            typ
            " pts-type="
            (pa-mep-cross-type-text pts)
            " pts-count="
            (if (pa-mep-cross-list-p pts) (itoa (length pts)) "<not-list>")
            " pts="
            (pa-mep-cross-value-text pts)
          )
        )
        (setq made (vl-catch-all-apply 'pa-mep-cross-make-curve-part (list typ pts ed)))
        (if (vl-catch-all-error-p made)
          (progn
            (pa-mep-cross-set-apply-step
              "make-part-error"
              (vl-catch-all-error-message made)
            )
            (setq made nil)
          )
        )
        (if made
          (setq count (1+ count))
        )
      )
    )
  )

  (pa-mep-cross-set-apply-step "entdel" (strcat "handle=" (pa-mep-cross-handle e)))
  (entdel e)
  count
    )
  )
)

(defun c:MEPCROSSCUT (/ *error* olderr defaultGap gap ss i j e1 e2 points p target dist cuts item cutEntityCount newEntityCount pairCount pointCount)
  (setq olderr *error*)

  (defun *error* (msg)
    (setq *error* olderr)
    (if (and msg (/= msg "Function cancelled"))
      (progn
        (princ (strcat "\n[MEPCROSSCUT] ERROR: " msg))
        (princ (strcat "\n[MEPCROSSCUT DBG] last-step: " pa-mep-cross-last-step))
        (princ (strcat "\n[MEPCROSSCUT DBG] last-info: " pa-mep-cross-last-info))
      )
    )
    (princ)
  )

  (vl-load-com)
  (setq defaultGap pa-mep-cross-default-gap)

  (setq gap defaultGap)
  (pa-mep-cross-set-step "start" (strcat "gap=" (rtos gap 2 3)))

  (princ "\nMEP_給水/排水/雨水/ガス/給湯 の LINE または LWPOLYLINE を選択: ")
  (setq ss
    (ssget
      '(
        (-4 . "<AND")
          (0 . "LINE,LWPOLYLINE")
          (8 . "MEP_給水,MEP_排水,MEP_雨水,MEP_ガス,MEP_給湯")
        (-4 . "AND>")
      )
    )
  )

  (setq cuts '())
  (setq cutEntityCount 0)
  (setq newEntityCount 0)
  (setq pairCount 0)
  (setq pointCount 0)

  (if ss
    (progn
      (pa-mep-cross-set-step "selected" (strcat "count=" (itoa (sslength ss))))
      (setq i 0)
      (while (< i (sslength ss))
        (setq e1 (ssname ss i))
        (setq j (1+ i))
        (while (< j (sslength ss))
          (setq e2 (ssname ss j))
          (setq pairCount (1+ pairCount))
          (if (= (rem pairCount 500) 0)
            (pa-mep-cross-set-step
              "scan"
              (strcat
                "pairs="
                (itoa pairCount)
                " i="
                (itoa i)
                " j="
                (itoa j)
              )
            )
          )
          (if (and (pa-mep-cross-target-entity-p e1) (pa-mep-cross-target-entity-p e2))
            (progn
              (pa-mep-cross-set-step-quiet
                "pair"
                (strcat
                  "i="
                  (itoa i)
                  " j="
                  (itoa j)
                  " h1="
                  (pa-mep-cross-handle e1)
                  " h2="
                  (pa-mep-cross-handle e2)
                )
              )
              (setq points (pa-mep-cross-intersections e1 e2))
              (if points
                (pa-mep-cross-set-step "points" (strcat "count=" (itoa (length points))))
              )
              (foreach p points
                (if (pa-mep-cross-point-p p)
                  (if (not (pa-mep-cross-endpoint-contact-p e1 e2 p))
                    (progn
                      (setq pointCount (1+ pointCount))
                      (pa-mep-cross-set-value-step "point" p)
                      (setq target (pa-mep-cross-cut-target e1 e2 p))
                      (pa-mep-cross-set-step "target" (strcat "handle=" (pa-mep-cross-handle target)))
                      (setq dist (pa-mep-cross-dist-at-point target p))
                      (pa-mep-cross-set-step
                        "dist"
                        (strcat
                          "type="
                          (pa-mep-cross-type-text dist)
                          " value="
                          (pa-mep-cross-value-text dist)
                        )
                      )
                      (if dist
                        (setq cuts (pa-mep-cross-put-cut target dist cuts))
                      )
                    )
                    (pa-mep-cross-set-value-step "skip-endpoint-contact" p)
                  )
                )
              )
            )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )

      (foreach item cuts
        (pa-mep-cross-set-step
          "apply"
          (strcat
            "handle="
            (pa-mep-cross-handle (cadr item))
            " cuts="
            (itoa (length (caddr item)))
          )
        )
        (setq newEntityCount
          (+ newEntityCount (pa-mep-cross-apply-cuts (cadr item) (caddr item) gap))
        )
        (setq cutEntityCount (1+ cutEntityCount))
      )

      (princ
        (strcat
          "\nMEPCROSSCUT cut entities: "
          (itoa cutEntityCount)
          " new entities: "
          (itoa newEntityCount)
          " pairs: "
          (itoa pairCount)
          " points: "
          (itoa pointCount)
        )
      )
    )
    (princ "\n対象の LINE/LWPOLYLINE が選択されませんでした。")
  )

  (setq *error* olderr)
  (princ)
)

(princ)
