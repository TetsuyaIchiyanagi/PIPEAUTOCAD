(defun c:JUDGEPOINT (/ ss pts p dirs typ)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq pts (pa-get-intersections ss))

      (foreach p pts
        (setq dirs (pa-get-dirs ss p))
        (setq typ (pa-classify dirs))

        (cond
          ((= typ "BEND") (pa-draw-circle p))
          ((= typ "JOIN") (pa-draw-triangle p))
        )
      )
    )
    (princ "\nLINEを選択してください。")
  )

  (princ)
)

(defun pa-get-dirs (ss p / dirs i e p1 p2 tol)
  (setq dirs '())
  (setq tol 1.0)
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq p1 (cdr (assoc 10 (entget e))))
    (setq p2 (cdr (assoc 11 (entget e))))

    ;; pが線上にあるかチェック
    (if (pa-point-on-seg p p1 p2 tol)
      (progn
        (setq dirs (cons (pa-dir p p1) dirs))
        (setq dirs (cons (pa-dir p p2) dirs))
      )
    )

    (setq i (1+ i))
  )

  dirs
)

(defun pa-dir (p1 p2 / dx dy)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))

  (cond
    ((> dx 1e-6) "R")
    ((< dx -1e-6) "L")
    ((> dy 1e-6) "U")
    ((< dy -1e-6) "D")
    (T nil)
  )
)

(defun pa-classify (dirs / u)
  (setq u (pa-unique-list (vl-remove-if 'null dirs)))

  (cond
    ((>= (length u) 3) "JOIN")
    ((and (= (length u) 2)
          (not (or (and (member "L" u) (member "R" u))
                   (and (member "U" u) (member "D" u)))))
     "BEND")
    (T nil)
  )
)

(defun pa-unique-list (lst / result x)
  (setq result '())

  (foreach x lst
    (if (not (member x result))
      (setq result (cons x result))
    )
  )

  result
)(defun pa-point-on-seg (p a b tol / ab ap ratio q ab2)
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
              (mapcar
                '(lambda (x) (* x ratio))
                ab
              )
            )
          )
          (< (distance p q) tol)
        )
        nil
      )
    )
  )
)