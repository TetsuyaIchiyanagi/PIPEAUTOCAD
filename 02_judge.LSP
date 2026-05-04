(defun c:JUDGEPOINT (/ ss pts p dirs typ)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq pts (get-intersections ss))

      (foreach p pts
        (setq dirs (get-dirs ss p))
        (setq typ (classify-node dirs))

        (cond
          ((= typ "BEND") (draw-circle p))
          ((= typ "JOIN") (draw-triangle p))
        )
      )
    )
  )

  (princ)
)

(defun get-dirs (ss p / dirs i e p1 p2)
  (setq dirs '())
  (setq i 0)

  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq p1 (cdr (assoc 10 (entget e))))
    (setq p2 (cdr (assoc 11 (entget e))))

    (if (< (distance p p1) 1.0)
      (setq dirs (cons (dir p p2) dirs))
    )

    (if (< (distance p p2) 1.0)
      (setq dirs (cons (dir p p1) dirs))
    )

    (setq i (1+ i))
  )

  dirs
)

(defun dir (p1 p2 / dx dy)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))

  (cond
    ((> dx 0) "R")
    ((< dx 0) "L")
    ((> dy 0) "U")
    ((< dy 0) "D")
  )
)

(defun classify-node (dirs / n)
  (setq n (length dirs))

  (cond
    ((>= n 3) "JOIN")
    ((= n 2)
      (if (not (or (and (member "L" dirs) (member "R" dirs))
                   (and (member "U" dirs) (member "D" dirs))))
        "BEND"
      )
    )
  )
)