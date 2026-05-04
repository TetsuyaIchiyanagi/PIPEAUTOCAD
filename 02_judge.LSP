(defun c:JUDGEPOINT (/ ss pts item p typ)

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq pts (pa-get-node-types ss))

      (foreach item pts
        (setq p (car item))
        (setq typ (cadr item))

        (cond
          ((= typ "BEND") (pa-draw-circle p))
          ((= typ "JOIN") (pa-draw-triangle p))
        )
      )

      (princ (strcat "\nçÏê}êî: " (itoa (length pts))))
    )
    (princ "\nLINEÇëIëÇµÇƒÇ≠ÇæÇ≥Ç¢ÅB")
  )

  (princ)
)

(defun pa-get-node-types (ss / result i j e1 e2 p1 p2 p3 p4 o1 o2 ip p typ tol)
  (setq result '())
  (setq tol 10.0)
  (setq i 0)

  (while (< i (sslength ss))
    (setq e1 (ssname ss i))
    (setq o1 (vlax-ename->vla-object e1))
    (setq p1 (cdr (assoc 10 (entget e1))))
    (setq p2 (cdr (assoc 11 (entget e1))))

    (setq j (+ i 1))
    (while (< j (sslength ss))
      (setq e2 (ssname ss j))
      (setq o2 (vlax-ename->vla-object e2))
      (setq p3 (cdr (assoc 10 (entget e2))))
      (setq p4 (cdr (assoc 11 (entget e2))))

      (setq ip (vlax-invoke o1 'IntersectWith o2 acExtendNone))

      (if ip
        (while ip
          (setq p (list (car ip) (cadr ip) (caddr ip)))

          (setq typ
            (pa-classify-by-endpoint
              p
              p1 p2
              p3 p4
              tol
            )
          )

          (setq result (pa-add-node p typ result tol))

          (setq ip (cdddr ip))
        )
      )

      (setq j (1+ j))
    )

    (setq i (1+ i))
  )

  result
)

(defun pa-classify-by-endpoint (p a1 a2 b1 b2 tol / aEnd bEnd)

  (setq aEnd
    (or
      (< (distance p a1) tol)
      (< (distance p a2) tol)
    )
  )

  (setq bEnd
    (or
      (< (distance p b1) tol)
      (< (distance p b2) tol)
    )
  )

  (cond
    ;; óºï˚ÇÃê¸ÇÃí[ì_ìØémÇ»ÇÁã»ì_
    ((and aEnd bEnd)
      "BEND"
    )

    ;; ï–ï˚Ç‹ÇΩÇÕóºï˚Ç™ê¸ÇÃìríÜÇ»ÇÁçáó¨ì_
    (T
      "JOIN"
    )
  )
)

(defun pa-add-node (p typ result tol / found item oldTyp newTyp)
  (setq found nil)
  (setq newResult '())

  (foreach item result
    (if (< (distance p (car item)) tol)
      (progn
        (setq found T)
        (setq oldTyp (cadr item))

        ;; JOINóDêÊ
        (setq newTyp
          (if (or (= oldTyp "JOIN") (= typ "JOIN"))
            "JOIN"
            typ
          )
        )

        (setq newResult (cons (list (car item) newTyp) newResult))
      )
      (setq newResult (cons item newResult))
    )
  )

  (if found
    newResult
    (cons (list p typ) result)
  )
)