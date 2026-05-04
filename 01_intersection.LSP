(defun get-intersections (ss / pts i j e1 e2 o1 o2 ip p tol)

  (vl-load-com)
  (setq tol 1.0)
  (setq pts '())
  (setq i 0)

  (while (< i (sslength ss))
    (setq e1 (ssname ss i))
    (setq o1 (vlax-ename->vla-object e1))

    (setq j (+ i 1))
    (while (< j (sslength ss))
      (setq e2 (ssname ss j))
      (setq o2 (vlax-ename->vla-object e2))

      (setq ip (vlax-invoke o1 'IntersectWith o2 acExtendNone))

      (if ip
        (while ip
          (setq p (list (car ip) (cadr ip) (caddr ip)))
          (setq pts (cons p pts))
          (setq ip (cdddr ip))
        )
      )

      (setq j (1+ j))
    )

    (setq i (1+ i))
  )

  pts
)