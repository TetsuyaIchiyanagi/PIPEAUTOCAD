(defun pa-get-dirs (ss p / dirs i e p1 p2 tol)
  (setq dirs '())
  (setq tol 10.0) ;; ← 少し広めに

  (setq i 0)
  (while (< i (sslength ss))
    (setq e (ssname ss i))
    (setq p1 (cdr (assoc 10 (entget e))))
    (setq p2 (cdr (assoc 11 (entget e))))

    ;; p1が交点に近い
    (if (< (distance p p1) tol)
      (setq dirs (cons (pa-dir p p2) dirs))
    )

    ;; p2が交点に近い
    (if (< (distance p p2) tol)
      (setq dirs (cons (pa-dir p p1) dirs))
    )

    (setq i (1+ i))
  )

  dirs
)