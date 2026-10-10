;; Load after 09_detail_cut.lsp; run DETAILCUTTESTSOLID.
;; Pure polygon arithmetic: creates/changes no drawing entities.
(defun dc-test:area (points / prev p sum)
  (setq sum 0.0)
  (if points
    (progn
      (setq prev (last points))
      (foreach p points
        (setq sum (+ sum (- (* (car prev) (cadr p)) (* (car p) (cadr prev)))) prev p))))
  (/ (abs sum) 2.0))
(defun c:DETAILCUTTESTSOLID (/ dc:x0 dc:x1 dc:y0 dc:y1 cases entry actual failures)
  (setq dc:x0 0.0 dc:x1 10.0 dc:y0 0.0 dc:y1 10.0 failures 0
        cases '( ("inside" ((1.0 1.0 0.0) (3.0 1.0 0.0) (1.0 3.0 0.0)) 2.0)
                 ("outside" ((-3.0 1.0 0.0) (-1.0 1.0 0.0) (-1.0 3.0 0.0)) 0.0)
                 ("crossing" ((-5.0 0.0 0.0) (5.0 0.0 0.0) (5.0 10.0 0.0)) 37.5)
                 ("enclosing" ((-20.0 -20.0 0.0) (40.0 -20.0 0.0) (-20.0 40.0 0.0)) 100.0)
                 ("boundary" ((0.0 0.0 0.0) (10.0 0.0 0.0) (0.0 10.0 0.0)) 50.0)
                 ("touch only" ((-2.0 0.0 0.0) (0.0 0.0 0.0) (0.0 -2.0 0.0)) 0.0)))
  (foreach entry cases
    (setq actual (dc-test:area (dc:polygon-clip (cadr entry))))
    (if (equal actual (caddr entry) 1e-8)
      (princ (strcat "\nPASS: " (car entry)))
      (progn (setq failures (1+ failures))
             (princ (strcat "\nFAIL: " (car entry) " area=" (rtos actual 2 10))))))
  (princ (strcat "\nSOLID arithmetic tests: " (itoa failures) " failures.")) (princ))
(princ)

;; Integration check: creates temporary POINTs, processes them, then cleans up.
;; Run in a scratch drawing with an unlocked current layer; respects current UCS.
(defun c:DETAILCUTTESTPOINT (/ *error* dc:made dc:issues dc:step dc:entity-info
                             dc:x0 dc:x1 dc:y0 dc:y1 dc:eps entry e o result failures)
  (defun *error* (msg) (dc:cleanup) (princ msg) (princ))
  (setq dc:x0 0.0 dc:x1 10.0 dc:y0 0.0 dc:y1 10.0 dc:eps 1e-7 failures 0)
  (foreach entry '(("inside" (5.0 5.0 0.0) T)
                   ("outside" (11.0 5.0 0.0) nil)
                   ("edge" (0.0 5.0 0.0) T)
                   ("corner" (10.0 10.0 0.0) T)
                   ("elevated" (5.0 5.0 7.0) T))
    (setq e (entmakex (list '(0 . "POINT") (cons 10 (trans (cadr entry) 1 0)))))
    (if (null e) (exit))
    (setq o (dc:track (vlax-ename->vla-object e)) result (dc:process o 0))
    (if (and (null dc:issues)
             (if (caddr entry)
               (and (= (length result) 1) (entget e)
                    (equal (cdr (assoc 10 (entget e))) (trans (cadr entry) 1 0) 1e-8))
               (and (null result) (null (entget e)))))
      (princ (strcat "\nPASS: " (car entry)))
      (progn (setq failures (1+ failures)) (princ (strcat "\nFAIL: " (car entry))))))
  (dc:cleanup)
  (princ (strcat "\nPOINT processing tests: " (itoa failures) " failures.")) (princ))

(defun c:DETAILCUTTESTELLIPSE (/ cases entry roots param residual failures)
  (setq failures 0
        cases (list (list 5.0 0.0 0.0 2)
                    (list 5.0 0.0 3.0 2)
                    (list 3.0 4.0 2.0 2)
                    (list 5.0 0.0 6.0 0)
                    (list 0.0 5.0 5.0 1)))
  (foreach entry cases
    (setq roots (dc:unique (dc:ellipse-roots (car entry) (cadr entry) (caddr entry) 0.0 (* 2.0 pi))))
    (if (/= (length roots) (cadddr entry)) (setq failures (1+ failures)))
    (foreach param roots
      (setq residual (- (+ (* (car entry) (cos param)) (* (cadr entry) (sin param))) (caddr entry)))
      (if (> (abs residual) 1e-8) (setq failures (1+ failures)))))
  (princ (strcat "\nELLIPSE boundary-root tests: " (itoa failures) " failures.")) (princ))
