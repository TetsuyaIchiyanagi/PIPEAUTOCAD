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

(defun c:DOUBLEPIPE ()
  (princ "\nDOUBLEPIPE: not implemented yet.")
  (princ)
)
