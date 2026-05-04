(vl-load-com)

(setq pa-path "C:/LISP/PipeAutoCAD/")

(load (strcat pa-path "01_intersection.lsp"))
(load (strcat pa-path "03_place_blocks.lsp"))
(load (strcat pa-path "02_judge.lsp"))

(princ "\nPipeAutoCAD loaded.")
(princ "\nCommand: JUDGEPOINT")
(princ)