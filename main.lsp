;; これがメイン

(setq pa-path "C:/LISP/PipeAutoCAD/")

(load (strcat pa-path "01_intersection.lsp"))
(load (strcat pa-path "02_node_classify.lsp"))
(load (strcat pa-path "03_direction.lsp"))
(load (strcat pa-path "04_place_blocks.lsp"))

(princ "\nPipeAutoCAD loaded.")
(princ "\nCommand: JUDGEPOINT")
(princ)