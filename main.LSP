(vl-load-com)

(setq pa-path "C:/LISP/PipeAutoCAD/")

(load (strcat pa-path "01_intersection.lsp"))
(load (strcat pa-path "02_node_classify.lsp"))
(load (strcat pa-path "03_direction.lsp"))
(load (strcat pa-path "04_place_blocks.lsp"))
(load (strcat pa-path "05_pipe_size_text.lsp"))
(load (strcat pa-path "06_double_line.lsp"))
(load (strcat pa-path "07_duct_double_line.lsp"))

(princ "\nPipeAutoCAD loaded.")
(princ "\nCommand: PIPESIZEAUTO")
(princ "\nCommand: INSERTPIPEPARTS")
(princ "\nCommand: CHECKPIPESEG")
(princ "\nCommand: CHECKNODESIZE")
(princ "\nCommand: DUCTDOUBLE")
