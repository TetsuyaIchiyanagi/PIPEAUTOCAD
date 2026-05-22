(vl-load-com)

(defun pa:add-trailing-slash (path / normalized)
  (if (and path (/= path ""))
    (progn
      (setq normalized (vl-string-translate "\\" "/" path))
      (if (= (substr normalized (strlen normalized) 1) "/")
        normalized
        (strcat normalized "/")
      )
    )
  )
)

(setq pa-main-file
  (cond
    ((and (boundp '*load-truename*) *load-truename*) *load-truename*)
    ((and (boundp '*load-pathname*) *load-pathname*) *load-pathname*)
    ((findfile "main.LSP"))
  )
)

(setq pa-path
  (if pa-main-file
    (pa:add-trailing-slash (vl-filename-directory pa-main-file))
  )
)

(if pa-path
  (progn
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
  )
  (princ "\nPipeAutoCAD error: main.LSP path was not found.")
)
