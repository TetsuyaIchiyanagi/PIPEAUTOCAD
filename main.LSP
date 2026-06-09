(vl-load-com)

(setq pa-cloud-project-relative-path
  "Project_以外/02_CAD/02_Auto_Cad/08_CADツール/PIPEAUTOCAD"
)

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

(defun pa:safe-findfile (path / result)
  (if (and path (/= path ""))
    (progn
      (setq result (vl-catch-all-apply 'findfile (list path)))
      (if (vl-catch-all-error-p result)
        nil
        result
      )
    )
  )
)

(defun pa:file-exists-p (path)
  (and path (pa:safe-findfile path))
)

(defun pa:safe-directory-files (dir pattern directories / result)
  (setq result (vl-catch-all-apply 'vl-directory-files (list dir pattern directories)))
  (if (vl-catch-all-error-p result)
    nil
    result
  )
)

(defun pa:project-dir-p (path / dir)
  (setq dir (pa:add-trailing-slash path))
  (and
    dir
    (or
      (pa:file-exists-p (strcat dir "01_intersection.LSP"))
      (pa:file-exists-p (strcat dir "01_intersection.lsp"))
    )
    (or
      (pa:file-exists-p (strcat dir "07_duct_double_line.LSP"))
      (pa:file-exists-p (strcat dir "07_duct_double_line.lsp"))
    )
  )
)

(defun pa:add-root (roots path / dir)
  (setq dir (pa:add-trailing-slash path))
  (if (and dir (not (member dir roots)))
    (append roots (list dir))
    roots
  )
)

(defun pa:add-env-root (roots name / value)
  (setq value (getenv name))
  (if (and value (/= value ""))
    (pa:add-root roots value)
    roots
  )
)

(defun pa:add-user-onedrive-roots (roots userprofile / names name)
  (if (and userprofile (/= userprofile ""))
    (progn
      (setq names (pa:safe-directory-files userprofile "OneDrive*" -1))
      (while names
        (setq name (car names))
        (if (and (/= name ".") (/= name ".."))
          (setq roots (pa:add-root roots (strcat userprofile "/" name)))
        )
        (setq names (cdr names))
      )
    )
  )
  roots
)

(defun pa:add-project-candidates (paths root / dir)
  (setq dir (pa:add-trailing-slash root))
  (if dir
    (progn
      (setq paths (pa:add-root paths (strcat dir pa-cloud-project-relative-path)))
    )
  )
  paths
)

(defun pa:find-project-path (/ roots paths userprofile found)
  (setq roots nil)
  (setq roots (pa:add-env-root roots "PIPEAUTOCAD_PATH"))
  (setq roots (pa:add-env-root roots "OneDriveCommercial"))
  (setq roots (pa:add-env-root roots "OneDrive"))
  (setq roots (pa:add-env-root roots "OneDriveConsumer"))

  (setq userprofile (getenv "USERPROFILE"))
  (if (and userprofile (/= userprofile ""))
    (progn
      (setq roots (pa:add-root roots (strcat userprofile "/OneDrive")))
      (setq roots (pa:add-user-onedrive-roots roots userprofile))
    )
  )

  (while roots
    (setq paths (pa:add-project-candidates paths (car roots)))
    (setq roots (cdr roots))
  )

  (while (and paths (not found))
    (if (pa:project-dir-p (car paths))
      (setq found (car paths))
    )
    (setq paths (cdr paths))
  )
  found
)

(setq pa-main-file
  (cond
    ((and (boundp '*load-truename*) *load-truename*) *load-truename*)
    ((and (boundp '*load-pathname*) *load-pathname*) *load-pathname*)
    ((pa:safe-findfile "01_intersection.lsp"))
  )
)

(setq pa-path
  (cond
    ((pa:find-project-path))
    ((and pa-main-file (pa:project-dir-p (vl-filename-directory pa-main-file)))
      (pa:add-trailing-slash (vl-filename-directory pa-main-file))
    )
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
    (princ (strcat "\nPipeAutoCAD path: " pa-path))
    (princ "\nCommand: PIPESIZEAUTO")
    (princ "\nCommand: INSERTPIPEPARTS")
    (princ "\nCommand: CHECKPIPESEG")
    (princ "\nCommand: CHECKNODESIZE")
    (princ "\nCommand: DUCTDOUBLE")
  )
  (progn
    (princ "\nPipeAutoCAD error: PipeAutoCAD folder was not found.")
    (if pa-main-file
      (princ (strcat "\nLoaded main.LSP from: " pa-main-file))
    )
    (princ "\nCopy the whole PipeAutoCAD folder, not only main.LSP.")
    (princ "\nSet PIPEAUTOCAD_PATH or sync the shared folder under OneDrive.")
  )
)
