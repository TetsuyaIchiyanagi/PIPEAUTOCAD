(setq pa-path "C:/Users/tetsu/OneDrive - 山門設備設計/Project_以外/02_CAD/02_Auto_Cad/08_CADツール/PIPEAUTOCAD")

(load (strcat pa-path "01_intersection.lsp"))
(load (strcat pa-path "03_place_blocks.lsp"))
(load (strcat pa-path "02_judge.lsp"))

(princ "\nPipeAutoCAD loaded.")
(princ "\nCommand: JUDGEPOINT")
(princ)