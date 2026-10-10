;; 先に最新版10_kikiset.lspをAPPLOADし、このファイルをロードする。
;; KIKISETDIAGは実際の読込み経路を通すが、属性更新前に終了する。
;; 自分専用のExcelだけを使用。既存Excelの終了や図面保存は行わない。
(defun c:KIKISETDIAG (/ ks:diagnostic-log ks:diagnostic-readonly *ks:master-cache*)
  (if (not (member 'ks:trace (atoms-family 0)))
    (princ "\n先に最新版の10_kikiset.lspをAPPLOADしてください。")
    (progn
      ;; キャッシュを局所的に無効化し、通信・Excel読込みも毎回診断する。
      ;; 通常コマンドのキャッシュは診断終了後に元に戻る。
      (setq ks:diagnostic-log (vl-filename-mktemp "KIKISET-DIAG-" nil ".log")
            ks:diagnostic-readonly T
            *ks:last-diagnostic-log* ks:diagnostic-log)
      (princ (strcat "\nKIKISET 診断 D1（属性変更なし）\nログ：" ks:diagnostic-log))
      (ks:trace "DIAG D1 / FRESH MASTER / NO ATTRIBUTE WRITES")
      (c:KIKISET)
      (ks:trace "DIAG COMMAND RETURN")
      (princ (strcat "\nログ：" ks:diagnostic-log))))
  (princ))
(defun c:KIKISETDIAGLOG ()
  (if (and (boundp '*ks:last-diagnostic-log*) *ks:last-diagnostic-log*)
    (princ (strcat "\n直近の診断ログ：" *ks:last-diagnostic-log*))
    (princ "\nこの図面ではまだ診断を実行していません。"))
  (princ))
(princ "\nKIKISET診断 D1 loaded. Command: KIKISETDIAG / KIKISETDIAGLOG")
(princ)
