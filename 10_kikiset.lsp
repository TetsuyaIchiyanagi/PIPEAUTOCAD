;; KIKISET - standalone Windows AutoCAD Visual LISP / Excel COM.
;; UTF-8: use Unicode AutoLISP (LISPSYS=1 or 2). See KIKISET.md.
(vl-load-com)
;; 診断時のみ、各操作の直前・直後を閉じたログファイルへ記録する。
;; COM呼出しが応答しない場合も、最後に開始した処理がディスクに残る。
(defun ks:trace (message / f result)
  (if (and (boundp 'ks:diagnostic-log) ks:diagnostic-log)
    (progn
      (princ (strcat "\nKIKISET診断：" message))
      (setq result (vl-catch-all-apply 'open (list ks:diagnostic-log "a")))
      (if (and (not (vl-catch-all-error-p result)) result)
        (progn (setq f result)
          (write-line (strcat (itoa (getvar "MILLISECS")) " : " message) f)
          (close f)))))
  (princ))
;; 全員共通の参照先。ユーザー名やOneDrive同期フォルダには依存しない。
(setq ks:master-source "https://sanmon.sharepoint.com/:x:/s/sanmon2/IQDPsk_iCte0Q44BOPlTXgG1Act5tUXbBxZYr7ZCH40Y30A?e=jsSooy")
(setq ks:remote-cache-minutes 5)
;; マスターの登録日。Excel日付または YYYY-MM-DD / YYYY/MM/DD を使用する。
(setq ks:registered-date-key "REGISTERED_DATE")
(defun ks:remote-p (path) (and (= (type path) 'STR) (wcmatch (strcase path) "HTTPS://*")))

;; Extend profiles here when adding ventilation equipment.
;; 実マスターに合わせ、参照先はユーザー指定の既存シート名を維持する。
(setq ks:profiles '(("KUCHO" "KUCHO_KIKI" "KUCHO_KIKI_MASTER" "KIKI_MASTER")
                    ("KANKI" "KANKI_KIKI" "KANKI_MASTER" nil)))
(setq ks:vent-steps '(("MAKER" . "メーカー") ("EQUIP_TYPE" . "機器種別")
  ("DUCT_DIA" . "ダクト径") ("AIRFLOW" . "風量")))
(setq ks:vent-tags '("EQUIP_TYPE" "MAKER" "MODEL" "TYPE" "MOUNT_TYPE"
  "DUCT_DIA" "AIRFLOW" "STATIC_PRESSURE" "PHASE" "VOLT" "POWER" "NOISE"
  "TEMP_EX_EFF" "ENTHALPY_HEAT_EFF" "ENTHALPY_COOL_EFF" "ACCESSORY"))
(defun ks:category (/ answer)
  ;; 日本語キーワードの戻り値に依存せず、番号で明示的に判定する。
  (while (not (member answer '(1 2)))
    (initget 6)
    (setq answer (getint "\n設備カテゴリ：1=空調 / 2=換気 <1>："))
    (if (null answer) (setq answer 1))
    (if (not (member answer '(1 2))) (princ "\n1（空調）または2（換気）を入力してください。")))
  (princ (if (= answer 2) "\n選択カテゴリ：換気（KANKI_KIKI）" "\n選択カテゴリ：空調（KUCHO_KIKI）"))
  (if (= answer 2) "KANKI" "KUCHO"))
(setq ks:protected '("SYSTEM" "NUM" "LOCATION" "FLOOR" "KIKI_NO"))
(setq ks:selection-steps
  '(("MAKER" . "メーカー") ("UNIT_TYPE" . "室内機タイプ")
    ("SYSTEM_CONFIG" . "システム構成") ("CAPACITY_CLASS" . "能力クラス")))
(setq ks:page-size 15)
(setq ks:aircon-steps ks:selection-steps)
;; 再ロード時も古い形式のキャッシュを残さない。COMオブジェクトは保持しない。
(setq *ks:master-cache* nil)

(defun ks:cache-key (path sheet / resolved stamp size)
  (if (ks:remote-p path)
    (list path sheet 'REMOTE)
    (if (and (setq resolved (findfile path))
           (setq stamp (vl-file-systime resolved))
           (setq size (vl-file-size resolved)))
    (list (strcase (vl-string-translate "/" "\\" resolved)) sheet stamp size))))
(defun c:KIKISETRELOAD ()
  (setq *ks:master-cache* nil)
  (princ "\n機器マスターのキャッシュを解除しました。次回KIKISETで再読込みします。") (princ))
(defun ks:load-master (path sheet / key records started)
  (setq key (ks:cache-key path sheet))
  (if (and key *ks:master-cache* (equal key (car *ks:master-cache*))
           (or (not (ks:remote-p path))
               (and (numberp (caddr *ks:master-cache*))
                    (<= 0 (- (getvar "DATE") (caddr *ks:master-cache*)))
                    (< (- (getvar "DATE") (caddr *ks:master-cache*)) (/ ks:remote-cache-minutes 1440.0)))))
    (progn (princ "\n読込済みの機器マスターを使用します。")
           (cadr *ks:master-cache*))
    (progn
      (setq *ks:master-cache* nil started (getvar "MILLISECS"))
      (princ "\n機器マスターを読込み中です。")
      (setq records (ks:read-master path sheet))
      ;; 読込み中にファイルが更新された場合は次回必ず読み直す。
      (if (and key (equal key (ks:cache-key path sheet)))
        (setq *ks:master-cache* (list key records (getvar "DATE"))))
      (princ (strcat "\nマスター読込み：" (rtos (/ (- (getvar "MILLISECS") started) 1000.0) 2 2) "秒"))
      records)))

(defun ks:trim (s) (vl-string-trim " \t\r\n" s))
(defun ks:tag (s) (strcase (ks:trim s)))
(defun ks:writable-p (tag value)
  (and (not (member (ks:tag tag) ks:protected))
       value (/= (ks:trim value) "")))
(defun ks:fail (s) (setq ks:reason s) (exit))
(defun ks:track (o)
  ;; Excel Item may return VT_DISPATCH (variant type 9), not a bare object.
  ;; Normalize BEFORE both use and ownership registration for cleanup.
  (while (= (type o) 'VARIANT) (setq o (vlax-variant-value o)))
  (if (/= (type o) 'VLA-OBJECT)
    (ks:fail "ExcelからCOMオブジェクトを取得できませんでした。"))
  (setq ks:objects (cons o ks:objects))
  o)
(defun ks:release (o)
  (if (and o (= (type o) 'VLA-OBJECT))
    (vl-catch-all-apply 'vlax-release-object (list o))))

(defun ks:close-excel (/ o r)
  (ks:trace "CLEANUP BEGIN")
  ;; Only objects created by this command are owned. Never GetObject.
  (if ks:book
    (progn
      (setq r (vl-catch-all-apply 'vlax-invoke-method (list ks:book 'Close :vlax-false)))
      (if (vl-catch-all-error-p r) (princ "\n警告：Excelブックの終了処理に失敗しました。"))))
  (foreach o ks:objects (ks:release o))
  (setq ks:objects nil ks:book nil)
  (if ks:excel
    (progn
      (setq r (vl-catch-all-apply 'vlax-invoke-method (list ks:excel 'Quit)))
      (if (vl-catch-all-error-p r) (princ "\n警告：専用Excelの終了処理に失敗しました。"))
      (ks:release ks:excel)))
  ;; COMは上で明示解放済み。ここで強制GCするとExcel終了後に長時間待つ
  ;; 環境があるため、通常のAutoLISPメモリ回収に任せる。
  (setq ks:excel nil)
  (if (and (boundp 'ks:download-file) ks:download-file)
    (progn
      (vl-catch-all-apply 'vl-file-delete (list ks:download-file))
      (setq ks:download-file nil)))
  (ks:trace "CLEANUP END")
  (princ))

(defun ks:value-string (value)
  ;; 表示書式(Text)ではなくValue2を変換する。単位・桁補正は加えない。
  (if (and (= (type value) 'VARIANT) (= (vlax-variant-type value) 10))
    (ks:fail (strcat "Excelセルにエラー値があります。" (if ks:stage ks:stage ""))))
  (while (= (type value) 'VARIANT) (setq value (vlax-variant-value value)))
  (cond ((null value) "")
        ((= (type value) 'STR) value)
        ((numberp value)
          (vlax-variant-value (vlax-variant-change-type (vlax-make-variant value) vlax-vbString)))
        (T (vl-princ-to-string value))))

(defun ks:cell (cells row col / cell result)
  (setq cell (ks:track (vlax-get-property cells 'Item row col))
        result (ks:value-string (vlax-get-property cell 'Value2)))
  (ks:release cell)
  (setq ks:objects (cdr ks:objects))
  result)

(defun ks:block-category-label (name)
  (cond ((= (strcase name) "KUCHO_KIKI") "空調")
        ((= (strcase name) "KANKI_KIKI") "換気")
        (T name)))
(defun ks:select-block (name / pick entity info obj actual done message)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq pick (entsel (strcat "\n機器ブロック（" name "）を選択してください［Enter/Escで終了］：")))
    (ks:trace "ENTSEL RETURN / ENTGET BEGIN")
    (setq entity (if pick (car pick)) info (if entity (entget entity)))
    (ks:trace (strcat "ENTGET END / TYPE=" (if info (cdr (assoc 0 info)) "NONE")))
    ;; Attribute clicks can resolve to ATTRIB rather than the owning INSERT.
    (if (and info (= (cdr (assoc 0 info)) "ATTRIB"))
      (setq entity (cdr (assoc 330 info)) info (if entity (entget entity))))
    (cond
      ((and (null pick) (= (getvar "ERRNO") 7))
        (princ "\n機器を取得できませんでした。ブロックの線または属性を選択してください。"))
      ((null pick) (setq done T))
      ((or (null info) (/= (cdr (assoc 0 info)) "INSERT"))
        (princ "\n属性付きブロックを選択してください。"))
      (T
        (ks:trace (strcat "VLA OBJECT BEGIN / HANDLE=" (cdr (assoc 5 info))))
        (setq ks:stage "ブロック情報の取得"
              obj (vlax-ename->vla-object entity))
        (ks:trace "VLA OBJECT END / EFFECTIVE NAME BEGIN")
        (setq actual (if (vlax-property-available-p obj 'EffectiveName)
                       (vla-get-EffectiveName obj) (vla-get-Name obj)))
        (ks:trace (strcat "EFFECTIVE NAME END / " actual " / HAS ATTRIBUTES CHECK"))
        (cond
          ((/= (strcase actual) (strcase name))
            ;; 違うカテゴリを選んでもコマンドを終了せず、警告後に再選択する。
            (setq message (strcat "選択したブロックが設備カテゴリと一致しません。"
              "\n\n指定カテゴリ：" (ks:block-category-label name)
              "\n対象ブロック：" name
              "\n選択したブロック：" actual
              "\n\nOKを押して、対象ブロックを選び直してください。"
              "\nカテゴリを変更する場合は、Escで終了してKIKISETを再実行してください。"))
            (ks:release obj) (setq obj nil)
            (princ (strcat "\n警告：" message))
            (alert message))
          ((/= (vla-get-HasAttributes obj) :vlax-true)
            (princ "\n属性付きブロックを選択してください。")
            (ks:release obj) (setq obj nil))
          (T (setq done T))))))
  obj)

(defun ks:get-attributes (block / result)
  (ks:trace "GET ATTRIBUTES BEGIN")
  ;; Normalize the explicit ActiveX return value before foreach / cleanup.
  (setq result (vl-catch-all-apply 'vla-GetAttributes (list block)))
  (ks:trace "GET ATTRIBUTES RETURN / ARRAY CONVERSION BEGIN")
  (if (vl-catch-all-error-p result)
    (ks:fail (strcat "ブロック属性を取得できません：" (vl-catch-all-error-message result))))
  (if (= (type result) 'VARIANT) (setq result (vlax-variant-value result)))
  (if (= (type result) 'SAFEARRAY)
    (setq result (if (< (vlax-safearray-get-u-bound result 1) (vlax-safearray-get-l-bound result 1))
                   nil (vlax-safearray->list result))))
  (if (not (listp result)) (ks:fail "ブロック属性の取得結果が不正です。"))
  (ks:trace (strcat "GET ATTRIBUTES END / COUNT=" (itoa (length result))))
  result)

(defun ks:file () ks:master-source)
(defun ks:download-master (url / http stream result body bytes download-url)
  (ks:trace "DOWNLOAD BEGIN / WINHTTP CREATE")
  (princ "\nSharePointから機器マスターを取得しています…")
  (setq ks:stage "SharePoint接続設定")
  (setq download-url (strcat url (if (vl-string-search "?" url) "&" "?") "download=1")
        http (ks:track (vlax-create-object "WinHttp.WinHttpRequest.5.1")))
  (vlax-invoke-method http 'SetTimeouts 5000 10000 10000 20000)
  (vlax-invoke-method http 'Open "GET" download-url :vlax-false)
  (setq ks:stage "SharePoint受信")
  (ks:trace "HTTP SEND BEGIN")
  (setq result (vl-catch-all-apply 'vlax-invoke-method (list http 'Send "")))
  (ks:trace "HTTP SEND END")
  (if (vl-catch-all-error-p result)
    (ks:fail (strcat "SharePointへ接続できません。ネットワーク・共有リンクを確認してください。\n"
                     (vl-catch-all-error-message result))))
  (if (/= (vlax-get-property http 'Status) 200)
    (ks:fail (strcat "SharePointの取得に失敗しました。HTTP " (itoa (vlax-get-property http 'Status)))))
  (setq body (vlax-get-property http 'ResponseBody)
        bytes (vlax-safearray->list (vlax-variant-value body)))
  ;; サインイン画面等のHTMLをxlsxとしてExcelへ渡さない。
  (if (not (equal (list (car bytes) (cadr bytes) (caddr bytes) (cadddr bytes)) '(80 75 3 4)))
    (ks:fail "共有リンクからExcelファイルを取得できません。共有権限・リンクの有効期限を確認してください。"))
  (setq ks:download-file (vl-filename-mktemp "KIKISET-master-" nil ".xlsx")
        stream (ks:track (vlax-create-object "ADODB.Stream")))
  (setq ks:stage "SharePoint一時ファイル保存")
  (vlax-put-property stream 'Type 1)
  (vlax-invoke-method stream 'Open :vlax-missing 0 -1 "" "")
  (vlax-invoke-method stream 'Write body)
  (vlax-invoke-method stream 'SaveToFile ks:download-file 2)
  (vlax-invoke-method stream 'Close)
  (ks:release stream) (setq ks:objects (vl-remove stream ks:objects))
  (ks:release http) (setq ks:objects (vl-remove http ks:objects))
  ks:download-file)

;; -------- ヘッダー・レコード：列順に依存しない純粋なデータ処理 --------
(defun ks:header-map (values / headers col value tag)
  (setq col 0)
  (foreach value values
    (setq tag (ks:tag (ks:value-string value)))
    (if (/= tag "")
      (progn
        (if (assoc tag headers) (ks:fail (strcat "1行目のキーが重複しています：" tag)))
        (setq headers (cons (cons tag col) headers))))
    (setq col (1+ col)))
  (if (not (assoc "MODEL" headers)) (ks:fail "1行目にMODEL列が存在しません。"))
  (reverse headers))

(defun ks:make-record (row values headers / entry data value)
  (foreach entry headers
    (setq ks:stage (strcat "Excelデータ読込み / 行=" (itoa row) " / " (car entry)))
    (setq value (nth (cdr entry) values))
    (if (= (type value) 'VARIANT) (setq value (vlax-variant-value value)))
    ;; 1904日付システムの場合だけ、登録日のシリアル値を1900基準へ揃える。
    (if (and (= (car entry) ks:registered-date-key) (numberp value)
             (boundp 'ks:date1904) ks:date1904)
      (setq value (+ value 1462)))
    (setq data (cons (cons (car entry) (ks:value-string value)) data)))
  ;; car=Excel行番号、cdr=タグと元値。MODEL重複でも行を区別する。
  (cons row (reverse data)))
(defun ks:record-value (record key / pair)
  (if (setq pair (assoc key (cdr record))) (ks:trim (cdr pair)) ""))
(defun ks:make-records (matrix headers / row values records record modelcol)
  (setq row 0 modelcol (cdr (assoc "MODEL" headers)))
  (foreach values matrix
    (setq row (1+ row) ks:stage (strcat "Excelデータ読込み / 行=" (itoa row)))
    (if (and (>= row 3) (/= (ks:trim (ks:value-string (nth modelcol values))) ""))
      (setq records (cons (ks:make-record row values headers) records))))
  (if (null records) (ks:fail "3行目以降にMODELが入力された機器データがありません。"))
  (reverse records))
(defun ks:unique-candidates (records key / record value candidates)
  (foreach record records
    (setq value (ks:record-value record key))
    (if (not (member value candidates)) (setq candidates (cons value candidates))))
  (reverse candidates))
(defun ks:filter-records (records key value)
  (vl-remove-if-not '(lambda (record) (= (ks:record-value record key) value)) records))
(defun ks:record-data (record)
  (if (and (boundp 'ks:category-id) (= ks:category-id "KANKI"))
    (mapcar '(lambda (entry)
      (if (= (car entry) "PHASE")
        (cons "PHASE" (ks:trim (vl-string-translate "φΦϕ" "   " (cdr entry)))) entry))
      (vl-remove-if-not '(lambda (entry) (member (car entry) ks:vent-tags)) (cdr record)))
    (vl-remove-if '(lambda (entry) (or (= (car entry) ks:registered-date-key)
                                     (member (car entry) ks:protected))) (cdr record))))
(defun ks:excel-column (n / result)
  (setq result "")
  (while (> n 0)
    (setq n (1- n) result (strcat (chr (+ 65 (rem n 26))) result) n (fix (/ n 26)))) result)

;; -------- Excel読込み：読取り専用。UIに進む前にCOMを全解放 --------
(defun ks:read-master (path sheet / books sheets ws used rows cols lastrow lastcol headers area matrix records result ks:date1904)
  (if (ks:remote-p path) (setq path (ks:download-master path)))
  (ks:trace "DOWNLOAD/LOCAL PATH READY")
  (if (not (findfile path)) (ks:fail (strcat "Excelファイルが存在しません：" path)))
  (ks:trace "EXCEL CREATE BEGIN")
  (setq result (vl-catch-all-apply 'vlax-create-object (list "Excel.Application")))
  (ks:trace "EXCEL CREATE END")
  (if (vl-catch-all-error-p result)
    (ks:fail (strcat "Excelを起動できません：" (vl-catch-all-error-message result))))
  (setq ks:excel result)
  (vlax-put-property ks:excel 'Visible :vlax-false)
  (vlax-put-property ks:excel 'DisplayAlerts :vlax-false)
  (vlax-put-property ks:excel 'EnableEvents :vlax-false)
  (vlax-put-property ks:excel 'AutomationSecurity 3)
  (setq books (ks:track (vlax-get-property ks:excel 'Workbooks)))
  (ks:trace "WORKBOOK OPEN BEGIN")
  ;; UpdateLinks=0, ReadOnly=True; opened files use their last saved contents.
  (setq result (vl-catch-all-apply 'vlax-invoke-method
    (list books 'Open path 0 :vlax-true 5 "" "" :vlax-true 2 "" :vlax-false :vlax-false)))
  (ks:trace "WORKBOOK OPEN END")
  (if (vl-catch-all-error-p result)
    (ks:fail (strcat "機器マスターを開けません：" path "\n" (vl-catch-all-error-message result))))
  (setq ks:book (ks:track result))
  (setq ks:date1904 (= (vlax-get-property ks:book 'Date1904) :vlax-true))
  (setq sheets (ks:track (vlax-get-property ks:book 'Worksheets)))
  (ks:trace (strcat "SHEET BEGIN / " sheet))
  (setq ws (vl-catch-all-apply 'vlax-get-property (list sheets 'Item sheet)))
  ;; 空調は既存名優先。新仕様のシート名へ移行済みの場合だけ別名を試す。
  (if (and (vl-catch-all-error-p ws) (boundp 'ks:sheet-alias) ks:sheet-alias)
    (setq ws (vl-catch-all-apply 'vlax-get-property (list sheets 'Item ks:sheet-alias))))
  (if (vl-catch-all-error-p ws) (ks:fail (strcat "シートが存在しません：" sheet "\n対象Excel：" path)))
  (setq ws (ks:track ws))
  (ks:trace "SHEET END / USED RANGE BEGIN")
  (setq used (ks:track (vlax-get-property ws 'UsedRange))
        rows (ks:track (vlax-get-property used 'Rows))
        cols (ks:track (vlax-get-property used 'Columns))
        lastrow (+ (vlax-get-property used 'Row) (vlax-get-property rows 'Count) -1)
        lastcol (+ (vlax-get-property used 'Column) (vlax-get-property cols 'Count) -1))
  ;; 最低2行のRangeにして、単一セルでも戻り値を2次元配列に揃える。
  (ks:trace (strcat "USED RANGE END / ROWS=" (itoa lastrow) " COLS=" (itoa lastcol) " / VALUE2 BEGIN"))
  (setq area (ks:track (vlax-get-property ws 'Range "A1"
                        (strcat (ks:excel-column lastcol) (itoa (max lastrow 2)))))
        matrix (vlax-get-property area 'Value2))
  (ks:trace "VALUE2 END / RECORDS BEGIN")
  (if (= (type matrix) 'VARIANT) (setq matrix (vlax-variant-value matrix)))
  (setq matrix (vlax-safearray->list matrix)
        headers (ks:header-map (car matrix)) records (ks:make-records matrix headers))
  (ks:trace (strcat "RECORDS END / COUNT=" (itoa (length records))))
  (if (and (= sheet "KANKI_MASTER") (not (assoc "EQUIP_TYPE" headers)))
    (ks:fail "KANKI_MASTERの1行目にEQUIP_TYPE列が存在しません。"))
  (ks:close-excel)
  records)

;; -------- 段階選択UI：全候補の番号は検索・ページ変更でも変えない --------
(defun ks:candidate-label (value) (if (= value "") "(未設定)" value))
(defun ks:search-options (options query / index found value)
  (setq index 0)
  (foreach value options
    (setq index (1+ index))
    (if (or (= query "") (vl-string-search (strcase query) (strcase (ks:candidate-label value))))
      (setq found (cons (cons index (ks:candidate-label value)) found))))
  (reverse found))
(defun ks:index (text / i ok)
  (setq i 1 ok (/= text ""))
  (while (and ok (<= i (strlen text)))
    (setq ok (<= 48 (ascii (substr text i 1)) 57) i (1+ i)))
  (if (and ok (<= (strlen text) 9) (> (atoi text) 0)) (atoi text)))
(defun ks:choose (title options / page query filtered pages start i entry answer index result size)
  (setq page 0 query "" size (max 1 ks:page-size))
  (while (null result)
    (setq filtered (ks:search-options options query)
          pages (max 1 (fix (/ (+ (length filtered) size -1) size))))
    (setq page (max 0 (min page (1- pages))) start (* page size) i start)
    (princ (strcat "\n" title "  [" (itoa (length filtered)) "件 / " (itoa (1+ page)) "/" (itoa pages) "ページ]"))
    (if (/= query "") (princ (strcat " 検索：" query)))
    (while (and (< i (length filtered)) (< i (+ start size)))
      (setq entry (nth i filtered))
      (princ (strcat "\n" (itoa (car entry)) ": " (cdr entry))) (setq i (1+ i)))
    (setq answer (strcase (ks:trim (getstring T "\n番号 / N:次頁 P:前頁 S:検索 B:戻る C:終了 <終了>："))))
    (cond
      ((member answer '("" "C" "CANCEL")) (setq result 'CANCEL))
      ((member answer '("B" "BACK")) (setq result 'BACK))
      ((= answer "N") (setq page (min (1- pages) (1+ page))))
      ((= answer "P") (setq page (max 0 (1- page))))
      ((= answer "S") (setq query (ks:trim (getstring T "\n検索文字（空欄で解除）：")) page 0))
      ((and (setq index (ks:index answer)) (assoc index filtered)) (setq result index))
      (T (princ "\n候補の番号、またはN/P/S/B/Cを入力してください。"))))
  result)
(defun ks:row-label (record)
  ;; 一覧表示は品番だけ。内部のレコードと選択番号は行単位のまま保持する。
  (ks:record-value record "MODEL"))
(defun ks:select-record-ui (records chooser / current steps step options answer history finished selected)
  (setq current records steps ks:selection-steps)
  (while (not finished)
    (setq ks:stage "機種の段階選択")
    (if steps
      (progn
        (setq step (car steps) options (ks:unique-candidates current (car step)))
        (if (equal options '(""))
          (progn
            (princ (strcat "\n" (cdr step) "：全候補が未設定のためスキップします。"))
            (setq steps (cdr steps)))
          (progn
            (setq answer (apply chooser (list (strcat (cdr step) " [" (car step) "]") options)))
            (if (numberp answer)
              (progn
                (setq history (cons (list steps current) history)
                      current (ks:filter-records current (car step) (nth (1- answer) options))
                      steps (cdr steps)))))))
      (if (= (length current) 1)
        (setq selected (car current) finished T answer nil)
        (progn
          (setq answer (apply chooser (list "品番を選択してください" (mapcar 'ks:row-label current))))
          (if (numberp answer) (setq selected (nth (1- answer) current) finished T)))))
    (cond
      ((eq answer 'CANCEL) (setq finished T selected nil))
      ((eq answer 'BACK)
        (if history
          (setq steps (caar history) current (cadar history) history (cdr history))
          (princ "\n最初の選択項目です。終了する場合はCを入力してください。"))))
    (setq answer nil))
  selected)
;; 日付は日単位の整数で比較する。ファイル更新日時から登録日を推測しない。
(defun ks:day-number (year month day / a y m limit)
  (setq limit (nth (1- (max 1 (min 12 month))) '(31 28 31 30 31 30 31 31 30 31 30 31)))
  (if (and (= month 2) (= (rem year 4) 0)
           (or (/= (rem year 100) 0) (= (rem year 400) 0))) (setq limit 29))
  (if (and (<= 1900 year 9999) (<= 1 month 12) (<= 1 day limit))
    (progn
      (setq a (fix (/ (- 14 month) 12)) y (- (+ year 4800) a) m (- (+ month (* 12 a)) 3))
      (+ day (fix (/ (+ (* 153 m) 2) 5)) (* 365 y) (fix (/ y 4))
         (- (fix (/ y 100))) (fix (/ y 400)) -32045))))
(defun ks:registration-day (value / s n)
  (setq s (ks:trim value))
  (cond
    ((and (= (strlen s) 10) (wcmatch s "####-##-##,####/##/##"))
      (ks:day-number (atoi (substr s 1 4)) (atoi (substr s 6 2)) (atoi (substr s 9 2))))
    ;; Value2で読み込むExcel日付（時刻があれば整数部を採用）。
    ((and (/= s "") (not (wcmatch s "*[~0-9.]*"))
          (setq n (distof s 2)) (<= 61 n 2958465)) (+ 2415019 (fix n)))
    (T nil)))
(defun ks:today (/ n year month day)
  (setq n (fix (getvar "CDATE")) year (fix (/ n 10000))
        month (rem (fix (/ n 100)) 100) day (rem n 100))
  (ks:day-number year month day))
(defun ks:registered-on (records day)
  (vl-remove-if-not '(lambda (record)
    (equal (ks:registration-day (ks:record-value record ks:registered-date-key)) day)) records))
(defun ks:select-record (records / recent answer selected done options)
  (setq recent (ks:registered-on records (ks:today)))
  (if recent
    (progn
      ;; 分類を省略し、MODELが重複してもExcelの行を選べる一覧を使う。
      (setq options (append (mapcar 'ks:row-label recent) '("マスター全体から分類で選ぶ")))
      (while (not done)
        (setq answer (ks:choose "本日Excelマスターに登録された機種" options))
        (cond
          ((eq answer 'CANCEL) (setq done T))
          ((or (eq answer 'BACK) (= answer (length options)))
            (setq selected (ks:select-record-ui records 'ks:choose) done T))
          ((numberp answer) (setq selected (nth (1- answer) recent) done T))))
      selected)
    (progn
      (if (not (vl-some '(lambda (record) (assoc ks:registered-date-key (cdr record))) records))
        (princ (strcat "\n登録日列がありません。"
          (if (and (boundp 'ks:category-id) (= ks:category-id "KANKI"))
            "KANKI_MASTER" "空調マスター")
          "の1行目に " ks:registered-date-key "、各機種の行に登録日を入力してください。")))
      (princ (strcat "\n本日の登録機種がありません（登録日列：" ks:registered-date-key
                     "）。マスター全体から選択します。"))
      (ks:select-record-ui records 'ks:choose))))

(defun ks:restore-visibility (states / entry)
  (foreach entry states
    (if (/= (vla-get-Invisible (car entry)) (cdr entry))
      (vla-put-Invisible (car entry) (cdr entry)))))
(defun ks:update (attrs data / a tag entry count cad-tags missing)
  (setq count 0)
  (foreach a attrs
    (setq tag (ks:tag (vla-get-TagString a)) cad-tags (cons tag cad-tags)
          entry (assoc tag data))
    (if (and entry (ks:writable-p tag (cdr entry))
             (/= (vla-get-TextString a) (cdr entry)))
      (progn
        ;; Record BEFORE mutation so an error or Esc can restore all changes.
        (setq ks:changes (cons (cons a (vla-get-TextString a)) ks:changes))
        (vla-put-TextString a (cdr entry))
        ;; 属性単体のUpdateは行わず、最後にブロック全体を再作図する。
        (setq count (1+ count)))))
  (foreach entry data
    (if (not (member (car entry) cad-tags))
      (princ (strcat "\nCADにないタグをスキップ：" (car entry)))))
  (foreach tag cad-tags
    (if (and (not (member tag ks:protected)) (not (assoc tag data)))
      (princ (strcat "\nExcelにないタグを保持：" tag))))
  count)

(defun ks:attribute-value (attrs tag / result a)
  (setq result "")
  (foreach a attrs
    (if (= (ks:tag (vla-get-TagString a)) tag) (setq result (vla-get-TextString a))))
  result)

(defun ks:equipment-number (attrs / system num)
  (setq system (ks:trim (ks:attribute-value attrs "SYSTEM"))
        num (ks:trim (ks:attribute-value attrs "NUM")))
  (if (and (/= system "") (/= num "")) (strcat system "-" num) ""))

(defun c:KIKISET (/ *error* ks:reason ks:stage ks:objects ks:excel ks:book ks:changes
                     ks:category-id ks:sheet-alias ks:selection-steps ks:visibility ks:download-file
                     profile block attrs path data records record doc undo count a pair result)
  (defun *error* (msg / pair r)
    (ks:trace (strcat "ERROR / " (if ks:stage ks:stage "START") " / " (if ks:reason ks:reason (if msg msg "UNKNOWN"))))
    ;; Report first: a secondary cleanup failure must not hide the cause.
    (princ (strcat "\nKIKISET [" (if ks:stage ks:stage "開始") "]："
             (cond (ks:reason)
                   ((and msg (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")) "キャンセルしました。")
                   (msg) (T "不明なエラー"))))
    (foreach pair ks:changes
      (setq r (vl-catch-all-apply 'vla-put-TextString (list (car pair) (cdr pair))))
      (if (vl-catch-all-error-p r) (princ "\n警告：属性の復元に失敗しました。図面を確認してください。"))
      )
    (if ks:visibility
      (progn
        (setq r (vl-catch-all-apply 'ks:restore-visibility (list ks:visibility)))
        (if (vl-catch-all-error-p r) (princ "\n警告：属性の表示状態を復元できませんでした。"))))
    (if doc (vl-catch-all-apply 'vla-Regen (list doc 1)))
    (ks:close-excel)
    (if undo (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (foreach a attrs (ks:release a))
    (ks:release block) (ks:release doc)
    (princ))
  ;; 選択順はカテゴリごとに設定し、コマンド外の設定へ影響させない。
  (ks:trace "START / CATEGORY WAIT")
  (setq ks:category-id (ks:category)
        profile (assoc ks:category-id ks:profiles)
        ks:sheet-alias (cadddr profile)
        ks:selection-steps (if (= ks:category-id "KANKI") ks:vent-steps ks:aircon-steps)
        ks:stage "ブロック選択")
  (ks:trace (strcat "CATEGORY=" ks:category-id " / BLOCK SELECTION WAIT"))
  (if (setq block (ks:select-block (cadr profile)))
    (progn
      (setq ks:stage "属性の取得" attrs (ks:get-attributes block))
      (if (null attrs) (ks:fail "編集可能な属性がありません。定数属性は対象外です。"))
      (princ (strcat "\nブロックを取得しました。編集可能な属性数：" (itoa (length attrs))))
      (setq ks:stage "Excelファイル選択")
      (if (setq path (ks:file))
        (progn
          (setq ks:stage "Excelマスター読み込み・機種選択"
                records (ks:load-master path (caddr profile)))
          (ks:trace "MASTER READY / MODEL SELECTION BEGIN")
          (setq record (ks:select-record records))
          (ks:trace "MODEL SELECTION END")
          (if (null record) (ks:fail "キャンセルしました。属性は変更していません。"))
          (if (and (boundp 'ks:diagnostic-readonly) ks:diagnostic-readonly)
            (progn
              (ks:trace (strcat "READONLY SUCCESS / EXCEL ROW=" (itoa (car record))))
              (ks:fail "診断完了：機種選択まで正常です。診断モードのため属性は変更しません。")))
          (setq data (ks:record-data record))
          (princ (strcat "\n確定：" (ks:record-value record "MODEL")))
          (setq ks:stage "属性の書き込み")
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
          ;; 全属性の非表示フラグを保存。未更新の属性も含め、元の設定を保つ。
          (setq ks:visibility (mapcar '(lambda (a) (cons a (vla-get-Invisible a))) attrs))
          (vla-StartUndoMark doc) (setq undo T)
          (setq count (ks:update attrs data))
          (vla-Update block)
          (ks:restore-visibility ks:visibility)
          (vla-Regen doc 1)
          (vla-EndUndoMark doc) (setq undo nil ks:changes nil)
          (princ (strcat "\n機器情報を更新しました。\n機器番号：" (ks:equipment-number attrs)
                         "\n型番：" (ks:attribute-value attrs "MODEL")
                         "\n更新項目数：" (itoa count))))
        (princ "\nキャンセルしました。")))
    (princ "\nキャンセルしました。"))
  (ks:close-excel)
  (foreach a attrs (ks:release a))
  (ks:release block) (ks:release doc)
  (princ))

(princ "\nKIKISET loaded. Command: KIKISET")
(princ)
