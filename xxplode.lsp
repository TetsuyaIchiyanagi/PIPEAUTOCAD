;; XXPLODE - 完全分解（Windows AutoCAD用・単独ロード可能）
(vl-load-com)

(defun XXP-fail (message)
  (setq XXP-reason message)
  (exit))

(defun XXP-multiline-p (entity / object)
  (and (member (cdr (assoc 0 (entget entity))) '("ATTRIB" "ATTDEF"))
       (progn
         (setq object (vlax-ename->vla-object entity))
         (and (vlax-property-available-p object 'MTextAttribute)
              (= :vlax-true (vla-get-MTextAttribute object))))))

(defun XXP-locked-p (data / layer)
  (and (setq layer (tblsearch "LAYER" (cdr (assoc 8 data))))
       (/= 0 (logand 4 (cdr (assoc 70 layer))))))

(defun XXP-block-skip-p (data / record object)
  (and (= "INSERT" (cdr (assoc 0 data)))
       (or (null (setq record (tblsearch "BLOCK" (cdr (assoc 2 data)))))
           (/= 0 (logand 124 (cdr (assoc 70 record))))
           (progn
             (setq object (vla-Item (vla-get-Blocks XXP-doc) (cdr (assoc 2 data))))
             (and (vlax-property-available-p object 'Explodable)
                  (= :vlax-false (vla-get-Explodable object)))))))

(defun XXP-unsafe-attributes-p (entity / data item definition unsafe object)
  ;; 消去できないロック画層の属性定義がある参照は、複製を作る前に保護する。
  ;; 複数行属性はTEXTへの無損失変換を保証できないため、元参照を保持する。
  (setq data (entget entity))
  (if (= "INSERT" (cdr (assoc 0 data)))
    (progn
      (foreach item (XXP-attributes entity)
        (setq object (vlax-ename->vla-object (cdr (assoc -1 item))))
        (if (and (not (XXP-invisible-p item))
                 (vlax-property-available-p object 'MTextAttribute)
                 (= :vlax-true (vla-get-MTextAttribute object)))
          (setq unsafe T)))
      (setq definition (tblobjname "BLOCK" (cdr (assoc 2 data))) item definition)
      (while (and item (setq item (entnext item))
                  (/= "ENDBLK" (cdr (assoc 0 (entget item)))))
        (setq data (entget item))
        (if (= "ATTDEF" (cdr (assoc 0 data)))
          (progn
            (setq object (vlax-ename->vla-object item))
            (if (or (XXP-locked-p data)
                    (and (not (XXP-invisible-p data))
                         (vlax-property-available-p object 'MTextAttribute)
                         (= :vlax-true (vla-get-MTextAttribute object))))
              (setq unsafe T)))))))
  unsafe)

(defun XXP-tail (/ entity next)
  ;; 最後の主図形の属性・頂点も含めた末尾を取得する。
  (setq entity (entlast))
  (while (and entity (setq next (entnext entity))) (setq entity next))
  entity)

(defun XXP-new-entities (marker owner / entity data result)
  ;; 今回追加された同一所有空間の主図形だけを追跡する。
  (setq entity marker)
  (while (setq entity (entnext entity))
    (setq data (entget entity))
    (if (and (equal owner (cdr (assoc 330 data)))
             (not (member (cdr (assoc 0 data)) '("ATTRIB" "VERTEX" "SEQEND"))))
      (setq result (cons entity result))))
  (reverse result))

(defun XXP-delete (entity)
  (if (and entity (entget entity))
    (progn
      (vla-Delete (vlax-ename->vla-object entity))
      (if (entget entity) (XXP-fail "図形を削除できません。")))))

(defun XXP-discard (entity / layer locked result)
  ;; 失敗時の生成物がロック画層にあっても、一時解除して確実に取り除く。
  (if (entget entity)
    (progn
      (setq layer (vla-Item (vla-get-Layers XXP-doc) (cdr (assoc 8 (entget entity))))
            locked (vla-get-Lock layer))
      (if (= locked :vlax-true) (vla-put-Lock layer :vlax-false))
      (setq result (vl-catch-all-apply 'XXP-delete (list entity)))
      (if (= locked :vlax-true) (vla-put-Lock layer :vlax-true))
      (if (vl-catch-all-error-p result)
        (XXP-fail (strcat "生成物の後処理失敗：" (vl-catch-all-error-message result)))))))

(defun XXP-rollback (/ entity)
  ;; 確定前の複製・生成図形だけを削除し、元図形には触れない。
  (if XXP-marker
    (foreach entity (XXP-new-entities XXP-marker XXP-owner)
      (XXP-discard entity)))
  (setq XXP-marker nil))

(defun XXP-invisible-p (data)
  (/= 0 (logand 1 (cond ((cdr (assoc 70 data))) (0)))))

(defun XXP-text (data parent / props code pair object entity current result)
  ;; ATTRIBの座標は既に参照の回転・尺度・鏡像が反映されたOCS座標。
  ;; 所有空間のAddTextで作成し、TEXT共通DXFを移す。74はTEXTの73へ変換。
  (foreach code '(8 6 48 62 420 430 440 370 60 39 10 40 1 50 41 51 7 71 72 11 210)
    (if (setq pair (assoc code data)) (setq props (cons pair props))))
  (setq props (cons (cons 73 (cond ((cdr (assoc 74 data))) (0))) props))
  (if (and parent (= "0" (cdr (assoc 8 data))))
    (setq props (subst (assoc 8 parent) (assoc 8 props) props)))
  ;; 属性のByBlock表示を、消去前のブロック参照から引き継ぐ。
  (if (and parent (= 0 (cdr (assoc 62 data))) (not (assoc 420 data)))
    (progn
      (setq props (vl-remove-if '(lambda (p) (member (car p) '(62 420 430))) props))
      (foreach code '(62 420 430)
        (if (setq pair (assoc code parent)) (setq props (cons pair props))))))
  (foreach code '(6 370)
    (if (and parent (equal (cdr (assoc code data)) (if (= code 6) "BYBLOCK" -2)))
      (progn
        (setq props (vl-remove-if '(lambda (p) (= (car p) code)) props))
        (if (setq pair (assoc code parent)) (setq props (cons pair props))))))
  (setq object (vla-AddText (vlax-ename->vla-object XXP-owner)
                 (cond ((cdr (assoc 1 data))) (""))
                 (vlax-3d-point '(0.0 0.0 0.0)) (cdr (assoc 40 data)))
        entity (vlax-vla-object->ename object)
        current (entget entity))
  (foreach pair props
    (if (assoc (car pair) current)
      (setq current (subst pair (assoc (car pair) current) current))
      (setq current (append current (list pair)))))
  (if (not (entmod current)) (XXP-fail "属性文字を作成できません。"))
  (entupd entity)
  entity)

(defun XXP-attributes (entity / next data result)
  (if (= 1 (cdr (assoc 66 (entget entity))))
    (progn
      (setq next (entnext entity))
      (while (and next (/= "SEQEND" (cdr (assoc 0 (setq data (entget next))))))
        (if (= "ATTRIB" (cdr (assoc 0 data))) (setq result (cons data result)))
        (setq next (entnext next)))))
  (reverse result))

(defun XXP-attribute-for-definition (definition attributes / tag attribute found)
  ;; 同じタグの参照属性があれば、定義の既定値ではなく実際の属性値を優先する。
  (setq tag (cdr (assoc 2 definition)))
  (foreach attribute attributes
    (if (and tag (cdr (assoc 2 attribute))
             (= (strcase tag) (strcase (cdr (assoc 2 attribute)))))
      (setq found attribute)))
  found)

(defun XXP-attribute-plan (attributes definitions / item visible hidden)
  ;; 表示する実値と削除数を先に確定する。非表示値を既定値で復活させない。
  (setq hidden 0)
  (foreach item attributes
    (if (XXP-invisible-p item)
      (setq hidden (1+ hidden))
      (setq visible (cons item visible))))
  (foreach item definitions
    (if (not (XXP-attribute-for-definition item attributes))
      (if (XXP-invisible-p item)
        (setq hidden (1+ hidden))
        (setq visible (cons item visible)))))
  (list (reverse visible) hidden))

(defun XXP-explode-one (entity / data copy generated attributes definitions plan item child texts hidden)
  (setq data (entget entity) XXP-owner (cdr (assoc 330 data))
        XXP-marker (XXP-tail) hidden 0)
  ;; EXPLODEが属性値を失う前に、元参照の値・非表示フラグ・配置を保存する。
  (if (= "INSERT" (cdr (assoc 0 data)))
    (setq attributes (XXP-attributes entity)))
  (setq copy (vlax-vla-object->ename (vla-Copy (vlax-ename->vla-object entity))))
  ;; ActiveXにExplodeがない寸法・ハッチ・マルチ引出線も標準コマンドで処理。
  ;; 1図形ずつ処理し、コピーが消え、生成図形を取得できた場合のみ成功とする。
  (command-s "_.EXPLODE" copy "")
  (setq generated (XXP-new-entities XXP-marker XXP-owner))
  (if (or (entget copy) (null generated))
    (progn (XXP-rollback) nil)
    (progn
      (foreach child generated
        (setq item (entget child))
        (if (= "ATTDEF" (cdr (assoc 0 item)))
          (setq definitions (cons item definitions))))
      (setq plan (XXP-attribute-plan attributes (reverse definitions)) hidden (cadr plan))
      (foreach item (car plan) (setq texts (cons (XXP-text item data) texts)))
      ;; 値の文字化完了後に、可変・定数・非表示を問わず元の属性定義を除去する。
      (foreach item definitions (XXP-delete (cdr (assoc -1 item))))
      ;; TEXTを含め全生成要素が準備できてから元を消去する。
      (setq generated (vl-remove-if-not 'entget (append generated texts)))
      (XXP-delete entity)
      (setq XXP-marker nil)
      (list generated hidden))))

(defun XXP-definition-one (entity / data text hidden)
  ;; 直接選択された属性定義も、生成された属性定義と同じ規則で扱う。
  (setq data (entget entity) XXP-owner (cdr (assoc 330 data)) XXP-marker (XXP-tail)
        hidden (if (XXP-invisible-p data) 1 0))
  (if (= hidden 0) (setq text (XXP-text data nil)))
  (XXP-delete entity)
  (setq XXP-marker nil)
  (list (if text (list text)) hidden))

(defun XXP-process (selection / stack seen index entity data result success hidden skipped)
  ;; 動的スコープの状態はXXP-runでローカル化。再帰呼出しを使わない待ち行列。
  (setq index 0 success 0 hidden 0 skipped 0 seen (ssadd))
  (repeat (sslength selection)
    (setq stack (cons (ssname selection index) stack) index (1+ index)))
  (while stack
    (setq entity (car stack) stack (cdr stack))
    (if (and (setq data (entget entity)) (not (ssmemb entity seen)))
      (progn
        (ssadd entity seen)
        (cond
          ((or (XXP-locked-p data) (XXP-block-skip-p data))
            (setq skipped (1+ skipped)))
          ((XXP-unsafe-attributes-p entity)
            (princ "\nXXPLODE：複数行属性またはロック画層の属性定義を含む参照を保持しました。")
            (setq skipped (1+ skipped)))
          ((and (not (XXP-invisible-p data)) (XXP-multiline-p entity))
            (princ "\nXXPLODE：複数行属性定義を保持しました。")
            (setq skipped (1+ skipped)))
          ;; 末端図形は通常の完了要素として保持し、スキップ数に含めない。
          ((member (cdr (assoc 0 data)) '("LINE" "ARC" "CIRCLE" "ELLIPSE" "SPLINE"
                  "POINT" "TEXT" "SOLID" "TRACE" "3DFACE" "RAY" "XLINE")))
          (T
            (setq XXP-reason nil)
            (setq result (vl-catch-all-apply
                           (if (= "ATTDEF" (cdr (assoc 0 data)))
                             'XXP-definition-one 'XXP-explode-one)
                           (list entity)))
            (if (vl-catch-all-error-p result)
              (progn
                (XXP-rollback)
                (if (and (null XXP-reason)
                         (wcmatch (strcase (vl-catch-all-error-message result))
                           "*CANCEL*,*QUIT*,*BREAK*,*ABORT*,*キャンセル*,*取消*,*取り消*,*中断*"))
                  (exit))
                (princ (strcat "\nXXPLODE: " (cdr (assoc 5 data)) " / "
                               (if XXP-reason XXP-reason (vl-catch-all-error-message result))))
                (setq skipped (1+ skipped)))
              (if result
                (setq stack (append (car result) stack) success (1+ success)
                      hidden (+ hidden (cadr result)))
                (setq skipped (1+ skipped)))))))))
  (list success hidden skipped))

(defun XXP-run (/ *error* XXP-doc XXP-marker XXP-owner XXP-reason undo echo selection result)
  (defun *error* (message / failure)
    (setq failure (vl-catch-all-apply 'XXP-rollback nil))
    (if (vl-catch-all-error-p failure)
      (princ (strcat "\nXXPLODE 後処理エラー：" (vl-catch-all-error-message failure))))
    (if undo (vl-catch-all-apply 'vla-EndUndoMark (list XXP-doc)))
    (if echo (setvar "CMDECHO" echo))
    (princ (strcat "\nXXPLODE 中断：" message)) (princ))
  (setq XXP-doc (vla-get-ActiveDocument (vlax-get-acad-object))
        echo (getvar "CMDECHO"))
  (setq selection (ssget "_I"))
  (if (null selection)
    (progn (princ "\n完全分解するオブジェクトを選択：") (setq selection (ssget))))
  (if selection
    (if (= 0 (logand 1 (getvar "UNDOCTL")))
      (princ "\nXXPLODE：UNDOが無効です。UNDOを有効にして再実行してください。")
      (progn
        (vla-StartUndoMark XXP-doc) (setq undo T)
        (setvar "CMDECHO" 0)
        (setq result (XXP-process selection))
        (vla-EndUndoMark XXP-doc) (setq undo nil)
        (setvar "CMDECHO" echo)
        (princ (strcat "\nXXPLODE 完了\n選択オブジェクト数：" (itoa (sslength selection))
                       "\n分解成功数：" (itoa (car result))
                       "\n非表示属性削除数：" (itoa (cadr result))
                       "\nスキップ数：" (itoa (caddr result))))))
    (princ "\nXXPLODE：選択なし。"))
  (princ))

(defun c:XXPLODE () (XXP-run))
(princ)
