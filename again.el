(provide 'again)

;;
;; ~/.emacsで以下を設定
;; (require 'again)
;; (defconst *again-key* "\C-l" "再実行指定キー")
;; (global-set-key *again-key* 'exec-again)
;;

(defvar *last-macro* "" "繰り返し文字列")
(defvar *old-recent* "" "ちょっと前のrecent-keys")
(defvar *new-recent* "" "最新のrecent-keys")

(defun clear-kbd-macro ()
  (setq *last-macro* "")
  (setq *old-recent* (concat (recent-keys)))
  )

(run-with-idle-timer 1 t 'clear-kbd-macro)

;;
;; "xyzabcdefg" と "abcdefghij" から "hij" を得る
;;
(defun get-postfix (s1 s2)
  (let (
	(len1 (length s1)) (len2 (length s2))
	(found nil) (i 0) (res "")
	)
    (while (and (< i len2) (not found))
      (let* ((s (substring s2 0 (- len2 i)))
	     (p (substring s1 (- (min len1 (length s))))))
	(setq found (string= s p))
	(if found (setq res (substring s2 (- i))))
	)
      (setq i (1+ i))
      )
    res
    )
  )

(defun chomp (s) ; 文字列の最後の文字を除く
  (let ((len (length s)))
    (if (= len 0) ""  (substring s 0 (1- len)))
    )
  )

(defun exec-again () ;;; *again-key* で呼ばれる
  (interactive)
  (let ((recent (concat (recent-keys))))
    (if (not (string= (substring recent -2) (concat *again-key* *again-key*))) ; 連打のときは何もしない
	(progn
	  (if (not (string= *last-macro* "")) ; 新規作成じゃない場合
	    (setq *old-recent* *new-recent*)
	    )
	  (setq *last-macro* (chomp (get-postfix *old-recent* recent)))
	  )
      )
    (setq *new-recent* recent)
    (execute-kbd-macro *last-macro*)
    )
  )

;;
;; exec-again の動作
;; * は時間待ち
;;
;; キー操作      123456789*            時間待ちしたところ
;; *old-recent*  123456789             時間待ちで設定
;; *new-recent*       
;; *last-macro*            
;; 
;; キー操作      123456789*abcL
;; *old-recent*  123456789             時間待ちで設定されたまま
;; *new-recent*       6789 abcL        Lで設定
;; *last-macro*            abc         引算計算 + 実行
;; 
;; キー操作      123456789*abcLL
;; *old-recent*  123456789
;; *new-recent*        789 abcLL
;; *last-macro*            abc         LLなので*last-macro*を実行
;; 
;; キー操作      123456789*abcLLL
;; *old-recent*  123456789
;; *new-recent*         89 abcLLL
;; *last-macro*            abc         LLなので*last-macro*を実行
;; 
;; キー操作      123456789*abcLLLd
;; *old-recent*  123456789
;; *new-recent*         89 abcLLL      変化せず
;; *last-macro*            abc         実行せず / 実行しない
;; 
;; キー操作      123456789*abcLLLde
;; *old-recent*  123456789             変化せず
;; *new-recent*         89 abcLLL      変化せず
;; *last-macro*            abc         変化せず / 実行しない
;; 
;; キー操作      123456789*abcLLLdeL
;; *old-recent*         89 abcLLL      前の*new-recent*をコピー
;; *new-recent*            abcLLLdeL
;; *last-macro*                  de    引算+実行


