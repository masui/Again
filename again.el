;;
;; again.el - Emacs上の操作を再実行するシステム
;;
;;  増井俊之
;;  masui@masui.org
;;  2022/7/12
;;  https://github.com/masui/Again
;;

;;
;; ~/.emacsで以下を設定
;; (require 'again)
;; (defconst *again-key* "\C-l" "再実行キー")
;; (global-set-key *again-key* 'exec-again)
;;

;;  M-x toggle-debug-on-error

(provide 'again)

(defvar *again-macro* [] "繰り返し文字列")
(defvar *old-history* [] "ちょっと前のrecent-keys")
(defvar *new-history* []  "最新のrecent-keys")

(defun clear-kbd-macro ()
  (setq *again-macro* [])
  (setq *old-history* (recent-keys))
  )

(run-with-idle-timer 1 t 'clear-kbd-macro)

;;
;; "xyzabcdefg" と "abcdefghij" から "hij" を得る
;;
(defun get-postfix (s1 s2)
  (let (
	(len1 (length s1)) (len2 (length s2))
	(found nil) (i 0) (res [])
	)
    (while (and (< i len2) (not found))
      (let* ((s (substring s2 0 (- len2 i)))
	     (p (substring s1 (- (min len1 (length s))))))
	(setq found (equal s p))
	(if found (setq res (substring s2 (- i))))
	)
      (setq i (1+ i))
      )
    res
    )
  )


(defun chomp (s) ; 文字列の最後の文字を除く
  (let ((len (length s)))
    (if (= len 0) []  (substring s 0 (1- len)))
    )
  )

(defun exec-again () ;;; *again-key* で呼ばれる
  (interactive)
  (let* ((recent (recent-keys)) (len (length recent)))
    ;;(if (not (string= (substring recent -2) (concat *again-key* *again-key*))) ; 連打のときは何もしない
    (if (not (and (= (aref recent (- len 1)) (aref *again-key* 0))
		  (= (aref recent (- len 2)) (aref *again-key* 0))))
	(progn
	  (if (not (equal *again-macro* [])) ; 新規作成じゃない場合
	    (setq *old-history* *new-history*)
	    )
	  (setq *again-macro* (chomp (get-postfix *old-history* recent)))
	  )
      )
    (setq *new-history* recent)
    (execute-kbd-macro *again-macro*)
    )
  )

;;
;; exec-again の動作
;; * は時間待ち
;;
;; キー操作       123456789*            時間待ちしたところ
;; *old-history*  123456789             時間待ちで設定
;; *new-history*       
;; *again-macro*            
;; 
;; キー操作       123456789*abcL
;; *old-history*  123456789             時間待ちで設定されたまま
;; *new-history*       6789 abcL        Lで設定
;; *again-macro*            abc         引算計算 + 実行
;; 
;; キー操作       123456789*abcLL
;; *old-history*  123456789
;; *new-history*        789 abcLL
;; *again-macro*            abc         LLなので*again-macro*を実行
;; 
;; キー操作       123456789*abcLLL
;; *old-history*  123456789
;; *new-history*         89 abcLLL
;; *again-macro*            abc         LLなので*again-macro*を実行
;; 
;; キー操作       123456789*abcLLLd
;; *old-history*  123456789
;; *new-history*         89 abcLLL      変化せず
;; *again-macro*            abc         実行せず / 実行しない
;; 
;; キー操作       123456789*abcLLLde
;; *old-history*  123456789             変化せず
;; *new-history*         89 abcLLL      変化せず
;; *again-macro*            abc         変化せず / 実行しない
;; 
;; キー操作       123456789*abcLLLdeL
;; *old-history*         89 abcLLL      前の*new-history*をコピー
;; *new-history*            abcLLLdeL
;; *again-macro*                  de    引算+実行
