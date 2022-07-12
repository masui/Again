(require 'again)

;;
;; again.elのテスト
;;
(and
 (string= (get-postfix "xx" "yy") "")
 (string= (get-postfix "xyz" "yzwu") "wu")
 (string= (get-postfix "xyzabc" "abcdefg") "defg")
 (string= (get-postfix "abc" "abcdefg") "defg")
 (string= (get-postfix "abckkk" "abcdefg") "")
 (string= (get-postfix "xxabcdef" "abcdefghi") "ghi")
 (string= (get-postfix "abcdef" "abcdefghi") "ghi")
 (string= (chomp "") "")
 (string= (chomp "xyz") "xy")
 (string= (chomp "") "")
 (let* (
	(s1 (concat [1 2 3 4 5 6 100 102]))
	(s2 "abcde")
	(s3 "kkkkk")
	(in (concat s1 s2))
	(out (concat s2 s3))
	)
   (string= (get-postfix in out) s3)
   )
 (let* (
	(s1 (concat [3 8 9 100 50 16 12 33 87 23 8 4 3]))
	(s2 "")
	(s3 "register")
	(in (concat s1 s2))
	(out (concat s2 s3))
	)
   (string= (get-postfix in out) s3)
   )
 )







