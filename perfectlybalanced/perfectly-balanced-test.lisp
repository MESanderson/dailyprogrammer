(load "perfectly-balanced.lisp")
(defparameter tst-strings-1
  '("xxxyyy" "yyyxxx" "xxxyyyy" "yyxyxxyxxyyyyxxxyxyx" "xyxxxxyyyxyxxyxxyy" "" "x"))
(defparameter tst-1-expected
  '(T T NIL T NIL T NIL))
(defparameter tst1-result (mapcar #'balanced tst-strings-1))
(defparameter tst1-pass (car (remove-duplicates (mapcar #'equal tst-1-expected tst1-result))))

(defparameter tst-strings-2
  '("xxxyyyzzz" "abccbaabccba" "xxxyyyzzzz" "abcdefghijklmnopqrstuvwxyz" "pqq"
    "fdedfdeffeddefeeeefddf" "www" "x" "y"))
(defparameter tst-2-expected '(T T NIL T NIL NIL T T T))
(defparameter tst2-result (mapcar #'balanced_bonus tst-strings-2))
(defparameter tst2-pass (car (remove-duplicates (mapcar #'equal tst-2-expected tst2-result))))

(format t "Test 1 results: ~a ~%" (if tst1-pass "PASS" "FAIL"))
(format t "Test 2 results: ~a ~%" (if tst2-pass "PASS" "FAIL"))