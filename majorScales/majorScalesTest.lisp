(load "majorScales.lisp")
(defmacro test (test-val expected)
   `(format t "Testing ~a:~%  expected: ~a~%  actual:   ~a~%  result:   ~a~%"
	    ',test-val ,expected ,test-val (if (equal ,expected ,test-val)  "PASS" "FAIL"))
  )



(test (note "C" "Do") "C")
(test (note "C" "Re") "D")
(test (note "C" "Mi") "E")
(test (note "D" "Mi") "F#")
(test (note "A#" "Fa") "D#")
