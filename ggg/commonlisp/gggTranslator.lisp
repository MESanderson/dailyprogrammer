(defun intTogggIter (i)
 (let* ((d (truncate (/ i 2)))
	(r  (mod i 2))
	(g (if (= r 1) "G" "g")))
    (if (= i 0)
	""
        (concatenate 'string (intTogggIter d) g))))

(defun gCodeMatch (gCode str)
  (string-equal gCode (subseq str 0 (length gCode))))

(defun take (num lst)
  (if (or (not lst) (< num 1))
      '()
      (cons (car lst) (take (1- num) (cdr lst)))))

(defun drop (num lst)
  (if (or (< num 1) (not lst))
      lst
      (drop (1- num) (cdr lst))))

(defun chunk (size l)
  (if l
      (cons (take size l) (chunk size (drop size l)))
      '()))
  
