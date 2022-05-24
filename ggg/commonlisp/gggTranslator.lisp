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
  

(defvar *alphaNumAll*
  (append
   (loop for i from 65 to 122 when (alphanumericp (code-char i)) collect (code-char i))
   (loop for i from 0 to 9 collect (digit-char i))))

(defun char_to_g_map_creator (msg)
  (let* ((unique-letters (remove-if-not #'alphanumericp
					 (remove-duplicates msg)))
	 (letter-and-ix (mapcar #'list
				(loop for x across unique-letters collect x)
				(loop for i from 4 to (+ 3 (length unique-letters))
				      collect (inttogggiter i))))
	 (h (make-hash-table)))
    (loop for (c g) in letter-and-ix do (setf (gethash c h) g))
    h))

(defun reverse_map (mp)
  (let ((h (make-hash-table)))
    (loop for key being the hash-keys of mp using (hash-value value)
	  do (setf (gethash value h) key))
    h))

