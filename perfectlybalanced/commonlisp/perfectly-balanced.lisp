(ql:quickload "alexandria")

(defun balanced (str &optional (xs 0) (ys 0))
  (if (equal str "")
      (equal xs ys)   
      (let ((char1 (char str 0))
	    (str-rest (subseq str 1)))
      (cond ((equal #\x char1) (balanced str-rest (1+ xs) ys))
	    ((equal #\y char1) (balanced str-rest xs (1+ ys)))))))
  
(defun balanced_bonus (str)
  (let ((h (make-hash-table)))
    (progn
      (loop for i across str do (setf (gethash i h) (1+ (gethash i h 0))))
      (>= 1 (length (remove-duplicates (loop for v being the hash-values of h collect v)))))))

(defun balanced_bonus2 (str)
  (let ((h (make-hash-table)))
    (progn
      (loop for i across str do (setf (gethash i h) (1+ (gethash i h 0))))
      (reduce #'= (or (alexandria:hash-table-values h) '(0 0))))))
