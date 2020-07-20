(defun balanced (str)
  (let ((h (make-hash-table)))
    (progn
      (loop for i across str do (setf (gethash i h) (1+ (gethash i h 0))))
      (equal 1 (length (remove-duplicates (loop for v being the hash-values of h collect v)))))))
	
