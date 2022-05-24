(ql:quickload "uiop")
(ql:quickload "alexandria")

(defvar *mirror-hash*)
(defvar *words)

(defparameter *max-mirror-size* 13)


(defun get-all-ix (pred seq)
  (if (stringp seq) (setf seq (loop for x across seq collect x)))
  (loop for x in seq
	for y from 0
	if (funcall pred x) collect y)
  )

(defun get-mirror-ixs (mirror-list)
  (labels ((is-back (x) (equal x #\\))
	  (is-forward (x) (equal x #\/))
	   (is-mirror (x) (or (is-back x) (is-forward x))))
  (loop for m in mirror-list
	for y from 0
	for mix = (get-all-ix #'is-mirror m)
        append (mapcar #'(lambda (x) (list y x (elt m x))) mix))))


(defun move-left (coord coord-list)
  (find-if #'(lambda (x)
	       (and (= (car coord) (car x))
		    (> (cadr coord) (cadr x)))) coord-list :from-end t))

(defun move-right (coord coord-list)
  (find-if #'(lambda (x)
	      (and (= (car coord) (car x))
		   (< (cadr coord) (cadr x)))) coord-list))

(defun move-up (coord coord-list)
  (find-if #'(lambda (x)
	      (and (= (cadr coord) (cadr x))
		   (> (car coord) (car x)))) coord-list :from-end t))

(defun move-down (coord coord-list)
  (find-if #'(lambda (x)
	       (and (= (cadr coord) (cadr x))
		    (< (car coord) (car x)))) coord-list))

(defparameter *letter-coord-lookup*
  (let ((h (make-hash-table :test 'equal)))
    (loop for i from 65 to 78
	  for ix = (- i 65)
	  for key = (code-char i)
	  do (setf (gethash key h) (list ix -1 :right)))
    (loop for i from 78 to 90
	  for ix = (- i 78)
	  for key = (code-char i)
	  do (setf (gethash key h) (list *max-mirror-size* ix :up)))
    (loop for i from 97 to 109
	  for ix = (- i 97)
	  for key = (code-char i)
	  do (setf (gethash key h) (list -1 ix :down)))
    (loop for i from 110 to 122
	  for ix = (- i 110)
	  for key = (code-char i)
	  do (setf (gethash key h) (list ix *max-mirror-size* :left)))
    (maphash #'(lambda (k v)
		 (destructuring-bind (a b dir) v
		 (let ((new-dir (case dir (:left :right) (:right :left) (:up :down) (:down :up))))
		   (setf (gethash (list a b new-dir) h) k)))) h)
  h))
    


(defun get-mirror-coords (start-coord mirror-ixs dir)
  (let* ((next-coord (cond ((eq dir :right) (move-right start-coord mirror-ixs))
			  ((eq dir :left) (move-left start-coord mirror-ixs))
			  ((eq dir :up) (move-up start-coord mirror-ixs))
			  ((eq dir :down) (move-down start-coord mirror-ixs))))
	 (next-dir (cond ((not next-coord) nil)
			  ((and (eq dir :right) (equal (caddr next-coord) #\\)) :down)
			  ((and (eq dir :right) (equal (caddr next-coord) #\/)) :up)
			  ((and (eq dir :left) (equal (caddr next-coord) #\\)) :up)
			  ((and (eq dir :left) (equal (caddr next-coord) #\/)) :down)
			  ((and (eq dir :up) (equal (caddr next-coord) #\\)) :left)
			  ((and (eq dir :up) (equal (caddr next-coord) #\/)) :right)
			  ((and (eq dir :down) (equal (caddr next-coord) #\\)) :right)
			  ((and (eq dir :down) (equal (caddr next-coord) #\/)) :left))))
    ;;(format t "~a ~a->~a ~a~%" start-coord dir next-coord next-dir)	  
    (if next-coord
        (get-mirror-coords next-coord mirror-ixs next-dir)
	(case dir (:left (list (car start-coord) -1 :left))
	      (:right (list (car start-coord) *max-mirror-size* :right))
	      (:up (list -1 (cadr start-coord) :up))
	      (:down (list  *max-mirror-size* (cadr start-coord) :down))))))
	 
	 


(defun get-mirror-hash (mirror-list)
  (let ((h (make-hash-table :test 'equal))
	(mirror-ixs (get-mirror-ixs mirror-list)))
    (progn
    (maphash #'(lambda (k v)
		 (if (characterp k)
		     (setf (gethash k h)
			   (destructuring-bind (a b dir) v
			     (gethash (get-mirror-coords (list a b) mirror-ixs dir)
				      *letter-coord-lookup*))))) *letter-coord-lookup*)
    h)))

(defun letter-check (letter)
  (let* ((s-coord (gethash letter *letter-coord-lookup*))
	 (e-coord (get-mirror-coords s-coord *mirror-ixs* (caddr s-coord))))
    (gethash e-coord *letter-coord-lookup*)))

(with-open-file (stream "../test-mirror.txt")
  (let ((lines (uiop/stream:read-file-lines stream)))
    (setf *words (elt lines 13))
    (setf *mirror-lines (subseq lines 0 13))
    (setf *mirror-hash* (get-mirror-hash *mirror-lines)))
  )
;;(format t "~{~a~%~}" *mirror-hash*)
(format t "~a" (concatenate 'string (loop for i across *words collect (gethash i *mirror-hash* #\?))))


