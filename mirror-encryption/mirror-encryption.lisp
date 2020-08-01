(ql:quickload "uiop")
(defvar *mirror-hash*)
(defvar *words)

(defun get-mirror-hash (mirror-list)
  mirror-list)

(with-open-file (stream "test-mirror.txt")
  (let ((lines (uiop/stream:read-file-lines stream)))
    (setf *words (subseq lines 13))
    (setf *mirror-hash* (get-mirror-hash (subseq lines 0 13))))
  )
(format t "~{~a~%~}" *mirror-hash*)
