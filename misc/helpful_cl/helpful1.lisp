

(defmacro recur (fn argList &body body)
  `(defun ,fn ,argList
  (progn
    (print ',fn)
    (print ',argList)
    (print ',body)
    (print '(defun ,fn
		,argList
	      ,@(butlast body)
	      (values ,(car (last body)) ,(car (last body)))))
    ,@body
    )))
       


(recur tstfn (a b) (+ a b))

(tstfn 1 2)
