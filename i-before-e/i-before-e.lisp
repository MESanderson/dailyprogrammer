(ql:quickload "cl-ppcre")

(defparameter exception-regex (ppcre:create-scanner "([^cC]ei|cie)"))
(defun i-before-e-p (string-in)
  (null (ppcre:scan exception-regex string-in)))
