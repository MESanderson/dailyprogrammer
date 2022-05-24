(ql:quickload "cl-ppcre")

(defparameter exception-regex (ppcre:create-scanner "([^c]ei|cie)"))
(defun i-before-e-p (string-in)
  (null (ppcre:scan exception-regex (string-downcase string-in))))
