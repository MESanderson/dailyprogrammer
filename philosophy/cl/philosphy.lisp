(ql:quickload '(:dexador :plump :lquery :lparallel))


(defvar  *request* (dex:get "https://en.wikipedia.org/wiki/John_W._Beschter"))

(defun parse-links-from-request (request)
  (let ((parsed-content (lquery:$ (initialize request))))
    (lquery:$ parsed-content "#content a" (attr :href))))

(defun parse-text-from-request (request)
  (let ((parsed-content (lquery:$ (initialize request))))
    (lquery:$ parsed-content "#content p" (text))))

(defparameter *links* (parse-links-from-request *request*))
(defparameter *text* (parse-text-from-request *request*))

