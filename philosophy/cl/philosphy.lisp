(ql:quickload '(:dexador :plump :lquery :lparallel))


;;(defvar  *request* (dex:get "https://en.wikipedia.org/wiki/John_W._Beschter"))
(defparameter *link-sleep-time* .1)

(defun parse-links-from-request (request)
  (let ((parsed-content (lquery:$ (initialize request))))
    (lquery:$ parsed-content "#content a" (attr :href))))

(defun parse-text-from-request (request)
  (let ((parsed-content (lquery:$ (initialize request))))
    (lquery:$ parsed-content "#content p" (text))))

(defun wikilink-p (link)
  (and link
       (> (length link) 5)
       (string= (subseq link 0 5) "/wiki")
       (not (search ":" link))))

(defun keep-wiki-links (linkList)
  (remove-duplicates
   (remove-if (complement #'wikilink-p) linkList)
   :test #'string=))

(defun get-full-wiki-address (link) (string-concat "https://en.wikipedia.org" link))

(defun findpage-from (startAddress endAddress)
  "Returns multiple values (end-found links-found)"
  (let* ((req (dex:get startAddress))
	 (pg-links (parse-links-from-request req))
	 (wiki-links (keep-wiki-links pg-links))
	 (full-wiki-links (map 'list #'get-full-wiki-address wiki-links)))
      (sleep *link-sleep-time*)
    (values (find endAddress full-wiki-links :test #'string=) full-wiki-links)))


(defun page-find-iter (startAddress endAddress)
  (let ((links-to-visit (list startAddress))
	(links-visited '())
	(link-found '()))
  (lambda ()
    (multiple-value-bind (found links) (findpage-from (car links-to-visit) endAddress)
      (setf links-to-visit (remove-duplicates (append (cdr links-to-visit) links)))
      (setf links-visited (append links-visited (list (car links-to-visit))))
      (setf link-found found)
      (values link-found links-to-visit links-visited)
      ))))

;; (defun find-in-n-steps (startAddress endAddress maxiters)
;;   (let ((iter (page-find-iter startAddress endAddress)))
;;     (dotimes  (num maxiters)
;;       (multiple-value-bind (lf ltv lv)  (funcall iter)
;; 	(if lf
;; 	    (progn
;; 	      (print "HI")
;; 	      (return (values num lf ltv lv)))
;; 	    nil)))))

(defun find-in-n-steps (startAddress endAddress maxiters)
  (let ((iter (page-find-iter startAddress endAddress)))
    (labels ((find-step (startAddress endAddress step)
	       (multiple-value-bind (lf ltv lv) (funcall iter)
		 (format t "step ~a: ~a~%" step (last lv))
		       (if (or (= 0 step) lf)
			   (values (- maxiters step) lf ltv lv)
			   (find-step startAddress endAddress (1- step))))))
    (find-step startAddress endAddress maxiters))))

;;(defparameter *links* (parse-links-from-request *request*))
;;(defparameter *text* (parse-text-from-request *request*))


(defparameter *root* "https://en.wikipedia.org/wiki/Cat")
(defvar *end* "https://en.wikipedia.org/wiki/Adolf_Hitler")
(multiple-value-bind (i lf ltv lv) (find-in-n-steps *root* *end* 300)
  (print i)
  (defparameter *lf* lf)
  (defparameter *ltv* ltv)
  (defparameter *lv* lv)
  nil)
(defparameter *tstiter* (page-find-iter *root* *end*))

;;(defvar startAddress *root*)
;;(defvar endAddress *end*)
;;(defparameter req (dex:get startAddress))
;;(defparameter pg-links (parse-links-from-request req))
;;(defparameter wiki-links (keep-wiki-links pg-links))



