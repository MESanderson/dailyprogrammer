#lang racket

(define (chunk size l)
  (if (> (length l) size)
  (cons (take l size) (chunk size (drop l size)))
  (list l)))

(define (intTogggIter i)
  (let*-values ([(a b) (quotient/remainder i 2)]
                [(g) (if (= b 0) "g" "G")])
    (if (= i 0) ""
        (string-append (intTogggIter a) g))))

(chunk 3 '(1 2 3 4 5 6 7 8))