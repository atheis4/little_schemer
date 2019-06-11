#lang scheme

(define (div-list xs)
  (if (null? xs)
      '()
      (cons (cons (car xs) '()) (div-list (cdr xs)))))


(define (div-half xs)
  (if (null? xs)
      '()
      (letrec ([len (length xs)]
               [mid (ceiling (/ len 2))]
               [f (lambda (xs n)
                    (cond
                      ((eq? n 0) (cons xs '()))
                      (else (cons (car xs) (f (cdr xs) (- n 1))))))])
        (f xs mid))))


(define test '(1 2))
(div-list test)

(define test2 '(1 2 3 4))
(div-list test2)

(div-half test)
(div-half '(1 2 3 4 5 6))
