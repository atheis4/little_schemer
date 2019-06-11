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
                      ((eq? n 0) '())
                      (else (cons (car xs) (f (cdr xs) (- n 1))))))])
        (cons (f xs mid) (cons (list-tail xs mid) '())))))


(define test '(1 2))
(div-list test)

(define test2 '(1 2 3 4))
(div-list test2)

(div-half test)
(define split (div-half '(6 3 9 1 0 2)))

(define (helper xxs)
  (if (null? xxs)
  '()
  (cons (div-half (car xxs)) (helper (cdr xxs)))))

(helper split)
  
