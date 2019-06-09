#lang scheme

; map
;     parameters:
;         f (procedure)
;         l (list)
;     returns:
;         a new list with the function applied to each element in the list
(define map
  (lambda (f l)
    (cond
      ((null? l) '())
      (else (cons (f (car l)) (map f (cdr l)))))))

; filter
;     parameters:
;         f (procedure) -> bool
;         l (list)
;     returns:
;         a new list with only those elements where (f element) evaluates to #t
(define filter
  (lambda (f l)
    (cond
      ((null? l) '())
      ((f (car l)) (cons (car l) (filter f (cdr l))))
      (else (filter f (cdr l))))))