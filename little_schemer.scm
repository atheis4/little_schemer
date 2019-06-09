#lang scheme

; -----------
; Chapter One

; all atoms are S-expressions

; The Law of cdr
; The primitive cdr is defined only for non-empty lists. The cdr of any non-empty list is always another list.

; The Law of cons
; The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list.

; The Law of null?
; The primitive null? is defined only for lists.

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; The Law of eq?
; The primitive eq? takes two arguments. Each must be a non-numerica atom.



; -----------
; Chapter Two

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; Note
; (cond ...) asks questions
; (lambda ...) creates a function
; (define ...) gives it a name

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; The First Commandment (preliminary)
; Always ask null? as the first question in expressing any function.

; else is a question whose value is always true.



; -------------
; Chapter Three

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; The Second Commandment
; Use cons to build lists.

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

; The Third Commandment
; When building a list, describe the first typical element, and then cons it onto the natural recursion.

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

; The Fourth Commandment
; Always change at least one argument while recurring. It must be changed to be closer to termination.
; The changing arguments must be tested in the termination condition: when using cdr, test termination with null?

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))