#lang scheme

; -----------
; Chapter One
; -----------

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
; -----------

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
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

; The First Commandment (preliminary)
; Always ask null? as the first question in expressing any function.

; else is a question whose value is always true.



; -------------
; Chapter Three
; -------------

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
      (else (cons (caar l) (firsts (cdr l)))))))

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



; ------------
; Chapter Four
; ------------

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(define ox
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (ox n (sub1 m)))))))

; The Fifth Commandment
; When building a value with +, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.
; When building a value with x, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a mutliplication.
; When building a value with cons, always consider '() for the value of the teriminating line.

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2) '()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((and (zero? n) (zero? m)) #t)
      ((or (zero? n) (zero? m)) #f)
      (else (= (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define **
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (ox n (** n (sub1 m)))))))

(define ÷
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (÷ (- n m) m))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
 
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (o= n 1)))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))



; ------------
; Chapter Five
; ------------

(define rember*
  (lambda (a l)
    



; -----------
; Chapter Six
; -----------



; -------------
; Chapter Seven
; -------------



; -------------
; Chapter Eight
; -------------



; ------------
; Chapter Nine
; ------------



; -----------
; Chapter Ten
; -----------
