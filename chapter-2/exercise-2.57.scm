#lang scheme

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (addend s)
  (cadr s))

(define (augend s)
  (let ((rest (cddr s)))
    (if (= 1 (length rest))
      (car rest)
      (append '(+) rest))))

(define (product? x)
  (and (pair? x) (eq? '* (car x))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (= 1 (length rest))
      (car rest)
      (append '(*) rest))))

(define (exponentiation? x)
  (and (pair? x) (eq? '** (car x))))

(define ** expt)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list '** base exponent))))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

;;; immutable
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product (make-product n (make-exponentiation u
                                                              (make-sum n -1)))
                         (deriv u var))))
        (else
          (error "unknow expression type -- DERIV" exp))))

;;; test
; (deriv '(+ x 3) 'x)
; (deriv '(* x y) 'x)
; (deriv '(* (* x y) (+ x 3)) 'x)
; (deriv '(** x 7 ) 'x)
; (deriv '(** y 7 ) 'x)

; (deriv '(* x y (+ x 3)) 'x)
; (deriv '(+ x y 4 x 7 x) 'x)
; (deriv '(+ x 3 4 (* 2 x 3)) 'x)

