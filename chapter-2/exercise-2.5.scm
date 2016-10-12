#lang scheme

(define (p x base)
  (if (= 0 (remainder x base))
    (+ 1 (p (/ x base) base))
    0))

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (p z 2))

(define (cdr z)
  (p z 3))

;;; test
(define x (cons 5 10))
(car x)
(cdr x)

