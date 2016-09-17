#lang scheme

(define (fast-expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          ((odd? n) (expt-iter b (- n 1) (* b a)))))
  (expt-iter b n 1))

(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (odd? x) (not (even? x)))
