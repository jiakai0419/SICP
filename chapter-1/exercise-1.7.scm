#lang scheme

(define (sqrt x)
  (define (sqrt-iter guess)
    (define better-guess (average guess (/ x guess)))
    (define good-enough?
      (if (< (/ (abs (- better-guess guess)) guess) 0.0001)
        #t
        #f))
    (if good-enough?
      guess
      (sqrt-iter better-guess)))
  (sqrt-iter 1.0))

(define (average x y)
  (/ (+ x y) 2))

