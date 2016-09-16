#lang scheme

(define (cbrt x)
  (define (cbrt-iter guess)
    (define better-guess (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
    (define good-enough?
      (if (< (/ (abs (- better-guess guess)) guess) 0.0001)
        #t
        #f))
    (if good-enough?
      guess
      (cbrt-iter better-guess)))
  (cbrt-iter 1.0))
