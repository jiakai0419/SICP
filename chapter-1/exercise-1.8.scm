#lang scheme

(define (cbrt x) (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cbrt-iter (improve guess x)
                 x)))

(define (good-enough? a b)
  (if (< (/ (abs (- b a)) a) 0.0001)
      #t
      #f))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

