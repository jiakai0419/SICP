#lang scheme

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? x) (= (remainder x 2) 0))

(define (odd? x) (not (even? x)))

; iteration
(define (fast-mult-i a b)
  (define (fast-mult-i-iter a b s)
    (cond ((= b 0) s)
          ((even? b) (fast-mult-i-iter (double a) (halve b) s))
          ((odd? b) (fast-mult-i-iter a (- b 1) (+ a s)))))
  (fast-mult-i-iter a b 0))

