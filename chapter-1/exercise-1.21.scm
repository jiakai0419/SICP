#lang scheme

(define (smallest-divisor n)
  (define (find-divisor x)
    (define square (* x x))
    (define divides? (= (remainder n x) 0))
    (cond ((> square n) n)
          (divides? x)
          (else (find-divisor (+ x 1)))))
  (find-divisor 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
