#lang scheme

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (carmichael? n)
  (define (carm-iter x)
    (cond ((= x 0) #t)
          ((= x (expmod x n n)) (carm-iter (- x 1)))
          (else #f)))
  (carm-iter (- n 1)))

(carmichael? 561)
(carmichael? 1105)
(carmichael? 1729)
(carmichael? 2465)
(carmichael? 2821)
(carmichael? 6601)
