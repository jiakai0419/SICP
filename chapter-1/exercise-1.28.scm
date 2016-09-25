#lang scheme

(define mr-factor 10)

(define (square x) (* x x))

(define (expmod base exp m)
  (define (non-trivial-sqrt-check x)
    (if (and (not (= x 1))
         (not (= x (- m 1)))
         (= 1 (remainder (square x) m)))
      0
      x))
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (non-trivial-sqrt-check (expmod base (/ exp 2) m))) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (mr-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) (remainder 1 n)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((mr-test n) (fast-prime? n (- times 1)))
        (else #f)))

