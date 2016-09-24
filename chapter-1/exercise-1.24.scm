#lang scheme

(define fermat-factor 10)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (define (runtime) (* (current-inexact-milliseconds) 1000))
  (define (start-prime-test start-time)
    (define (report-prime)
      (display " *** ")
      (display (- (runtime) start-time)))
    (if (fast-prime? n fermat-factor)
      (report-prime)
      (display " ### ")))
  (newline)
  (display n)
  (start-prime-test (runtime)))

(define (search-for-primes n)
  (cond ((fast-prime? n fermat-factor) (timed-prime-test n))
        (else (search-for-primes (+ n 1)))))

; (search-for-primes 1000)
; (search-for-primes 10000)
; (search-for-primes 100000)
; (search-for-primes 1000000)

