#lang scheme

(define (smallest-divisor n)
  (define (find-divisor x)
    (define next
      (if (= x 2)
        3
        (+ x 2)))
    (define square (* x x))
    (define divides? (= (remainder n x) 0))
    (cond ((> square n) n)
          (divides? x)
          (else (find-divisor next))))
  (find-divisor 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (define (runtime) (* (current-inexact-milliseconds) 1000))
  (define (start-prime-test start-time)
    (define (report-prime)
      (display " *** ")
      (display (- (runtime) start-time)))
    (if (prime? n)
      (report-prime)
      (display " ### ")))
  (newline)
  (display n)
  (start-prime-test (runtime)))

(define (search-for-primes n)
  (cond ((prime? n) (timed-prime-test n))
        (else (search-for-primes (+ n 1)))))

; (search-for-primes 1000)
; (search-for-primes 10000)
; (search-for-primes 100000)
; (search-for-primes 1000000)

