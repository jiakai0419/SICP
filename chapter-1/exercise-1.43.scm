#lang scheme

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0) 
    (lambda (x) x)
    (compose f (repeated f (- n 1)))))

;;; test
; (define (square x) (* x x))

; ((repeated square 2) 5)
; ((repeated square 3) 5)
