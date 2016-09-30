#lang scheme

(define (product-r term a next b)
  (if (> a b)
    1
    (* (term a) (product-r term (next a) next b))))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
    (iter a 1))

(define product product-i)
;(define product product-r)

(define (factorial n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (product term 2 next n))

(define (pi prec)
  (define (term x) (- 1 (/ 1.0 (* x x))))
  (define (next x) (+ x 2))
  (* 4 (product term 3 next prec)))

; (factorial 10)
; (pi 100)
; (pi 1000)
; (pi 10000)
; (pi 100000)
; (pi 1000000)
