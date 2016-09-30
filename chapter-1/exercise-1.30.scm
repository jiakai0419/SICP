#lang scheme

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; (define (inc x) (+ x 1))
; (define (identity x) x)
; (define (cube x) (* x x x))

; (sum identity 1 inc 100)
; (sum cube 1 inc 5)
