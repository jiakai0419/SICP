#lang scheme

(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate-r combiner null-value term (next a) next b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define accumulate accumulate-i)
; (define accumulate accumulate-r)

(define (sum term a next b)
  (define (combiner x y) (+ x y))
  (define null-value 0)
  (accumulate combiner null-value term a next b))

(define (product term a next b)
  (define (combiner x y) (* x y))
  (define null-value 1)
  (accumulate combiner null-value term a next b))

;;; test product
(define (pi prec)
  (define (term x) (- 1 (/ 1.0 (* x x))))
  (define (next x) (+ x 2))
  (* 4 (product term 3 next prec)))

(pi 1000000)

;;; test sum
(define (cube-sum a b)
  (define (term x) (* x x x))
  (define (next x) (+ x 1))
  (sum term a next b))

(cube-sum 5 10)

