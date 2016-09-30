#lang scheme
(require "exercise-1.23.scm")

(define (filtered-accumulate combiner null-value term a next b cnd)
  (define (iter a result)
    (cond ((> a b) result)
          ((cnd a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (identity x) x)
(define (inc x) (+ x 1))

;;; a)
(define (func_a a b)
  (define (combiner x y) (+ x y))
  (define cnd prime?)
  (filtered-accumulate combiner 0 identity a inc b cnd))

(func_a 10 20)

;;; b)
(define (func_b n)
  (define (combiner x y) (* x y))
  (define (cnd x) (= 1 (gcd x n)))
  (filtered-accumulate combiner 1 identity 2 inc (- n 1) cnd))

(func_b 20)
