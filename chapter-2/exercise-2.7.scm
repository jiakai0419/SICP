#lang scheme

(define (make-interval a b)
  (cons a b))

(define (lower-bound itv)
  (car itv))

(define (upper-bound itv)
  (cdr itv))

