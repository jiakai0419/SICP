#lang scheme

(define (make-accumulator s)
  (lambda (x)
    (begin (set! s (+ s x))
           s)))

;;; test
; (define A (make-accumulator 5))
; (A 10)
; (A 10)

