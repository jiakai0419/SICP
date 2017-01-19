#lang planet neil/sicp

(#%require "stream.scm")

(define (integral integrand init-value dt)
  (define int
    (cons-stream init-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C)) v0 dt))))

