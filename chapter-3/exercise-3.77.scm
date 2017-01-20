#lang planet neil/sicp

;;; pseudo code

(#%require "stream.scm")

(define (integral delayed-integrand init-value dt)
  (cons-stream init-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                   the-empty-stream
                   (integral (delay (stream-cdr integrand))
                             (+ init-value
                                (* (stream-car integrand) dt))
                             dt)))))

;;; test
; (define (solve f y0 dt)
;   (define y (integral (delay dy) y0 dt))
;   (define dy (stream-map f y))
;   y)

; (stream-ref (solve (lambda (y) y) 1 0.001) 1000)

