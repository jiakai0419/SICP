#lang planet neil/sicp

(#%require "stream.scm")

(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                       (stream-cdr t))
                           (interleave (pairs (stream-cdr s) (stream-cdr t))
                                       (stream-map (lambda (x) (list x (stream-car t)))
                                                   (stream-cdr s))))))

;;; test
; (print-stream (pairs integers integers) 100)

