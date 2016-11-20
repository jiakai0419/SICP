#lang scheme

(provide make-frame origin edge1 edge2)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin car)
(define edge1 cadr)
(define edge2 cddr)

