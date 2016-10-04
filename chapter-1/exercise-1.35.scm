#lang scheme

(define (fixed-point f first-guess)
  (define (close-enough? guess next)
    (let ((tolerance 0.00001)) (< (abs (- guess next)) tolerance)))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
