#lang scheme

(define (fixed-point f first-guess)
  (define (close-enough? guess next)
    (let ((tolerance 0.00001)) (< (abs (- guess next)) tolerance)))
  (define (try guess times)
    (display guess)
    (display "  ")
    (let ((next (f guess)))
      (if (close-enough? guess next)
        ((lambda (x) (display x) (newline) (+ 1 times)) next)
        (try next (+ 1 times)))))
  (try first-guess 1))

; 非平均阻尼
(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

; 平均阻尼
(fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2)) 1.1)

