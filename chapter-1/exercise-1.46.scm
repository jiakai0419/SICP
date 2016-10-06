#lang scheme

(define (iterative-improve close-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (close-enough? guess next)
          next
          (try next))))
    (try first-guess)))

(define (sqrt x)
  ((iterative-improve
     (lambda (guess next) (< (abs (- guess next)) 0.001))
     (lambda (guess) (/ (+ guess (/ x guess)) 2)))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
     (lambda (guess next) (< (abs (- guess next)) 0.00001))
     (lambda (guess) (f guess)))
   first-guess))

; test
; (sqrt 49)

; (fixed-point cos 1.0)

