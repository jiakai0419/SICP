#lang scheme

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (adapt-fixed-point f first-guess)
  (define (close-enough? guess next)
    (let ((tolerance 0.00001)) (< (abs (- guess next)) tolerance)))
  (define (try guess times damped-f damped-times)
    (let ((times-thr 100000)
          (next (damped-f guess)))
      (cond ((> times times-thr) (try first-guess 1 (average-damp damped-f) (+ damped-times 1)))
            ((close-enough? guess next) (display "damped-times=") (display damped-times) (newline) next)
            (else (try next (+ times 1) damped-f damped-times)))))
  (try first-guess 1 (average-damp f) 1))

(define (nrt x n)
  (adapt-fixed-point (lambda (y) (/ x (expt y (- n 1)))) 1.0))

;;; test
; (nrt 2 2)
; (nrt 4 2)
; (nrt 8 3)
; (nrt 16 4)
; (nrt 243 5)
; (nrt 4096 6)
; (nrt 78125 7)
; (nrt 1679616 8)
; (nrt 40353607 9)
; (nrt 1073741824 10)
