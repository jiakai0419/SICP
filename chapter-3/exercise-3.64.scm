#lang planet neil/sicp

(#%require "stream.scm")

(define (stream-limit s tolerance)
  (let ((a (stream-ref s 0))
        (b (stream-ref s 1)))
    (if (< (abs (- b a)) tolerance)
      b
      (stream-limit (stream-cdr s) tolerance))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;; test
; (sqrt 2.0 0.0001)

; (print-stream (sqrt-stream 2.0) 20)

