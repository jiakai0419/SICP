#lang planet neil/sicp

(#%require "stream.scm")
(#%require "exercise-3.5.scm")

(define (random-stream-in-range low high)
  (cons-stream (random-in-range low high)
               (random-stream-in-range low high)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (exact->inexact (/ passed (+ passed failed)))
                 (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(define (estimate-integral P x1 x2 y1 y2)
  (scale-stream (monte-carlo (stream-map P
                                         (random-stream-in-range x1 x2)
                                         (random-stream-in-range y1 y2))
                             0
                             0)
                (* (- x2 x1) (- y2 y1))))

(define estimate-pi
  (estimate-integral (lambda (x y)
                       (<= (+ (sqr (- x 1))
                              (sqr (- y 1)))
                           1))
                     0 2 0 2))

;;; test

;; center (5, 7) | radius 3 | s ~= 28.2743334
; (stream-ref (estimate-integral (lambda (x y)
;                                  (<= (+ (sqr (- x 5))
;                                         (sqr (- y 7)))
;                                      9))
;                                2 8 4 10)
;             1000000)

;; pi
; (stream-ref estimate-pi 10000000)

