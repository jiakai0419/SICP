#lang scheme

(require "exercise-2.46.scm")

(define (make-segment start end)
  (lambda (x)
    (if (= x 0)
      start
      end)))

(define (start-segment s)
  (s 0))

(define (end-segment s)
  (s 1))

;;; test
; (define s (make-segment (make-vect 2 2)
;                         (make-vect 7 7)))
; (start-segment s)
; (end-segment s)

