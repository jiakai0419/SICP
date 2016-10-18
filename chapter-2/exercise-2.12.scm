#lang scheme

(require "exercise-2.11.scm")

(define (make-center-percent c p)
  (make-interval (- c (* c p 0.01))
                 (+ c (* c p 0.01))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i)) 2 0.01 (center i)))

;;; test
(define i (make-center-percent 50 2))
(display i)
(center i)
(percent i)

