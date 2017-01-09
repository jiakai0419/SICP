#lang r5rs

;;; pseudo code

;; z = x - y | z + y = x
(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

;; z = x * y
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

;; z = x / y | z * y = x
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv x)
  (let ((y (make-connector)))
    (constant x y)
    y))

