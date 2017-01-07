#lang r5rs

;;; pseudo code

(define (averager a b c)
  (let ((m (make-connector))
        (two (make-connector)))
    (constant 2 two)
    (adder a b m)
    (multiplier two c m)
    'ok))

