#lang r5rs

;;; pseudo

(define (or-gate i1 i2 output)
  (let ((ni1 (make-wire))
        (ni2 (make-wire))
        (a (make-wire)))
    (inverter i1 ni1)
    (inverter i2 ni2)
    (and-gate ni1 ni2 a)
    (inverter a output))
  'ok)

(define or-gate-delay (+ inverter-delay and-gate-delay inverter-delay))

