#lang scheme

(define (cn2-max-sum x y z) (cond ((and (< x y) (< x z)) (+ y z))
                                  ((and (< y x) (< y z)) (+ x z))
                                  (else (+ x y))))