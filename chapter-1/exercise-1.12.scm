#lang scheme

(define (pascal-elem r c)
  (cond ((= c 1) 1)
        ((= r c) 1)
        (else (+ (pascal-elem (- r 1) (- c 1))
                 (pascal-elem (- r 1) c)))))
