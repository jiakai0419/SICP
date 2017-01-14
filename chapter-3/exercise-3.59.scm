#lang r5rs

;;; pseudo code

;; a)
(define (integrate-series S)
  (mul-streams S
               (stream-map (lambda (x) (/ 1 x))
                           integers)))

;; b)
(define cosine-series
  (cons-stream 1
               (integrate-series
                 (stream-map (lambda (x) -x)
                             sine-series))))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

