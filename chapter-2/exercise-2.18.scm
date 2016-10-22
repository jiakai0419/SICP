#lang scheme

(define (reverse l)
  (define (reverse-iter a b)
    (if (null? a)
      b
      (reverse-iter (cdr a) (cons (car a) b))))
  (reverse-iter l null))

;;; test
; (reverse (list 1 4 9 16 25))
; (reverse null)
; (reverse (list 7))

