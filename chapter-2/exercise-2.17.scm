#lang scheme

(define (last-pair l)
  (if (null? (cdr l))
    (car l)
    (last-pair (cdr l))))

;;; test
; (last-pair (list 23 73 149 34))

