#lang scheme

(define (filter f l)
  (cond ((null? l) l)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (same-parity x . l)
  (cons x
        (filter (lambda (y) (= (remainder y 2) (remainder x 2)))
                l)))

;;; test
; (same-parity 1 2 3 4 5 6 7)

; (same-parity 2 3 4 5 6 7)

; (same-parity 1)

