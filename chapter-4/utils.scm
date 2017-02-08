#lang planet neil/sicp

(#%provide
 tagged-list?
 filter)

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define (filter pred lizt)
  (cond ((null? lizt) '())
        ((pred (car lizt)) (cons (car lizt)
                                 (filter pred (cdr lizt))))
        (else
          (filter pred (cdr lizt)))))

