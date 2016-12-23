#lang scheme

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (<= amount balance)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch pwd m)
    (if (eq? passwd pwd)
      (cond ((eq? 'withdraw m) withdraw)
            ((eq? 'deposit m) deposit)
            (else
              (error "Unknow request -- MAKE-ACCOUNT"
                     m)))
      (lambda (x) "Incorrect password")))
  dispatch)

;;; test
; (define acc (make-account 100 'Kai))
; ((acc 'Kai 'withdraw) 30)
; ((acc 'Kai 'deposit) 200)
; ((acc 'Ki 'withdraw) 30)
; ((acc 'Ki 'deposit) 200)
; ((acc 'Kai 'cancel) 200)

