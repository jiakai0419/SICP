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
  (let ((ec 0)
        (call-the-cops (lambda () "cops!!!")))
    (lambda (pwd m)
      (if (eq? passwd pwd)
        (begin (set! ec 0)
               (cond ((eq? 'withdraw m) withdraw)
                     ((eq? 'deposit m) deposit)
                     (else
                       (error "Unknow request -- MAKE-ACCOUNT"
                              m))))
        (begin (set! ec (+ ec 1))
               (if (>= ec 7)
                 (lambda (x) (call-the-cops))
                 (lambda (x) "Incorrect password")))))))

;;; test
; (define acc (make-account 100 'Kai))
; (define acc2 (make-account 200 'Jia))

; ((acc 'K 'deposit) 10)
; ((acc 'K 'deposit) 10)
; ((acc 'K 'deposit) 10)
; ((acc 'K 'deposit) 10)
; ((acc 'K 'deposit) 10)
; ((acc 'K 'deposit) 10)
; ((acc 'K 'deposit) 10)

; ((acc 'K 'deposit) 10)

; ((acc 'Kai 'deposit) 30)
; ((acc 'K 'deposit) 10)

; ((acc2 'Jia 'withdraw) 30)

