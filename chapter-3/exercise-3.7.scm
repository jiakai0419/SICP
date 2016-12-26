#lang scheme

(define (incorrect-password x)
  "Incorrect password")

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
      incorrect-password))
  dispatch)

(define (make-joint account passwd new-passwd)
  (define (dispatch pwd m)
    (if (eq? new-passwd pwd)
      (account passwd m)
      incorrect-password))
  dispatch)

;;; test
; (define adam-acc (make-account 65535 'adam_1024))
; (define eve-acc (make-joint adam-acc 'adam_1024 'eve_2048))

; ((adam-acc 'adam_1024 'withdraw) 100)
; ((eve-acc 'eve_2048 'withdraw) 35)

; ((adam-acc 'adam_1024 'deposit) 200)
; ((eve-acc 'eve_2048 'deposit) 400)

; ((adam-acc 'adam_xyz 'deposit) 200)
; ((eve-acc 'eve_uvw 'deposit) 400)

; (define snake-acc (make-joint eve-acc 'eve_uvw 'snake_4096))
; ((snake-acc 'snake_4096 'withdraw) 60000)

