#lang r5rs

;;; pseudo

(define generate-account-id
  (let ((id 0)
        (s (make-serializer)))
    (s (lambda ()
         (set! id (+ id 1))
         id))))

(define (make-account balance)
  ;; withdraw
  ;; ...
  ;; deposit
  ;; ...
  (let ((id (generate-account-id))
        (balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else
              (error "unknow m --MAKE-ACCOUNT" m))))
    dispatch))

;; exchange
;; ...

(define (serialized-exchange account1 account2)
  (define (do-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
      ((serializer1 (serializer2 exchange)) account1 account2)))
  (if (< (account1 'id) (account2 'id))
    (do-exchange account1 account2)
    (do-exchange account2 account1)))

