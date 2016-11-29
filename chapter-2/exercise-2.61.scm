#lang scheme

(define (adjoin-set x set)
  (if (null? set)
    (cons x set)
    (let ((head (car set)))
      (cond ((< x head) (cons x set))
            ((= x head) set)
            (else (cons head
                        (adjoin-set x (cdr set))))))))

