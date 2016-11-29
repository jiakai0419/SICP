#lang scheme

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
          (let ((x1 (car s1))
                (x2 (car s2)))
            (cond ((< x1 x2) (cons x1
                                   (union-set (cdr s1) s2)))
                  ((= x1 x2) (cons x1
                                   (union-set (cdr s1) (cdr s2))))
                  (else (cons x2
                              (union-set s1 (cdr s2)))))))))

