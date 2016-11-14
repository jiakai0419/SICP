#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x)
                                  y))
              null
              sequence))
;;; test
; (map sqr (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))
;;; test
; (append (list 1 3 5 7) (list 2 4 6 8))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))
;;; test
; (length (list 0 1 1 2 3 5 8 13))

