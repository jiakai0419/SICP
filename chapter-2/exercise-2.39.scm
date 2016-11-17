#lang scheme

(define (fold-right op init sequence)
  (if (null? sequence)
    init
    (op (car sequence)
        (fold-right op init (cdr sequence)))))

(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter init sequence))

(define (reverse-r sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              null
              sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y)
                (cons y x))
              null
              sequence))

;;; test
; (define reverse reverse-r)
; (define reverse reverse-l)

; (reverse (list 1 3 5 7 9 2))

