#lang r5rs

(define (loop? x)
  (let ((slow x)
        (fast x))
    (define (next)
      (cond ((or (null? (cdr fast))
                 (null? (cddr fast))) #f)
            ((or (eq? slow (cdr fast))
                 (eq? slow (cddr fast))) #t)
            (else (set! fast (cddr fast))
                  (set! slow (cdr slow))
                  (next))))
    (next)))

;;; test
; (display (loop? '(a b c d e)))
; (newline)

; (define lp '(a b c d e))
; (set-cdr! (cddddr lp) (cdr lp))
; (display (loop? lp))
; (newline)

