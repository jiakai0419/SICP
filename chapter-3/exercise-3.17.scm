#lang scheme

(define (count-pairs x)
  (let ((encountered '()))
    (define (count x)
      (if (or (not (pair? x))
              (memq x encountered))
        0
        (begin (set! encountered (cons x encountered))
               (+ 1
                  (count (car x))
                  (count (cdr x))))))
    (count x)))

;;; test
; (count-pairs '(a b c))

; (define c (cons 'c '()))
; (define b (cons c c))
; (define a (cons 'a b))
; (count-pairs a)

; (define z (cons 'z '()))
; (define y (cons z z))
; (define x (cons y y))
; (count-pairs x)

