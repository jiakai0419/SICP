#lang scheme

(define (deep-reverse l)
  (define (dr-iter a b)
    (if (null? a)
      b
      (dr-iter (cdr a)
               (cons (deep-reverse (car a))
                     b))))
  (cond ((null? l) l)
        ((pair? l) (dr-iter l null))
        (else l)))

;;; test
; (define x (list (list 1 2) (list 3 4)))
; x
; (deep-reverse x)

; (deep-reverse null)

; (deep-reverse (list 1 2 3 4))

; (define y (list 1 2 (list 3 4 5) 6 (list 7 8) (list 9) 10))
; y
; (deep-reverse y)

; (define z (list (list (list 1 2)
;                       (list 3 4))
;                 (list (list 5 6)
;                       (list 7 8))))
; z
; (deep-reverse z)

