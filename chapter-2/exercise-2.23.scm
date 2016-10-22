#lang scheme

(define (for-each f l)
  (if (null? l)
    (newline)
    ((lambda (l)
       (f (car l))
       (for-each f (cdr l)))
     l)))

;;; test
; (for-each (lambda (x) (newline) (display x))
;           (list 57 321 88))

