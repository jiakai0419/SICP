#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (subtree)
                     (if (pair? subtree)
                       (count-leaves subtree)
                       1))
                   t)))

;;; test
; (count-leaves (list 1 3 5 (list 7 8 (list 11 15)) 9 (list (list 7 18) 1 2)))

