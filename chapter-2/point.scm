#lang scheme

(provide make-point _x _y print-point)

(define (make-point x y)
  (cons x y))

(define (_x p)
  (car p))

(define (_y p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (_x p))
  (display ",")
  (display (_y p))
  (display ")"))

