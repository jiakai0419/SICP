#lang scheme

(provide make-rectangle _a _b _c _d)

(require "point.scm")

(define (make-rectangle a c)
  (let ((b (make-point (_x c)
                       (_y a)))
        (d (make-point (_x a)
                       (_y c))))
    (cons a (cons c (cons b d)))))

(define (_a rectangle)
  (car rectangle))

(define (_b rectangle)
  (car (cdr (cdr rectangle))))

(define (_c rectangle)
  (car (cdr rectangle)))

(define (_d rectangle)
  (cdr (cdr (cdr rectangle))))

(display "----rectangle focus speed----")
