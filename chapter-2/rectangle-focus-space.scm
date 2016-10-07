#lang scheme

(provide make-rectangle _a _b _c _d)

(require "point.scm")

(define (make-rectangle a c)
  (cons a c))

(define (_a rectangle)
  (car rectangle))

(define (_b rectangle)
  (make-point (_x (_c rectangle))
              (_y (_a rectangle))))

(define (_c rectangle)
  (cdr rectangle))

(define (_d rectangle)
  (make-point (_x (_a rectangle))
              (_y (_c rectangle))))

(display "----rectangle focus space----")
