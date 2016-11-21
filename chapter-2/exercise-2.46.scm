#lang scheme

(provide make-vect
         add-vect
         scale-vect)

(define (make-vect x y)
  (cons x y))

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;; test
; (define a (make-vect 2 5))
; (define b (make-vect 3 7))
; (add-vect a b)
; (sub-vect a b)
; (scale-vect 3 a)
