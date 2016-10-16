#lang scheme

(define (make-interval a b)
  (cons a b))

(define (lower-bound itv)
  (car itv))

(define (upper-bound itv)
  (cdr itv))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

;;; test
; (sub-interval (make-interval 0 100)
;               (make-interval 20 70))

