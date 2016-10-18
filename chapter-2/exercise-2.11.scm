#lang scheme

(provide make-interval lower-bound upper-bound)

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

(define (mul-interval x y)
  (let ((a1 (lower-bound x))
        (b1 (upper-bound x))
        (a2 (lower-bound y))
        (b2 (upper-bound y)))
    (cond ((and (< a1 0) (< b1 0) (< a2 0) (< b2 0)) (make-interval (* b1 b2) (* a1 a2)))
          ((and (< a1 0) (< b1 0) (< a2 0) (>= b2 0)) (make-interval (* b2 a1) (* a1 a2)))
          ((and (< a1 0) (< b1 0) (>= a2 0) (>= b2 0)) (make-interval (* b2 a1) (* a2 b1)))
          ((and (< a1 0) (>= b1 0) (< a2 0) (< b2 0)) (make-interval (* b1 a2) (* a1 a2)))
          ((and (< a1 0) (>= b1 0) (< a2 0) (>= b2 0)) (make-interval (min (* a1 b2) (* a2 b1))
                                                                      (max (* a1 a2) (* b1 b2))))
          ((and (< a1 0) (>= b1 0) (>= a2 0) (>= b2 0)) (make-interval (* b2 a1) (* b1 b2)))
          ((and (>= a1 0) (>= b1 0) (< a2 0) (< b2 0)) (make-interval (* b1 a2) (* a1 b2)))
          ((and (>= a1 0) (>= b1 0) (< a2 0) (>= b2 0)) (make-interval (* a2 b1) (* b1 b2)))
          ((and (>= a1 0) (>= b1 0) (>= a2 0) (>= b2 0)) (make-interval (* a1 a2) (* b1 b2))))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (<= 0 (upper-bound y)))
    (error "div 0: " y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

