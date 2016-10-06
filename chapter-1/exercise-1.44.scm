#lang scheme

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0) 
    (lambda (x) x)
    (compose f (repeated f (- n 1)))))

(define (smooth f)
  (let ((dx 0.000001)
        (average (lambda (x y z) (/ (+ x y z) 3))))
    (lambda (x)
      (average (f (- x dx))
               (f x)
               (f (+ x dx))))))

(define (smooth-repeated f n)
  ((repeated smooth n) f))

