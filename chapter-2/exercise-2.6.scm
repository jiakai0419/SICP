#lang scheme

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;;; test
(define (f x) (display "*"))
((one f) "a")
(newline)
((two f) "a")
(newline)
(((add one two) f) "a")
(newline)

