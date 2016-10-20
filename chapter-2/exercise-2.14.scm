#lang scheme

(require "exercise-2.11.scm")
(require "exercise-2.12.scm")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one 
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;; test 1
(define A (make-interval 2.0 10.0))
(define B (make-interval 1.0 2.0))
(par1 A B)
(par2 A B)

;;; test 2
(div-interval A A)
(div-interval A B)

;;; test 3
(display "----test 3----")
(newline)
(define A0 (make-center-percent 10 1))
(define B0 (make-center-percent 20 1))
(div-interval A0 A0)
;(div-interval A0 B0)

(define A1 (make-center-percent 10 10))
(define B1 (make-center-percent 20 10))
(div-interval A1 A1)
;(div-interval A1 B1)

(define A2 (make-center-percent 10 30))
(define B2 (make-center-percent 20 30))
(div-interval A2 A2)
;(div-interval A2 B2)

(define A3 (make-center-percent 10 60))
(define B3 (make-center-percent 20 60))
(div-interval A3 A3)
;(div-interval A3 B3)

