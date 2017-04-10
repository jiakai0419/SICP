#lang swindle

(define (require p)
  (if (not p)
    (amb)))

(define (an-integer-from low)
  (amb low (an-integer-from (+ low 1))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (square x)
  (* x x))

(define (a-pythagorean-triple)
  (let ((k (an-integer-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (square i) (square j)) (square k)))
        (list i j k)))))

;;; test
; (print (a-pythagorean-triple))

