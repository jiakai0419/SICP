#lang swindle

(define (require p)
  (if (not p)
    (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (square x)
  (* x x))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (square i) (square j)) (square k)))
        (list i j k)))))

;;; test
; (print (amb-collect (a-pythagorean-triple-between 1 50)))

