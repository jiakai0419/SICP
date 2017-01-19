#lang planet neil/sicp

(#%require "stream.scm")
(#%require "exercise-3.70.scm")

(define (cube x)
  (* x x x))

(define (sum-cube pair)
  (+ (cube (car pair))
     (cube (cadr pair))))

(define (ramanujan-founder pre c s)
  (let ((cur (sum-cube (stream-car s))))
    (if (= pre cur)
      (ramanujan-founder cur (+ c 1) (stream-cdr s))
      (if (> c 0)
        (cons-stream pre
                     (ramanujan-founder cur 0 (stream-cdr s)))
        (ramanujan-founder cur 0 (stream-cdr s))))))

(define ramanujan
  (ramanujan-founder 0 0 (weighted-pairs integers integers sum-cube)))

;;; test
; (print-stream ramanujan 6)

