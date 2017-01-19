#lang planet neil/sicp

(#%require "stream.scm")
(#%require "exercise-3.70.scm")

(define (square a) (* a a))

(define (sum-square pair) (+ (square (car pair))
                             (square (cadr pair))))

(define (founder buffer s)
  (let ((pre (car buffer))
        (cur (stream-car s)))
    (let ((pss (sum-square pre))
          (css (sum-square cur)))
      (if (= pss css)
        (founder (cons cur buffer) (stream-cdr s))
        (if (= 4 (length buffer))
          (cons-stream buffer
                       (founder (list cur (sum-square cur)) (stream-cdr s)))
          (founder (list cur (sum-square cur)) (stream-cdr s)))))))

(define r
  (founder '((0 0) 0) (weighted-pairs integers integers sum-square)))

;;; test
; (print-stream r 6)
