#lang scheme

; recursion
(define (fr n)
  (cond ((< n 3) n)
        (else (+ (fr (- n 1))
                 (* 2 (fr (- n 2)))
                 (* 3 (fr (- n 3)))))))


; iteration
(define (fi n)
  (define (fi-iter a b c t)
    (if (= t 0)
      c
      (fi-iter b c (+ c (* 2 b) (* 3 a)) (- t 1))))
  (cond ((< n 3) n)
        (else (fi-iter 0 1 2 (- n 2)))))
