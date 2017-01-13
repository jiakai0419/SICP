#lang r5rs

;;; pseudo code

;; normal version
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;; grace version
(define (partial-sum s)
  (add-streams s
               (cons-stream 0
                            (partial-sum s))))

