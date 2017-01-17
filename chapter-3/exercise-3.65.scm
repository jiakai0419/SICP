#lang planet neil/sicp

(#%require "stream.scm")
(#%require "accelerated-sequence.scm")

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1.0)))

;;; test
(print-stream ln2-stream 8)

(print-stream (euler-transform ln2-stream) 8)

(print-stream (accelerate-sequence euler-transform ln2-stream) 8)

