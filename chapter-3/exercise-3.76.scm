#lang planet neil/sicp

(#%require "stream.scm")
(#%require "exercise-3.74.scm")

(define sense-data (cons-stream 1.0
                                (cons-stream -1.0
                                             (cons-stream 2.0
                                                          -2.0))))  ; fake

(define (smooth s)
  (stream-map (lambda (a b) (/ (+ a b) 2))
              s
              (stream-cdr s)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream (sign-change-detector (stream-car input-stream) last-value)
               (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream))))

(define zero-crossings
  (make-zero-crossings (smooth sense-data) 0))

