#lang planet neil/sicp

(#%require "stream.scm")

(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                       (stream-cdr t))
                           (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (stream-car u))
               (interleave
                 (stream-map (lambda (pair) (cons (stream-car s) pair))
                             (stream-cdr (pairs t u)))
                 (triples (stream-cdr s)
                          (stream-cdr t)
                          (stream-cdr u)))))

(define (square a) (* a a))

(define pythagoras
  (stream-filter (lambda (triple)
                   (let ((a (car triple))
                         (b (cadr triple))
                         (c (caddr triple)))
                     (= (+ (square a) (square b))
                        (square c))))
                 (triples integers integers integers)))

;;; test
; (print-stream (triples integers integers integers) 100)

; (print-stream pythagoras 5)

