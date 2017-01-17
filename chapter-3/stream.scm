#lang planet neil/sicp

(#%provide
 stream-null?
 stream-car
 stream-cdr
 stream-ref
 stream-map
 print-stream
 add-streams
 partial-sums)

(define the-empty-stream '())

(define (stream-null? s)
  (eq? s the-empty-stream))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-ref s i)
  (if (= i 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- i 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car argstreams))
                 (apply stream-map
                        (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (print-stream s n)
  (if (or (stream-null? s)
          (= n 0))
    (newline)
    (begin (display (stream-car s))
           (display " ")
           (print-stream (stream-cdr s) (- n 1)))))

(define (partial-sums s)
  (add-streams s
               (cons-stream 0
                            (partial-sums s))))

