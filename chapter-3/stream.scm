#lang planet neil/sicp

(#%provide
 stream-null?
 stream-car
 stream-cdr
 stream-ref
 stream-map
 stream-filter
 print-stream
 add-streams
 scale-stream
 partial-sums
 interleave
 integers)

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

(define (stream-filter pred s)
  (let ((a (stream-car s)))
    (if (pred a)
      (cons-stream a
                   (stream-filter pred (stream-cdr s)))
      (stream-filter pred (stream-cdr s)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s m)
  (stream-map (lambda (x) (* m x))
              s))

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

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))

(define integers
  (integers-starting-from 1))

