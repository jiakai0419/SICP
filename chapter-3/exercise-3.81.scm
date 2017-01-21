#lang planet neil/sicp

(#%require "stream.scm")

(define (rand-update x)
  (let ((a 1103515245)
        (b 12345)
        (m 2147483647))
    (modulo (+ (* a x)
               b)
            m)))

(define (rand-stream input-stream)
  (define init-seed 0)
  (define (generate-rand-stream input-stream seed)
    (if (stream-null? input-stream)
      the-empty-stream
      (let ((input (stream-car input-stream)))
        (if (eq? input 'g)
          (let ((next (rand-update seed)))
            (cons-stream next
                         (generate-rand-stream (stream-cdr input-stream) next)))
          (generate-rand-stream (stream-cdr input-stream) input)))))
  (generate-rand-stream input-stream init-seed))

;;; test
; (define istream
;   (cons-stream 1024
;                (cons-stream 'g
;                             (cons-stream 'g
;                                          (cons-stream 'g
;                                                       (cons-stream 'g
;                                                                    (cons-stream 'g
;                                                                                 istream)))))))
; (print-stream (rand-stream istream) 13)

