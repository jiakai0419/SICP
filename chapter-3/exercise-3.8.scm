#lang scheme

(define f
  (let ((called #f))
    (lambda (x)
      (if called
        0
        (begin (set! called #t)
               x)))))

;;; test
; (+ (f 0) (f 1))
; (+ (f 1) (f 0))

