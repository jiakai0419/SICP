#lang planet neil/sicp

(#%require "stream.scm")
(#%require "exercise-3.74.scm")

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((present (stream-car input-stream)))
    (let ((present-avpt (/ (+ present last-value) 2)))
      (cons-stream (sign-change-detector present-avpt last-avpt)
                   (make-zero-crossings (stream-cdr input-stream) present present-avpt)))))

