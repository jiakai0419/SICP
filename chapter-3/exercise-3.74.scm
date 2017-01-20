#lang planet neil/sicp

(#%provide sign-change-detector)
(#%require "stream.scm")

(define sense-data '())  ; fake

(define (sign-change-detector present last)
  (cond ((and (< last 0) (> present 0)) 1)
        ((and (> last 0) (< present 0)) -1)
        (else 0)))

(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0
                           sense-data)))

