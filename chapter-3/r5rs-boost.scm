#lang r5rs

(#%provide (all-defined))
; (#%require "r5rs-boost.scm")

(define (error reason . args)
  (display "Error: ")
  (display reason)
  (for-each (lambda (arg) 
              (display " ")
              (write arg))
            args)
  (newline)
  (scheme-report-environment -1))

