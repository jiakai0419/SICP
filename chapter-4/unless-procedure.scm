#lang planet neil/sicp

(#%require "interpreter-gamma.scm")

(eval '(define (unless condition usual-value exceptional-value)
         (if condition
           exceptional-value
           usual-value))
      the-global-environment)

;; naive case: expected 1
(eval '(unless #t 0 1) the-global-environment)

;; laziness case: expected error
; (eval '(unless #f 1 (/ 1 0)) the-global-environment)

;; higher-order function case: effective
(eval '(display unless) the-global-environment)

