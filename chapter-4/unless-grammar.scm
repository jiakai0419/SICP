#lang planet neil/sicp

(#%require "interpreter-gamma.scm")

;; naive case: expected 1
(eval '(unless #t 0 1) the-global-environment)

;; laziness case: expected 1
(eval '(unless #f 1 (/ 1 0)) the-global-environment)

;; higher-order function case: error
; (eval '(display unless) the-global-environment)

