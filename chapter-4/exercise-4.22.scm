#lang planet neil/sicp

(#%require "interpreter-gamma.scm")

;; expected 25
(eval '(let ((x 10)
             (f (lambda (a b)
                  (+ a b))))
         (f x 15)) the-global-environment)

