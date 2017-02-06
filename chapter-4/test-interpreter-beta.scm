#lang planet neil/sicp

(#%require "interpreter-beta.scm")

;; expected 10
(eval '(+ 2 3 5) the-global-environment)

;; expected -4
(eval '(- 1 2 3) the-global-environment)

