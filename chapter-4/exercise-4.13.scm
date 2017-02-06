#lang planet neil/sicp

(#%require "interpreter-beta.scm")

(define e1 (extend-environment '(x y z) '(2 3 5) the-empty-environment))
;; expected 2
(eval 'x e1)

(eval '(define universe 42) e1)
;; expected 42
(eval 'universe e1)

(eval '(make-unbound! universe) e1)
;; expected 2
(eval 'x e1)
;; expected 5
(eval 'z e1)
;; expected 3
(eval 'y e1)
;; expected error
(eval 'universe e1)

