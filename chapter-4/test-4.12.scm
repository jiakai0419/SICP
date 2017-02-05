#lang planet neil/sicp

(#%require "exercise-4.12.scm")

(define e1 the-empty-environment)

(define e2 (extend-environment '(x y z)
                               '(1 2 4)
                               e1))

;; expected 1
(lookup-variable-value 'x e2)

;; expected 4
(lookup-variable-value 'z e2)

;; expected 2
(lookup-variable-value 'y e2)

(display "--------")
(newline)

(define e3 (extend-environment '(u v w)
                               '(7 77 777)
                               e2))

;; expected 7
(lookup-variable-value 'u e3)

;; expected 2
(lookup-variable-value 'y e3)

(define-variable! 'y 2048 e3)

;; expected 2048
(lookup-variable-value 'y e3)


(display "--------")
(newline)

(set-variable-value! 'y 11 e3)

;; expected 11
(lookup-variable-value 'y e3)

;; expected 2
(lookup-variable-value 'y e2)

(set-variable-value! 'y 13 e2)

;; expected 11
(lookup-variable-value 'y e3)

;; expected 13
(lookup-variable-value 'y e2)

(set-variable-value! 'x 17 e3)

;; expected 17
(lookup-variable-value 'x e3)

;; expected 17
(lookup-variable-value 'x e2)

