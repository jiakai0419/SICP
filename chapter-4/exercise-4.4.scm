#lang planet neil/sicp

(#%require "interpreter-beta.scm")

;;; test
(define env '())

;; and
(eval '(and) env)
(eval '(and #t) env)
(eval '(and #f) env)
(eval '(and #f #t #t) env)
(eval '(and #t #f #t) env)
(eval '(and #t #t #f) env)
(eval '(and #t #t #t) env)

(newline)
(display "--------")
(newline)

;; or
(eval '(or) env)
(eval '(or #t) env)
(eval '(or #f) env)
(eval '(or #t #f #f) env)
(eval '(or #f #t #f) env)
(eval '(or #f #f #t) env)
(eval '(or #f #f #f) env)

