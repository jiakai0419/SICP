#lang planet neil/sicp

(#%require "interpreter-beta.scm")

;; expected 120
(eval '(letrec ((fact
                  (lambda (n)
                    (if (= n 1)
                      1
                      (* n (fact (- n 1)))))))
         (fact 5))
      the-global-environment)

