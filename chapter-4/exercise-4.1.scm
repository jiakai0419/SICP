#lang planet neil/sicp

(#%require "interpreter-alpha.scm")

(define (list-of-values-l2r exps env)
  (if (no-operands exps)
    '()
    (let ((fo (eval (first-operand exps) env)))
      (cons fo
            (list-of-values-l2r (rest-operands exps) env)))))

(define (list-of-values-r2l exps env)
  (if (no-operands exps)
    '()
    (let ((ros (list-of-values-r2l (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            ros))))

