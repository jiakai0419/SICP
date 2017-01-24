#lang planet neil/sicp

(#%provide
 no-operands
 first-operand
 rest-operands
 tagged-list?
 eval)

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define (no-operands exps)
  'TODO)

(define (first-operand exps)
  'TODO)

(define (rest-operands exps)
  'TODO)

(define (eval exp env)
  'TODO)

