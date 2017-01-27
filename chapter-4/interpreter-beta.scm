#lang planet neil/sicp

(#%require "dispatch-table.scm")

;; variable
(define (lookup-variable-value exp env)
  'SKIP)

(define (set-variable-value! variable value env)
  'SKIP)

(define (define-variable! variable value env)
  'SKIP)

;; procedure
(define (make-procedure parameters body env)
  'SKIP)

;; install packages
(define (install-self_evaluating-package)
  (define type 'self-evaluating)

  (put! 'eval type (lambda (exp env) exp))
  'done)
(install-self_evaluating-package)

(define (install-variable-package)
  (define type 'variable)

  (put! 'eval type lookup-variable-value)
  'done)
(install-variable-package)

(define (install-quoted-package)
  (define type 'quote)

  (define (text-of-quotation exp) (cadr exp))

  (put! 'eval type (lambda (exp env) (text-of-quotation exp)))
  'done)
(install-quoted-package)

(define (install-assignment-package)
  (define type 'set!)

  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env))

  (put! 'eval type eval-assignment)
  'done)
(install-assignment-package)

(define (install-definition-package)
  (define type 'define)

  (define (definition-variable exp)
    (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env))
  (put! 'eval type eval-definition)
  'done)
(install-definition-package)

(define (install-if-package)
  (define type 'if)

  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
  (define (true? predicate)
    (not (eq? predicate '#f)))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

  (put! 'eval type eval-if)
  (put! 'make type (lambda (predicate consequent alternative)
                     (list type predicate consequent alternative)))
  'done)
(install-if-package)
(define make-if (get 'make 'if))

(define (install-lambda-package)
  (define type 'lambda)

  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))

  (put! 'eval type (lambda (exp env)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env)))
  (put! 'make type (lambda (parameters body)
                     (cons type
                           (cons parameters body))))
  'done)
(install-lambda-package)
(define make-lambda (get 'make 'lambda))

(define (install-begin-package)
  (define type 'begin)

  (define (begin-actions exp) (cdr exp))
  (define (last-exp? seq) (null? (cdr exp)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-begin seq)
    (cons type seq))

  (put! 'eval type (lambda (exp env)
                       (eval-sequence (begin-actions exp) env)))
  (put! 'sequence->exp type sequence->exp)
  'done)
(install-begin-package)
(define sequence->exp (get 'sequence->exp 'begin))

(define (install-cond-package)
  (define type 'cond)

  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  (define (expand-clauses clauses)
    (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
          (if (null? rest)
            (sequence->exp (cond-actions first))
            (error "ELSE clause isn't last -- COND->IF"
                   clauses))
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest))))))

  (put! 'eval type (lambda (exp env)
                     (eval (cond->if exp) env)))
  'done)
(install-cond-package)

(define (install-application-package)
  (define type 'call)

  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (define (no-operands ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
  (define (list-of-values exps env)
    (if (no-operands exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

  (put! 'eval type (lambda (exp env)
                     (apply (eval (operator exp) env)
                            (list-of-values (operands exp) env))))
  'done)
(install-application-package)
(define eval-application (get 'eval 'call))

;; eval
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((eq? exp '#t) true)
        ((eq? exp '#f) true)
        (else #f)))

(define (variable? exp)
  (symbol? exp))

(define (exp-type exp)
  (cond ((self-evaluating? exp) 'self-evaluating)
        ((variable? exp) 'variable)
        ((pair? exp) (car exp))
        (else
          (error "Unknow expression type -- EVAL" exp))))

(define (eval exp env)
  ((or (get 'eval (exp-type exp))
       eval-application)
   exp
   env))

;; apply
(define (apply procedure arguments)
  'SKIP)

