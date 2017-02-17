#lang planet neil/sicp

(#%require "dispatch-table.scm")
(#%require "environment.scm")
(#%require "procedure.scm")

(#%provide
 eval
 the-empty-environment
 extend-environment
 the-global-environment
 compound-procedure?
 procedure-parameters
 procedure-body)

;; global-environment
(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    initial-env))

(define the-global-environment (setup-environment))

;; install packages
(define (install-self_evaluating-package)
  (define type 'self-evaluating)

  (put! 'analyze type (lambda (exp)
                        (lambda (env) exp)))
  'done)
(install-self_evaluating-package)

(define (install-variable-package)
  (define type 'variable)

  (put! 'analyze type (lambda (exp)
                        (lambda (env)
                          (lookup-variable-value exp env))))
  'done)
(install-variable-package)

(define (install-quoted-package)
  (define type 'quote)

  (define (text-of-quotation exp) (cadr exp))

  (put! 'analyze type (lambda (exp)
                        (let ((qval (text-of-quotation exp)))
                          (lambda (env) qval))))
  'done)
(install-quoted-package)

(define (install-assignment-package)
  (define type 'set!)

  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
      (lambda (env)
        (set-variable-value! var (vproc env) env)
        'ok)))

  (put! 'analyze type analyze-assignment)
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
  (define (analyze-definition exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
      (lambda (env)
        (define-variable! var (vproc env) env)
        'ok)))
  (put! 'analyze type analyze-definition)
  'done)
(install-definition-package)

;; TODO
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

; (define (install-let-package)
;   (define type 'let)

;   (define (let-naming? exp)
;     (symbol? (cadr exp)))
;   (define (let-bindings exp)
;     (cadr exp))
;   (define (let-body exp)
;     (cddr exp))
;   (define (letn-var exp)
;     (cadr exp))
;   (define (letn-bindings exp)
;     (caddr exp))
;   (define (letn-body exp)
;     (cdddr exp))
;   (define (let->combination exp)
;     (let ((bindings (if (let-naming? exp) (letn-bindings exp) (let-bindings exp)))
;           (body (if (let-naming? exp) (letn-body exp) (let-body exp))))
;       (let ((lbd (make-lambda (map car bindings) body)))
;         (if (let-naming? exp)
;           (let->combination (list type
;                                   (list (list (letn-var exp) lbd))
;                                   (cons type (cddr exp))))
;           (cons lbd
;                 (map cadr bindings))))))

;   (put! 'eval type (lambda (exp env)
;                      (eval (let->combination exp) env)))
;   'done)
; (install-let-package)

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

(define (analyze exp)
  ((or (get 'analyze (exp-type exp))
       analyze-application)
   exp))

(define (eval exp env)
  ((analyze exp) env))

;; apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unkown procedure type -- APPLY" procedure))))

