#lang planet neil/sicp

(#%require "dispatch-table.scm")
(#%require "environment.scm")
(#%require "procedure.scm")
(#%require "exercise-4.20.scm")

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

(define (install-undefinition-package)
  (define type 'make-unbound!)

  (define (undefinition-variable exp)
    (cadr exp))
  (define (eval-undefinition exp env)
    (undefine-variable! (undefinition-variable exp)
                        env))

  (put! 'eval type eval-undefinition)
  'done)
(install-undefinition-package)

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
    (if (true? (actual-value (if-predicate exp) env))
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
  (define (last-exp? seq) (null? (cdr seq)))
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
  (put! 'eval-sequence type eval-sequence)
  'done)
(install-begin-package)
(define sequence->exp (get 'sequence->exp 'begin))
(define eval-sequence (get 'eval-sequence 'begin))

(define (install-cond-package)
  (define type 'cond)

  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  (define (clause->exp clause)
    (let ((snd (cadr clause)))
      (if (eq? snd '=>)
        (recipient-clause->exp clause)
        (sequence->exp (cond-actions clause)))))
  (define (recipient-clause->exp clause)
    (let ((test (cond-predicate clause))
          (recipient (caddr clause)))
      (list recipient
            test)))

  (define (expand-clauses clauses)
    (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
          (if (null? rest)
            (clause->exp first)
            (error "ELSE clause isn't last -- COND->IF"
                   clauses))
          (make-if (cond-predicate first)
                   (clause->exp first)
                   (expand-clauses rest))))))

  (put! 'eval type (lambda (exp env)
                     (eval (cond->if exp) env)))
  'done)
(install-cond-package)

(define (install-application-package)
  (define type 'call)

  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))

  (put! 'eval type (lambda (exp env)
                     (apply (actual-value (operator exp) env)
                            (operands exp)
                            env)))
  'done)
(install-application-package)
(define eval-application (get 'eval 'call))

(define (install-and-package)
  (define type 'and)

  (define (and-clauses exp) (cdr exp))
  (define (and->if exp)
    (expand-clauses (and-clauses exp)))
  (define (expand-clauses clauses)
    (if (null? clauses)
      #t
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 (expand-clauses rest)
                 #f))))

  (put! 'eval type (lambda (exp env)
                     (eval (and->if exp) env)))
  'done)
(install-and-package)

(define (install-or-package)
  (define type 'or)

  (define (or-clauses exp) (cdr exp))
  (define (or->if exp)
    (expand-clauses (or-clauses exp)))
  (define (expand-clauses clauses)
    (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 #t
                 (expand-clauses rest)))))

  (put! 'eval type (lambda (exp env)
                     (eval (or->if exp) env)))
  'done)
(install-or-package)

(define (install-let-package)
  (define type 'let)

  (define (let-naming? exp)
    (symbol? (cadr exp)))
  (define (let-bindings exp)
    (cadr exp))
  (define (let-body exp)
    (cddr exp))
  (define (letn-var exp)
    (cadr exp))
  (define (letn-bindings exp)
    (caddr exp))
  (define (letn-body exp)
    (cdddr exp))
  (define (let->combination exp)
    (let ((bindings (if (let-naming? exp) (letn-bindings exp) (let-bindings exp)))
          (body (if (let-naming? exp) (letn-body exp) (let-body exp))))
      (let ((lbd (make-lambda (map car bindings) body)))
        (if (let-naming? exp)
          (let->combination (list type
                                  (list (list (letn-var exp) lbd))
                                  (cons type (cddr exp))))
          (cons lbd
                (map cadr bindings))))))

  (put! 'eval type (lambda (exp env)
                     (eval (let->combination exp) env)))
  'done)
(install-let-package)

(define (install-let*-package)
  (define type 'let*)

  (define (let-bindings exp)
    (cadr exp))
  (define (let-body exp)
    (cddr exp))
  (define (let*->nested-lets exp)
    (expand-bindings (let-bindings exp) (let-body exp)))
  (define (expand-bindings bindings body)
    (if (null? bindings)
      body
      (cons 'let
            (cons (list (car bindings))
                  (expand-bindings (cdr bindings) body)))))

  (put! 'eval type (lambda (exp env)
                     (eval (let*->nested-lets exp) env)))
  'done)
(install-let*-package)

(define (install-letrec-package)
  (define type 'letrec)

  (put! 'eval type (lambda (exp env)
                     (eval (letrec->let exp) env)))
  'done)
(install-letrec-package)

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
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (no-operands ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-arg-values exps env)
  (if (no-operands exps)
    '()
    (cons (actual-value (first-operand exps) env)
          (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands exps)
    '()
    (cons (delay-it (first-operand exps) env)
          (list-of-delayed-args (rest-operands exps) env))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)
             (procedure-environment procedure))))
        (else
          (error "Unkown procedure type -- APPLY" procedure))))

