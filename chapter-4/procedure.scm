#lang planet neil/sicp

(#%require "extension-inner-definition.scm")
(#%require "utils.scm")

(#%provide
 primitive-procedure?
 primitive-procedure-names
 primitive-procedure-objects
 apply-primitive-procedure
 make-procedure
 compound-procedure?
 procedure-parameters
 procedure-body
 procedure-environment)

;; primitive procedure
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'display display)
        (list 'newline newline)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

;; compound-procedure
(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        (scan-out-defines body)
        env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

