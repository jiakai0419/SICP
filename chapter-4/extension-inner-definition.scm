#lang planet neil/sicp

(#%require "constants.scm")
(#%require "utils.scm")

(#%provide
 scan-out-defines) ;; have a little deficient

(define (make-let bindings body)
  (cons 'let
        (cons bindings
              body)))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define make-lambda
  (lambda (parameters body)
    (cons 'lambda
          (cons parameters body))))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (not-definition? exp)
  (not (definition? exp)))

(define (scan-out-defines body)
  (let ((definition-exps (filter definition? body))
        (other-exps (filter not-definition? body)))
    (if (null? definition-exps)
      body
      (let ((bindings
              (map (lambda (exp)
                     (list (definition-variable exp) QUOTE_UNASSIGNED))
                   definition-exps))
            (sets
              (map (lambda (exp)
                     (list 'set! (definition-variable exp) (definition-value exp)))
                   definition-exps)))
        (list (make-let bindings
                        (append sets other-exps)))))))

; (define (scan-out-defines body)
;   body)

