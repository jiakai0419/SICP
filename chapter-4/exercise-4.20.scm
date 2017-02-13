#lang planet neil/sicp

(#%require "constants.scm")

(#%provide letrec->let)

(define (make-let bindings body)
  (cons 'let
        (cons bindings
              body)))

(define (letrec->let exp)
  (let ((letrec-bindings (cadr exp))
        (letrec-body (cddr exp)))
    (let ((bindings (map (lambda (binding)
                           (list (car binding) QUOTE_UNASSIGNED))
                         letrec-bindings))
          (sets (map (lambda (binding)
                       (list 'set! (car binding) (cadr binding)))
                     letrec-bindings)))
      (make-let bindings
                (append sets letrec-body)))))

