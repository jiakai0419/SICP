#lang planet neil/sicp

(#%provide
 lookup-variable-value
 extend-environment
 define-variable!
 set-variable-value!
 the-empty-environment)

;; export

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings) (env-loop (enclosing-environment env)))
            ((eq? var (car (first-binding bindings))) (cdr (first-binding bindings)))
            (else
              (scan (rest-bindings bindings)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan frame))))
  (env-loop env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (define-variable! var value env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings) (add-binding-to-frame! var value frame))
            ((eq? var (car (first-binding bindings))) (set-cdr! (first-binding bindings) value))
            (else
              (scan (rest-bindings bindings)))))
    (scan frame)))

(define (set-variable-value! var value env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings) (env-loop (enclosing-environment env)))
            ((eq? var (car (first-binding bindings))) (set-cdr! (first-binding bindings) value))
            (else
              (scan (rest-bindings bindings)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan frame))))
  (env-loop env))

;; environment

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

;; frame -- has little deficient

(define (make-frame variables values)
  (if (null? variables)
    '()
    (cons (cons (car variables) (car values))
          (make-frame (cdr variables) (cdr values)))))

(define (first-binding frame)
  (car frame))

(define (rest-bindings frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (define (last-node p)
    (if (null? (cdr p))
      p
      (last-node (cdr p))))
  (set-cdr! (last-node frame)
            (cons (cons var val)
                  '())))

