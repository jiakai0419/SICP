#lang planet neil/sicp

(#%require "constants.scm")

(#%provide
 lookup-variable-value
 extend-environment
 define-variable!
 set-variable-value!
 the-empty-environment
 undefine-variable!)

;; common abstract
(define (scan vars vals var)
  (cond ((null? vars) #f)
        ((eq? (car vars) var) vals)
        (else
          (scan (cdr vars) (cdr vals) var))))

;; export
(define (lookup-variable-value var env)
  (define (unassigned? val)
    (if (eq? val UNASSIGNED)
      #t
      #f))
  (if (eq? env the-empty-environment)
    (error "Unbound variable" var)
    (let ((frame (first-frame env)))
      (let ((vals (scan (frame-variables frame) (frame-values frame) var)))
        (if vals
          (let ((val (car vals)))
            (if (unassigned? val)
              (error "Unassigned variable" var)
              val))
          (lookup-variable-value var (enclosing-environment env)))))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
    (error "Unbound variable" var)
    (let ((frame (first-frame env)))
      (let ((vals (scan (frame-variables frame) (frame-values frame) var)))
        (if vals
          (set-car! vals val)
          (set-variable-value! var val (enclosing-environment env)))))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((vals (scan (frame-variables frame) (frame-values frame) var)))
      (if vals
        (set-car! vals val)
        (add-binding-to-frame! var val frame)))))

(define (undefine-variable! var env)
  (define (scan pre-vars pre-vals vars vals)
    (if (not (null? vars))
      (cond ((eq? var (car vars))
             (set-cdr! pre-vars (cdr vars))
             (set-cdr! pre-vals (cdr vals)))
            (else
              (scan vars vals (cdr vars) (cdr vals))))))
  (if (not (eq? env the-empty-environment))
    (let* ((frame (first-frame env))
           (vars (frame-variables frame))
           (vals (frame-values frame)))
      (if (not (null? vars))
        (cond ((eq? var (car vars))
               (set-car! frame (cdr vars))
               (set-cdr! frame (cdr vals)))
              (else
                (scan vars vals (cdr vars) (cdr vals))))))))

;; environment
(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

;; frame
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

