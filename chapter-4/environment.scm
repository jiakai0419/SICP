#lang planet neil/sicp

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
  (if (eq? env the-empty-environment)
    (error "Unbound variable" var)
    (let ((frame (first-frame env)))
      (let ((vals (scan (frame-variables frame) (frame-values frame) var)))
        (if vals
          (car vals)
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
  (define (except var frame)
    (define (do-except vars vals)
      (cond ((null? vars) '())
            ((eq? var (car vars)) (do-except (cdr vars) (cdr vals)))
            (else
              (cons (cons (car vars) (car vals))
                    (do-except (cdr vars) (cdr vals))))))
    (let ((pairs (do-except (frame-variables frame) (frame-values frame))))
      (make-frame (map car pairs)
                  (map cdr pairs))))
  (if (eq? env the-empty-environment)
    (error "Unbound variable" var)
    (let ((frame (first-frame env)))
      (let ((new-frame (except var frame)))
        (if (= (length (frame-variables frame)) (length (frame-variables new-frame)))
          (error "Unbound variable" var)
          (set-car! env new-frame))))))

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

