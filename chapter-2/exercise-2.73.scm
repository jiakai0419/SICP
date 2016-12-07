#lang scheme

;;; pseudo code

(define (install-sum-expression-package)
  ;; internal procedures
  (define (tag x)
    (attach-tag '+ x))

  (define (addend s)
    (cadr s))

  (define (augend s)
    (caddr s))

  (define (make-exp a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (tag
                  (list '+ a1 a2)))))

  (define (deriv0 exp var)
    (make-exp (deriv (addend exp) var)
              (deriv (augend exp) var)))

  ;; interface to the rest of the system
  (put 'deriv '+ deriv0)
  (put 'make-exp '+ make-exp))

(define (install-product-expression-package)
  ;; internal procedures
  (define (tag x)
    (attach-tag '* x))

  (define (multiplier p)
    (cadr p))

  (define (multiplicand p)
    (caddr p))

  (define (make-exp m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (tag
                  (list '* m1 m2)))))

  (define (deriv0 exp var)
    (let ((make-sum (get 'make-exp '+)))
      (make-sum (make-exp (multiplier exp)
                          (deriv (multiplicand exp) var))
                (make-exp (derive (multiplier exp) var)
                          (multiplicand exp)))))

  ;; interface to the rest of the system
  (put 'deriv '* deriv0)
  (put 'make-exp '* make-exp))


(define (install-exponentiation-expression-package)
  ;; internal procedures
  (define (tag x)
    (attach-tag '** x))

  (define ** expt)

  (define (base e)
    (cadr e))

  (define (exponent e)
    (caddr e))

  (define (make-exp base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent)) (** base exponent))
          (else (tag
                  (list '** base exponent)))))

  (define (deriv0 exp var)
    (let ((n (exponent exp))
          (u (base exp))
          (make-product (get 'make-exp '**))
          (make-sum (get 'make-exp '+)))
      (make-product (make-product n (make-exp u
                                              (make-sum n -1)))
                    (deriv u var))))

  ;; interface to the rest of the system
  (put 'deriv '** deriv0)
  (put 'make-exp '** make-exp))

