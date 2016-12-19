#lang scheme

;;; pseudo code

;;; equals
(define (equ? a b)
  (apply-generic 'equ? a b))

;;; project
(define (project a)
  (apply-generic 'project a))

;; add to complex package
(put 'project '(complex)
     (lambda (x)
       (make-real (real-part x))))

;; add to real package
(put 'project '(real)
     (lambda (x)
       (make-rational (round x) 1)))

;; add to rational package
(put 'project '(rational)
     (lambda (x)
       (round (/ (numer x) (denom x)))))

;;; drop
(define (drop a)
  (let ((type (type-tag a)))
    (if (eq? type 'integer)
      a
      (let ((b (project a)))
        (if (equ? a (raise b))
          (drop b)
          a)))))

;;; apply-generic
(define (apply-generic op . args)
  (define (no-method type-tags)
    (error "No method for these types"
           (list op type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (drop (apply proc (map contents args)))
        (if (= (length args) 2)
          (let ((level1 (get-level (car type-tags)))
                (level2 (get-level (cadr type-tags)))
                (a1 (car args))
                (a2 (cadr args)))
            (cond ((< level1 level2) (apply-generic op (raise a1) a2))
                  ((< level2 level1) (apply-generic op a1 (raise a2)))
                  (else (no-method))))
          (no-method type-tags))))))

