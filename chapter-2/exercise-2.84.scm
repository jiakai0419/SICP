#lang scheme

;;; pseudo code

(define (index e lst)
  (cond ((null? lst) (error "not found" e))
        ((eq? e (car lst)) 0)
        (else (+ 1
                 (index e (cdr lst))))))

(define level
  '(integer rational real complex))

(define (get-level type)
  (index type level))

(define (apply-generic op . args)
  (define (no-method type-tags)
    (error "No method for these types"
           (list op type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((level1 (get-level (car type-tags)))
                (level2 (get-level (cadr type-tags)))
                (a1 (car args))
                (a2 (cadr args)))
            (cond ((< level1 level2) (apply-generic op (raise a1) a2))
                  ((< level2 level1) (apply-generic op a1 (raise a2)))
                  (else (no-method))))
          (no-method type-tags))))))

