#lang scheme

;;; pseudo code

;; negative
(define (negative a)
  (apply-generic 'negative a))

;; add to polynomial package
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list (negative-poly p2))))
    (error "Polys not in same var -- SUB-POLY"
           (list p1 p2))))

