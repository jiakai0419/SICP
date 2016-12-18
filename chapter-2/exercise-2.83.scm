#lang scheme

;;; pseudo code

(define (raise x)
  (apply-generic 'raise x))

;; add into integer package
(put 'raise '(integer)
     (lambda (x)
       (make-rational x 1)))

;; add into rational package
(put 'raise '(rational)
     (lambda (x)
       (make-real (/ (numer x)
                     (denom x)))))

;; add into real package
(put 'raise '(real)
     (lambda (x)
       (make-complex-from-real-imag x 0)))

