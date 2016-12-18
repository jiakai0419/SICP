#lang scheme

;;; pseudo code

(define (equ? a b)
  (apply-generic 'equ? a b))

(define (install-scheme-number-package)
  ;; ...
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

(define (install-rational-package)
  ;; ...
  (put 'equ? '(rational rational)
       (lambda (a b)
         (= (* (numer a) (denom b))
            (* (denom a) (numer b)))))
  'done)

(define (install-complex-package)
  ;; ...
  (put 'equ? '(complex complex)
       (lambda (a b)
         (and (equ? (real-part a) (real-part b))
              (equ? (imag-part a) (imag-part b)))))
  'done)

