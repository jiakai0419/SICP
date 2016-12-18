#lang scheme

;;; pseudo code

(define (=zero? a)
  (apply-generic '=zero? a))

(define (install-scheme-number-package)
  ;; ...
  (put '=zero? '(scheme-number)
       (lambda (a) (= a 0)))
  'done)

(define (install-rational-package)
  ;; ...
  (put '=zero? '(rational)
       (lambda (a)
         (= (numer a)
            0)))
  'done)

(define (install-complex-package)
  ;; ...
  (put '=zero? '(complex)
       (lambda (a)
         (and (=zero? (real-part a))
              (=zero? (imag-part a)))))
  'done)

