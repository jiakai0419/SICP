#lang scheme

(require "exercise-2.40.scm")

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-eq-s-unique-triples n s)
  (filter (lambda (triple)
            (let ((a (car triple))
                  (b (cadr triple))
                  (c (caddr triple)))
              (= (+ a b c)
                 s)))
          (unique-triples n)))

;;; test
; (sum-eq-s-unique-triples 6 10)

