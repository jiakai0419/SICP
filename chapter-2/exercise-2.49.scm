#lang racket

(require (except-in sicp-pict
                    make-vect))

(require "exercise-2.46.scm")

(define one 0.99)

(define a (make-vect 0 one))
(define b (make-vect one one))
(define c (make-vect one 0))
(define d (make-vect 0 0))

(define w (scale-vect 0.5
                      (add-vect a b)))
(define x (scale-vect 0.5
                      (add-vect b c)))
(define y (scale-vect 0.5
                      (add-vect c d)))
(define z (scale-vect 0.5
                      (add-vect d a)))

(define boundary-painter
  (segments->painter (list (make-segment a b)
                           (make-segment b c)
                           (make-segment c d)
                           (make-segment d a))))
;;; test
; (paint boundary-painter)

(define diagonal-painter
  (segments->painter (list (make-segment a c)
                           (make-segment b d))))
;;; test
; (paint diagonal-painter)

(define diamond-painter
  (segments->painter (list (make-segment w x)
                           (make-segment x y)
                           (make-segment y z)
                           (make-segment z w))))
;;; test
; (paint diamond-painter)


;;; TODO
;;; define wave-painter

