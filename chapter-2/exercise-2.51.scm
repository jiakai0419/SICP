#lang racket

(require (except-in sicp-pict
                    transform-painter))
(require "exercise-2.50.scm")

(define (below-a painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((painter-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (painter-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (painter-bottom frame)
        (painter-top frame)))))
;;; test
; (paint (below-a einstein diagonal-shading))

(define (below-b painter1 painter2)
  (rotate90
    (beside (rotate270 painter1)
            (rotate270 painter2))))
;;; test
; (paint (below-b einstein diagonal-shading))

