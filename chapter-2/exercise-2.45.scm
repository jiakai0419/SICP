#lang racket

(require sicp-pict)

(define (split identity-op smaller-op)
  (define (split-rec painter n)
    (if (= n 0)
      painter
      (let ((smaller (split-rec painter (- n 1))))
        (identity-op painter
                     (smaller-op smaller smaller)))))
  split-rec)

(define right-split (split beside below))

(define up-split (split below beside))

;;; test
(paint (right-split einstein 2))
(paint (up-split einstein 2))

