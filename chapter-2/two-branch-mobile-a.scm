#lang scheme

(provide make-mobile
         left-branch
         right-branch
         make-branch
         branch-length
         branch-structure)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car) ; nodiff

(define right-branch cadr)

(define branch-length car) ; nodiff

(define branch-structure cadr)

