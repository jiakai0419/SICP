#lang scheme

(provide make-mobile
         left-branch
         right-branch
         make-branch
         branch-length
         branch-structure)

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define left-branch car) ; nodiff

(define right-branch cdr)

(define branch-length car) ; nodiff

(define branch-structure cdr)

