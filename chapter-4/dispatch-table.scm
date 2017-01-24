#lang planet neil/sicp

(#%require "../chapter-3/exercise-3.25.scm")

(#%provide
 put!
 get)

(define table (make-table))

(define (put! op type item)
  (table 'insert! (list op type) item))

(define (get op type)
  (table 'lookup (list op type)))

