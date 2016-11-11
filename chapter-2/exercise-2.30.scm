#lang scheme

(define (square-tree-A tree)
  (cond ((null? tree) tree)
        ((pair? tree) (cons (square-tree-A (car tree))
                            (square-tree-A (cdr tree))))
        (else (* tree tree))))

(define (square-tree-B tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (square-tree-B subtree)
           (* subtree subtree)))
       tree))

(define square-tree square-tree-A)
; (define square-tree square-tree-B)

;;; test
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

