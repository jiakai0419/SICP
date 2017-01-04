#lang r5rs

(#%require "r5rs-boost.scm")
(#%provide empty-tree)

;;; origin
(define (make-tree entry left right)
  (list entry left right))

(define (entry tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (set-entry! tree item)
  (set-car! tree item))

(define (set-left! tree item)
  (set-car! (cdr tree)
            item))

(define (set-right! tree item)
  (set-car! (cddr tree)
            item))

(define (empty-tree? tree)
  (null? tree))

;;; binary tree
(define (empty-tree)
  (let ((local-tree (list '*tree*)))
    (define get-key car)
    (define (retrieve given-key tree)
      (if (empty-tree? tree)
        #f
        (let ((key (get-key (entry tree))))
          (cond ((< given-key key) (retrieve given-key (left tree)))
                ((< key given-key) (retrieve given-key (right tree)))
                (else (entry tree))))))
    (define (lookup given-key)
      (retrieve given-key (cdr local-tree)))
    (define (insert given-entry)
      (let ((given-key (get-key given-entry))
            (new-tree (make-tree given-entry '() '())))
        (define (insert-iter tree)
          (let ((key (get-key (entry tree)))
                (lb (left tree))
                (rb (right tree)))
            (cond ((< given-key key)
                   (if (empty-tree? lb)
                     (set-left! tree new-tree)
                     (insert-iter lb)))
                  ((< key given-key)
                   (if (empty-tree? rb)
                     (set-right! tree new-tree)
                     (insert-iter rb)))
                  (else (set-entry! tree given-entry)))))
        (if (empty-tree? (cdr local-tree))
          (set-cdr! local-tree new-tree)
          (insert-iter (cdr local-tree))))
      'ok)
    (define (print)
      (display (cdr local-tree))
      (newline))
    (define (dispatch op . args)
      (cond ((eq? op 'lookup) (lookup (car args)))
            ((eq? op 'insert) (insert (car args)))
            ((eq? op 'print) (print))
            (else
              (error "Unknow operation --EMPTY-TREE" op))))
    dispatch))

