#lang scheme

(define entry car)

(define left-branch cadr)

(define right-branch caddr)

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (merge-set f s1 s2)
  (let ((l1 (tree->list s1))
        (l2 (tree->list s2)))
    (list->tree (f l1 l2))))

(define (union-set s1 s2)
  (define (union l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else (let ((x1 (car l1))
                      (x2 (car l2)))
                  (cond ((< x1 x2) (cons x1
                                         (union (cdr l1) l2)))
                        ((= x1 x2) (cons x1
                                         (union (cdr l1) (cdr l2))))
                        (else (cons x2
                                    (union l1 (cdr l2)))))))))
  (merge-set union
             s1
             s2))

(define (intersection-set s1 s2)
  (define (intersection l1 l2)
    (if (or (null? l1) (null? l2))
      '()
      (let ((x1 (car l1))
            (x2 (car l2)))
        (cond ((< x1 x2) (intersection (cdr l1) l2))
              ((= x1 x2) (cons x1
                               (intersection (cdr l1) (cdr l2))))
              (else (intersection l1 (cdr l2)))))))
  (merge-set intersection
             s1
             s2))

