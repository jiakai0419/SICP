#lang scheme

;;; pseudo code

(define (adjoin-term term term-list)
  (if (=zero? term)
    term-list
    (let ((term-order (order term))
          (head-order (order (first-term term-list))))
      (if (= (- term-order head-order) 1)
        (cons (coeff term) term-list)
        (adjoin-term term (cons 0 term-list))))))

(define (first-term term-list)
  (make-term (- (length term-list) 1)
             (car term-list)))

