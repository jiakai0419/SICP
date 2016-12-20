#lang scheme

;;; pseudo code

;; add to polynomial package
(put '=zero? '(polynomial)
     (lambda (x)
       (empty-termlist? (term-list x))))

