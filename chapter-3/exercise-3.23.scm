#lang r5rs

(#%require "deque.scm")

;;; test

;; case 1
(define d1 (make-deque))
(print-deque
  (front-insert-deque! d1 'a))
(print-deque
  (front-insert-deque! d1 'b))
(print-deque
  (front-insert-deque! d1 'c))
(print-deque
  (front-delete-deque! d1))
(print-deque
  (front-delete-deque! d1))
(print-deque
  (front-delete-deque! d1))
(newline)

;; case 2
(define d2 (make-deque))
(print-deque
  (rear-insert-deque! d2 'a))
(print-deque
  (rear-insert-deque! d2 'b))
(print-deque
  (rear-insert-deque! d2 'c))
(print-deque
  (rear-delete-deque! d2))
(print-deque
  (rear-delete-deque! d2))
(print-deque
  (rear-delete-deque! d2))
(newline)

;; case 3
(define d3 (make-deque))
(print-deque
  (front-insert-deque! d3 'a))
(print-deque
  (rear-insert-deque! d3 'b))
(print-deque
  (rear-insert-deque! d3 'c))
(print-deque
  (front-insert-deque! d3 'd))
(print-deque
  (rear-delete-deque! d3))
(print-deque
  (front-delete-deque! d3))
(print-deque
  (rear-insert-deque! d3 'e))
(print-deque
  (front-insert-deque! d3 'f))
(newline)

