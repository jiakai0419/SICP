#lang r5rs

(#%provide
 make-deque
 empty-deque?
 front-deque
 rear-deque
 front-insert-deque!
 rear-insert-deque!
 front-delete-deque!
 rear-delete-deque!
 print-deque)
(#%require "r5rs-boost.scm")

;; deque
(define (make-deque)
  (cons '() '()))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

;; node
(define (make-node value pre next)
  (cons value
        (cons pre next)))

(define (value node)
  (car node))

(define (pre node)
  (cadr node))

(define (next node)
  (cddr node))

(define (set-pre! node item)
  (set-car! (cdr node) item))

(define (set-next! node item)
  (set-cdr! (cdr node) item))

;; deque opt
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an empty deque" deque)
    (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an empty deque" deque)
    (value (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (if (empty-deque? deque)
    (insert-when-empty-deque! deque item)
    (let ((head (front-ptr deque)))
      (let ((new-node (make-node item '() head)))
        (set-pre! head new-node)
        (set-front-ptr! deque new-node)
        deque))))

(define (rear-insert-deque! deque item)
  (if (empty-deque? deque)
    (insert-when-empty-deque! deque item)
    (let ((tail (rear-ptr deque)))
      (let ((new-node (make-node item tail '())))
        (set-next! tail new-node)
        (set-rear-ptr! deque new-node)
        deque))))

(define (insert-when-empty-deque! deque item)
  (let ((new-node (make-node item '() '())))
    (set-front-ptr! deque new-node)
    (set-rear-ptr! deque new-node)
    deque))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
    (error "FRONT-DELETE called with an empty deque" deque)
    (let ((new-head (next (front-ptr deque))))
      (set-front-ptr! deque new-head)
      (if (null? new-head)
        deque
        (begin (set-pre! new-head '())
               deque)))))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
    (error "REAR-DELETE called with an empty deque" deque)
    (let ((new-tail (pre (rear-ptr deque))))
      (set-rear-ptr! deque new-tail)
      (if (null? new-tail)
        deque
        (begin (set-next! new-tail '())
               deque)))))

(define (print-deque deque)
  (display "(")
  (let ((first? #t))
    (define (print-iter deque)
      (if (empty-deque? deque)
        (display ")")
        (begin (if first?
                 (set! first? #f)
                 (display " "))
               (display (front-deque deque))
               (let ((new-deque (make-deque)))
                 (set-front-ptr! new-deque (next (front-ptr deque)))
                 (set-rear-ptr! new-deque (rear-ptr deque))
                 (print-iter new-deque)))))
    (print-iter deque))
  (newline))

