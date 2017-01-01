#lang r5rs

(#%provide make-queue empty-queue? front-queue insert-queue! delete-queue! print-queue)
(#%require "r5rs-boost.scm")

;;; queue
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (dispatch op . args)
      (cond ((eq? op 'front-ptr) front-ptr)
            ((eq? op 'rear-ptr) rear-ptr)
            ((eq? op 'set-front-ptr!) (set! front-ptr (car args)))
            ((eq? op 'set-rear-ptr!) (set! rear-ptr (car args)))
            (else
              (error "Unknow OP MAKE-QUEUE" op))))
    dispatch))

(define (front-ptr queue)
  (queue 'front-ptr))

(define (rear-ptr queue)
  (queue 'rear-ptr))

(define (set-front-ptr! queue item)
  (queue 'set-front-ptr! item))

(define (set-rear-ptr! queue item)
  (queue 'set-rear-ptr! item))

;;; queue opt
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

