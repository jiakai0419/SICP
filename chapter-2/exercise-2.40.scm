#lang scheme

(require "../chapter-1/exercise-1.23.scm")

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append
              null
              (map proc seq)))

(define (enumerate-interval a b)
  (if (<= a b)
    (cons a
          (enumerate-interval (+ a 1) b))
    null))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair)
             (cadr pair))))

(define (make-pair-sum pair)
  (let ((a (car pair))
        (b (cadr pair)))
    (list a b (+ a b))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;; test
; (prime-sum-pairs 6)

