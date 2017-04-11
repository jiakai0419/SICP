#lang swindle

(define (require p)
  (if (not p)
    (amb)))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
        ; (require (distinct? (list baker cooper fletcher miller smith)))
        (require (not (= baker 5)))
        (require (not (= cooper 1)))
        (require (not (= fletcher 1)))
        (require (not (= fletcher 5)))
        (require (> miller cooper))
        (require (not (= (abs (- smith fletcher)) 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (require (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))

(define (solutions)
  (amb-collect (multiple-dwelling)))

;;; test
; (define (try f n)
;   (if (> n 0)
;     (begin (f)
;            (try f (- n 1)))))

; (try solutions 1000)

;;; console
;; old
; racket exercise-4.39.scm  10.11s user 0.25s system 99% cpu 10.369 total
; racket exercise-4.39.scm  10.06s user 0.25s system 99% cpu 10.318 total
;; new
; racket exercise-4.39.scm  9.56s user 0.25s system 99% cpu 9.808 total
; racket exercise-4.39.scm  9.39s user 0.24s system 99% cpu 9.633 total

