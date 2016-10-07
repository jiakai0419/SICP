#lang scheme

(define (make-segment sp ep)
  (cons sp ep))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let ((sp (start-segment s))
        (ep (end-segment s))
        (average (lambda (a b) (/ (+ a b) 2.0))))
    (make-point (average (x-point sp) (x-point ep))
                (average (y-point sp) (y-point ep)))))

(define (print-segment s)
  (newline)
  (display "----segment start----")
  (print-point (start-segment s))
  (print-point (end-segment s))
  (newline)
  (display "----segment end----"))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;; test
; (define a-segment
;   (make-segment (make-point 2.0 1.0)
;                 (make-point 4.0 3.0)))
; (print-segment a-segment)

; (define a-point
;   (midpoint-segment a-segment))
; (print-point a-point)

