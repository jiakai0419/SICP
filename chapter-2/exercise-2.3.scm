#lang scheme

;;; import point
(require "point.scm")

;;; import rectangle
(require "rectangle-focus-space.scm")
; (require "rectangle-focus-speed.scm")

(define (length rectangle)
  (- (_x (_c rectangle))
     (_x (_a rectangle))))

(define (width rectangle)
  (- (_y (_a rectangle))
     (_y (_c rectangle))))

(define (C rectangle)
  (* 2 (+ (length rectangle)
          (width rectangle))))

(define (S rectangle)
  (* (length rectangle)
     (width rectangle)))

(define (print rectangle)
  (newline)
  (display "----rectangle start----")
  (print-point (_a rectangle))
  (display "    ---- A")
  (newline)
  (print-point (_b rectangle))
  (display "    ---- B")
  (newline)
  (print-point (_c rectangle))
  (display "    ---- C")
  (newline)
  (print-point (_d rectangle))
  (display "    ---- D")
  (newline)
  (display "----rectangle end----")
  )

;;; test
; (define rectangle1
;   (make-rectangle (make-point 1.0 5.0)
;                   (make-point 8.0 2.0)))
; (print rectangle1)
; (newline)
; (C rectangle1)
; (S rectangle1)

