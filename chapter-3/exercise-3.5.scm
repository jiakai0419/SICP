#lang scheme

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (exact->inexact (/ trials-passed trials)))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (monte-carlo trials
                  (lambda ()
                    (let ((rx (random-in-range x1 x2))
                          (ry (random-in-range y1 y2)))
                      (P rx ry))))
     (* (- x2 x1) (- y2 y1))))

(define (estimate-pi trials)
  (estimate-integral (lambda (x y)
                       (<= (+ (sqr (- x 1))
                              (sqr (- y 1)))
                           1))
                     0 2 0 2
                     trials))

;;; test

;; center (5, 7) | radius 3 | s ~= 28.2743334
; (estimate-integral (lambda (x y)
;                      (<= (+ (sqr (- x 5))
;                             (sqr (- y 7)))
;                          9))
;                    2 8 4 10
;                    1000000)

;; pi
; (estimate-pi 10000000)

