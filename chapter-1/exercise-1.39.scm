#lang scheme

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* 2 i) 1.0))
             k))


;;; test
; (define K 100000)
; (define (deg2rad x) (* (/ 3.1415926 180) x))

; (tan-cf (deg2rad 30) K)
; (tan (deg2rad 30))

; (tan-cf (deg2rad 45) K)
; (tan (deg2rad 45))

; (tan-cf (deg2rad 60) K)
; (tan (deg2rad 60))

