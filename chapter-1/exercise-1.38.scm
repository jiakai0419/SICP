#lang scheme

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define e
  (+ 2 
     (cont-frac (lambda (x) 1.0)
                (lambda (x)
                  (cond ((or (< x 2) (= x 2)) x)
                        ((and (> x 2) (= 0 (remainder (- x 2) 3))) (+ 2 (* 2 (/ (- x 2) 3))))
                        (else 1)))
                100000)))

; e
