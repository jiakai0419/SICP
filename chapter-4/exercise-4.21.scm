#lang planet neil/sicp

;; a)
((lambda (n)
   ((lambda (fibo)
      (fibo fibo n))
    (lambda (fb k)
      (cond ((= k 1) 0)
            ((= k 2) 1)
            (else (+ (fb fb (- k 1))
                     (fb fb (- k 2))))))))
 8)

;; b)
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
       #t
       (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
       #f
       (ev? ev? od? (- n 1))))))

;; test

;; expected false
(f 7)
;; expected true
(f 18)

