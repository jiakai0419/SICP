#lang scheme

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (s-next x) (+ x 1))
  (define (s-term k)
    (* (cond ((or (= 0 k) (= n k)) 1)
             ((even? k) 2)
             (else 4))
       (f (+ a (* k h)))))
  (* (/ h 3)
     (sum s-term 0 s-next n)))

(define (cube x) (* x x x))

(simpson cube 0 1 100.0)
(simpson cube 0 1 1000.0)
