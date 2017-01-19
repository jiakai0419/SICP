#lang planet neil/sicp

(#%provide weighted-pairs)

(#%require "stream.scm")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((a1 (stream-car s1))
                (a2 (stream-car s2)))
            (let ((w1 (weight a1))
                  (w2 (weight a2)))
              (cond ((< w1 w2) (cons-stream a1
                                            (merge-weighted (stream-cdr s1) s2 weight)))
                    ((< w2 w1) (cons-stream a2
                                            (merge-weighted s1 (stream-cdr s2) weight)))
                    (else
                      (cons-stream a1
                                   (merge-weighted (stream-cdr s1) s2 weight)))))))))


(define (weighted-pairs s t weight)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted (stream-map (lambda (x) (list (stream-car s) x))
                                           (stream-cdr t))
                               (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
                               weight)))

;; a)
(define a
  (weighted-pairs integers integers (lambda (x) (apply + x))))
; (print-stream a 10)

;; b)
(define (divide? x y) (= 0 (remainder y x)))

(define (b-pred x) (or (divide? 2 x) (divide? 3 x) (divide? 5 x)))

(define b
  (stream-filter (lambda (x)
                   (let ((i (car x))
                         (j (cadr x)))
                     (or (b-pred i)
                         (b-pred j))))
                 (weighted-pairs integers integers (lambda (x)
                                                     (let ((i (car x))
                                                           (j (cadr x)))
                                                       (+ (* 2 i) (* 3 j) (* 5 i j)))))))
; (print-stream b 10)

