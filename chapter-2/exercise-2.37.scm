#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    null
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate +
              0
              (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w)) 
       m))
;;; test
; (matrix-*-vector (list (list 1 1 3 1)
;                        (list 2 3 4 2)
;                        (list 5 7 2 4))
;                  (list 1 2 3 4))

(define (transpose m)
  (accumulate-n cons null m))
;;; test
; (transpose (list (list 1 2)
;                  (list 3 4)
;                  (list 5 6)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rows)
           (matrix-*-vector cols rows)) 
         m)))
;;; test
; (matrix-*-matrix (list (list 1 2 3)
;                        (list 4 5 6))
;                  (list (list 7 8)
;                        (list 9 10)
;                        (list 11 12)))

