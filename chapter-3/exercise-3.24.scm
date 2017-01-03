#lang r5rs

(#%require "r5rs-boost.scm")

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (cdr record)
          #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! local-table
                    (cons (cons key value)
                          (cdr local-table)))))
      'ok)
    (define (dispatch op . args)
      (cond ((eq? op 'lookup) (lookup (car args)))
            ((eq? op 'insert!) (insert! (car args) (cadr args)))
            (else
              (error "Unknow operation -- TABLE" op))))
    dispatch))

;;; test
; (define (display-ln a)
;   (display a)
;   (newline))

; (define t1 (make-table equal?))
; (t1 'insert! 1.00 'a)
; (t1 'insert! 1.00 'b)
; (t1 'insert! 2.00 'c)
; (t1 'insert! 3.00 'd)
; (display-ln (t1 'lookup 1.00))
; (display-ln (t1 'lookup 0.999))
; (display-ln (t1 'lookup 2.00))
; (display-ln (t1 'lookup 3.00))
; (newline)

; (define t2 (make-table (lambda (x y) (< (abs (- x y)) 0.01))))
; (t2 'insert! 1.00 'a)
; (t2 'insert! 1.00 'b)
; (t2 'insert! 2.00 'c)
; (t2 'insert! 3.00 'd)
; (display-ln (t2 'lookup 1.00))
; (display-ln (t2 'lookup 0.999))
; (display-ln (t2 'lookup 0.99))
; (display-ln (t2 'lookup 2.00))
; (display-ln (t2 'lookup 3.00))

