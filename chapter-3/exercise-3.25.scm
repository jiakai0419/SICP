#lang r5rs

(#%require "r5rs-boost.scm")

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key)
      (define (lookup-iter key table)
        (if (null? key)
          (cdr table)
          (let ((record (assoc (car key) (cdr table))))
            (if record
              (lookup-iter (cdr key) record)
              #f))))
      (lookup-iter key local-table))

    (define (insert! key value)
      (define (insert-iter! key table)
        (if (null? key)
          (set-cdr! table value)
          (let ((record (assoc (car key) (cdr table))))
            (if record
              (insert-iter! (cdr key) record)
              (let ((new-pair (cons (list (car key))
                                    (cdr table))))
                (set-cdr! table new-pair)
                (insert-iter! (cdr key) (car new-pair)))))))
      (insert-iter! key local-table)
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

; (define t1 (make-table))
; (t1 'insert! '(a) 1)
; (t1 'insert! '(c d) 3)
; (t1 'insert! '(c a) 9)
; (t1 'insert! '(b) 2)
; (t1 'insert! '(e f) 4)
; (t1 'insert! '(g h i) 5)
; (t1 'insert! '(j k l) 6)
; (t1 'insert! '(m n o p q r s t) 7)

; (display-ln (t1 'lookup '(a)))
; (display-ln (t1 'lookup '(c d)))
; (display-ln (t1 'lookup '(c a)))
; (display-ln (t1 'lookup '(b)))
; (display-ln (t1 'lookup '(e f)))
; (display-ln (t1 'lookup '(g h i)))
; (display-ln (t1 'lookup '(j k l)))
; (display-ln (t1 'lookup '(m n o p q r s t)))

; (display-ln (t1 'lookup '(c)))
; (display-ln (t1 'lookup '(x y)))
; (t1 'insert! '(m n o p q r s t) 8)
; (display-ln (t1 'lookup '(m n o p q r s t)))

