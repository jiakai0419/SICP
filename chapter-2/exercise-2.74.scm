#lang scheme

;;; pseudo code

(define (attach-id id file)
  (cons id file))

(define (id datum)
  (if (pair? datum)
    (car datum)
    (error "Bad datum -- ID" datum)))

(define (file datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad datum -- FILE" datum)))

;;; a)
(define (get-record datum ename)
  (let ((proc (get (id datum) 'get-record)))
    (if proc
      (proc (file datum) ename)
      (error "No mehtod for these id -- GET-RECORD" (id datum)))))

;;; b)
(define (get-salary datum ename)
  (let ((proc (get (id datum) 'get-salary)))
    (if proc
      (proc (file datum) ename)
      (error "No method for these id -- GET-SALARY" (id datum)))))

;;; c)
(define (find-employee-record datums ename)
  (if (null? datums)
    '()
    (append (get-record (car datums) ename)
            (find-emplyee-record (cdr datums) ename))))

;;; d)
;;; attach id to file, impl get-record get-salary for this id.

