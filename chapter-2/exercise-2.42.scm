#lang scheme

(require "exercise-2.40.scm")

(define empty-board null)

(define (safe? k positions)
  (let ((new-queen (car positions))
        (rest-of-queens (cdr positions))
        (row car)
        (col cadr))
    (= 0
       (length
         (filter (lambda (pre-queen)
                   (or (= (row new-queen) (row pre-queen))
                       (= (col new-queen) (col pre-queen))
                       (= (abs
                            (- (row new-queen)
                               (row pre-queen)))
                          (abs
                            (- (col new-queen)
                               (col pre-queen))))))
                 rest-of-queens)))))

(define (adjoin-position row col positions)
  (append (list (list row col))
          positions))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; test
; (queens 0)
; (queens 1)
; (queens 2)
; (queens 3)
; (queens 4)
; (length (queens 5))
; (length (queens 6))
; (length (queens 7))
; (length (queens 8))

