#lang scheme

(define l1 (list 1 3 (list 5 7) 9))
l1
(car (cdaddr l1))

(define l2 (list (list 7)))
l2
(caar l2)

(define l3 (list 1
                 (list 2
                       (list 3
                             (list 4
                                   (list 5
                                         (list 6 7)))))))
l3
(cadadr (cadadr (cadadr l3)))

