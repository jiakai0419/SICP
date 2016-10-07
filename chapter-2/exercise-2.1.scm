#lang scheme

(define (make-rat n d)
  (let ((g (gcd n d))
        (f (if (> d 0) 1 -1)))
    (cons (* f (/ n g))
          (* f (/ d g)))))

