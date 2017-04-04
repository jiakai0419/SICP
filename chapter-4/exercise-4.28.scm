(define (g x) (+ x 1))
(define (f g x) (g x))
(f g 1023)
