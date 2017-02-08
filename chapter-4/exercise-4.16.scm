;;; test

((lambda (x)
   (define (s x)
     (* x x))
   (display (c x))
   (newline)
   (define (c x)
     (* x x x))
   (display (s x))
   (newline)
   'done)
 2)

