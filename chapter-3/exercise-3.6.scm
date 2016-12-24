#lang scheme

(define (rand-update x)
  (let ((a 1103515245)
        (b 12345)
        (m 2147483647))
    (modulo (+ (* a x)
               b)
            m)))

(define rand
  (let ((x 1))
    (lambda (op . new-value)
      (cond ((eq? op 'generate) (set! x (rand-update x))
                                x)
            ((eq? op 'reset) (set! x (car new-value))
                             x)
            (else
              (error "Unknow op --RAND" op))))))

;;; test
(rand 'generate)
(rand 'generate)
(rand 'generate)

(rand 'reset 1024)
(rand 'generate)
(rand 'generate)

(rand 'reset 1024)
(rand 'generate)
(rand 'generate)

