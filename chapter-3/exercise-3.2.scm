#lang scheme

(define (make-monitored f)
  (let ((c 0))
    (lambda (x)
      (cond ((eq? 'how-many-calls? x) c)
            ((eq? 'reset-count x) (begin (set! c 0)
                                         c))
            (else (begin (set! c (+ c 1))
                         (f x)))))))

;;; test
; (define s (make-monitored sqrt))
; (s 100)
; (s 'how-many-calls?)
; (s 81)
; (s 'how-many-calls?)
; (s 'reset-count)
; (s 'how-many-calls?)

