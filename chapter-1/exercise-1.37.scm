#lang scheme

(define (cont-frac-r n d k)
  (define (recu i)
    (if (= k i)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (recu (+ 1 i))))))
  (recu 1))

(define (cont-frac-i n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

;;; test

; (define cont-frac cont-frac-r)
; (define cont-frac cont-frac-i)

; (define (invoke i n)
;   (let ((ret (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) i)))
;     (newline)
;     (display i)
;     (display "  ")
;     (display ret)
;     (if (< i n)
;         (invoke (+ i 1) n)
;         (void))))

; (invoke 1 100)
