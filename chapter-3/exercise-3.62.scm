#lang r5rs

;;; pseudo

(define (div-series s1 s2)
  (let ((c (stream-car s2)))
    (if (= c 0)
      (error "div zero --DIV-SERIES")
      (scale-stream (mul-series s1
                                (reci-series (scale-stream s2 (/ 1 c))))
                    (/ 1 c)))))

(define tane-series
  (div-series sine-series cosine-series))

