#lang r5r5

;;; pseudo code
(define (ripple-carry-adder A B S C)
  (define (iter A B S cin)
    (if (null? A)
      'ok
      (let ((a (car A))
            (b (car B))
            (s (car S))
            (cout
              (if (= (length A)
                     1)
                C
                (make-wire))))
        (full-adder a b cin s cout)
        (ripple-carry-adder (cdr A)
                            (cdr B)
                            (cdr S)
                            cout))))
  (let ((cinit (make-wire)))
    (set-signal cinit 0)
    (iter (reverse A)
          (reverse B)
          (reverse S)
          cinit)))

;;; n bits ripple-carry-adder delay
;; a -- and-gate-delay
;; o -- or-gate-delay
;; i -- inverter-delay
; (* n
;    (+ o
;       a
;       (+ a
;          (max o
;               (+ a i)))))

