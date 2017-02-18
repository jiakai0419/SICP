; #lang planet neil/sicp

; (#%require "interpreter-beta.scm")  92726ms
; (#%require "interpreter-gamma.scm") 24125ms

; (define start (runtime))

; (eval '(define (foo n)
;          (if (= n 1)
;            1
;            (+ n (foo (- n 1))))) the-global-environment)

; (eval '(foo 10000000) the-global-environment)

; (- (runtime) start)


;;; 3.84 times -- beta / gamma
;;; analyze 74%, execute 26%

