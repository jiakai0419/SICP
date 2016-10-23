#lang scheme

(require "two-branch-mobile-a.scm")
; (require "two-branch-mobile-b.scm")

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
      (total-weight structure)
      structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balance? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile))
        (moment (lambda (branch)
                  (* (branch-weight branch)
                     (branch-length branch))))
        (branch-balance? (lambda (branch)
                           (let ((structure (branch-structure branch)))
                             (if (pair? structure)
                               (balance? structure)
                               #t)))))
    (and (branch-balance? left)
         (branch-balance? right)
         (= (moment left)
            (moment right)))))

;;; test
; (define mobile-a
;   (make-mobile (make-branch 10
;                             (make-mobile (make-branch 1
;                                                       (make-mobile (make-branch 2 5)
;                                                                    (make-branch 1 10)))
;                                          (make-branch 3 5)))
;                (make-branch 5
;                             (make-mobile (make-branch 3 10)
;                                          (make-branch 1 30)))))
; (total-weight mobile-a)
; (balance? mobile-a)

; (define mobile-b
;   (make-mobile (make-branch 10
;                             (make-mobile (make-branch 1
;                                                       (make-mobile (make-branch 2 5)
;                                                                    (make-branch 1 10)))
;                                          (make-branch 2 5)))
;                (make-branch 5
;                             (make-mobile (make-branch 3 10)
;                                          (make-branch 1 30)))))
; (total-weight mobile-b)
; (balance? mobile-b)

