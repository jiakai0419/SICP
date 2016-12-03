#lang scheme

;;; leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define symbol-leaf cadr)

(define weight-leaf caddr)

;;; tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define left-branch car)

(define right-branch cadr)

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

;;; decode
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;;; encode
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (elem? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (elem? x (cdr set)))))

(define (encode-symbol c tree)
  (if (leaf? tree)
    '()
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond ((elem? c (symbols left)) (cons 0
                                            (encode-symbol c left)))
            ((elem? c (symbols right)) (cons 1
                                             (encode-symbol c right)))
            (else (error "bad symbol -- ENCODE-SYMBOL" c))))))

;;; test
; (define sample-tree
;   (make-code-tree (make-leaf 'A 4)
;                   (make-code-tree (make-leaf 'B 2)
;                                   (make-code-tree (make-leaf 'D 1)
;                                                   (make-leaf 'C 1)))))
; (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) sample-tree)
; (encode '(A D A B B C A) sample-tree)

