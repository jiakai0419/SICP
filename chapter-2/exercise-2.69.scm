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

;;; generate huffman-tree
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x
               set))
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define (successive-merge nodes)
  (if (= (length nodes) 1)
    (car nodes)
    (let ((fst (car nodes))
          (snd (cadr nodes))
          (rest (cddr nodes)))
      (successive-merge (adjoin-set (make-code-tree fst snd)
                                   rest)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

