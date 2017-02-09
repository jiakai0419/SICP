#lang planet neil/sicp

(#%require "utils.scm")

(#%provide
 scan-out-defines)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (not-definition? exp)
  (not (definition? exp)))

(define (scan-out-defines body)
  (append (filter definition? body)
          (filter not-definition? body)))

