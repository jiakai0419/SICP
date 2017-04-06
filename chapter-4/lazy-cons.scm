#lang planet neil/sicp

(#%require "interpreter-delta.scm")

(define (load exp)
  (eval exp the-global-environment))

(load '(define (cons x y)
         (lambda (m) (m x y))))

(load '(define (car z)
         (z (lambda (p q) p))))

(load '(define (cdr z)
         (z (lambda (p q) q))))

