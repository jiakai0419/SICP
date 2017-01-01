#lang r5rs

(#%require "queue.scm")

(define q1 (make-queue))

(print-queue
  (insert-queue! q1 'a))

(print-queue
  (insert-queue! q1 'b))

(print-queue
  (delete-queue! q1))

(print-queue
  (delete-queue! q1))

