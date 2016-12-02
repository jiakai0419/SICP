#lang scheme

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records)
         (error "not found given-key:" given-key))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((< (key (entry set-of-records)) given-key)
         (lookup give-key (right-branch set-of-records)))
        (else
          (entry set-of-records))))

