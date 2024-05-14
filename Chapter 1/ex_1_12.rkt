#lang sicp

(define (pascal n k)
  (cond ((= k 1) 1)
        ((= k n) 1)
        ((or (< k 1) (> k n)) 0)
        (else (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k)))))