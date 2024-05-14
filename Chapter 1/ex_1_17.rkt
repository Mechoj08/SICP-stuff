#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (halve b)))
        (else (+ (mult a (- b 1)) a))))

(define (double x) (+ x x))

(define (halve x) (/ x 2))