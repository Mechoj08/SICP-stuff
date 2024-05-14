#lang sicp

(define (ndivisible n base)
  (define (divisible-counter n base result)
    (if (not (eq? (remainder n base) 0))
        result
        (divisible-counter (/ n base) base (+ result 1))))
  (divisible-counter n base 0))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (ndivisible z 2))

(define (cdr z)
  (ndivisible z 3))

(define (add-points x1 x2)
  (* x1 x2))