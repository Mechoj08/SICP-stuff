#lang sicp

(define (?even n) (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-exp b n)
  (define (iter-fast-exp a b n)
    (cond ((= n 0) a)
          ((?even n) (iter-fast-exp a (square b) (/ n 2)))
          (else (iter-fast-exp  (* a b) b (- n 1)))))
  (iter-fast-exp 1 b n))