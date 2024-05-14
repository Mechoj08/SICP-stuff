#lang sicp

(define (product a b term next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity n) n)

(define (inc n) (+ n 1))

(define (factorial n)
  (product 1 n identity inc))

(define (square x) (* x x))

(define (pi_product n)
  (define (4nsquare n) (* 4.0 (square n)))
  (define (pi_term n) (/ (4nsquare n) (- (4nsquare n) 1)))
  (* 2 (product 1 n pi_term inc)))