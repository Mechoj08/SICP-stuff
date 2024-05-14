#lang sicp

(define (sqrt-iter guess prev_guess x)
  (new-if (good-enough? guess prev_guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess prev_guess)
  (< (/ (abs (- guess prev_guess)) guess) 0.00001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))