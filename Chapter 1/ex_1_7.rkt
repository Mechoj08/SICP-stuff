#lang sicp

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

(define (sqrt-iter guess prev_guess x)
  (if (good-enough? guess prev_guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess prev_guess)
  (< (/ (abs (- guess prev_guess)) guess) 0.00001))