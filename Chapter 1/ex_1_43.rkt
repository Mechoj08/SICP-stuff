#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (< n 2)
      (lambda (x) (f x))
      (lambda (x) ((compose (repeated f (- n 1)) f) x))))

(define (square x) (* x x))