#lang sicp

(define (smoothed f)
  (lambda (x) (let ((dx 0.0001))
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define (square x) (* x x))

(define (smoothed-square x) ((smoothed square) x))