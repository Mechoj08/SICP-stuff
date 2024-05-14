#lang sicp

(define (cont-frac n d k)
  (define (iter n d k result)
    (if (< k 2)
        (/ (n 1) (+ (d 1) result))
        (iter n d (- k 1) (/ (n k) (+ (d k) result)))))
  (iter n d k 0.0))