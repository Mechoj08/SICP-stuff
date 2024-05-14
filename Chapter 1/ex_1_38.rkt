#lang sicp

(define (cont-frac n d k)
  (define (iter n d k result)
    (if (< k 2)
        (/ (n 1) (+ (d 1) result))
        (iter n d (- k 1) (/ (n k) (+ (d k) result)))))
  (iter n d k 0.0))

(define (euler-seq i)
  (if (= (modulo (- i 2) 3) 0)
      (* 2 (+ (/ (- i 2) 3) 1))
      1))

(+ 2 (cont-frac (lambda (i) 1) euler-seq 25))