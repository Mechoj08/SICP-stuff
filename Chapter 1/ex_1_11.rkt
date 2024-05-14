#lang sicp

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f_alternate n)
  (define (iter n fnmin1 fnmin2 fnmin3)
    (if (< n 3)
        fnmin1
        (iter (- n 1) (+ fnmin1 (* 2 fnmin2) (* 3 fnmin3)) fnmin1 fnmin2)))
  (if (< n 3)
      n
      (iter n 2 1 0)))
  