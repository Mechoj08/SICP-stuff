#lang sicp

(define (accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (accumulate combiner null-value term (next a) next b))
          (accumulate combiner null-value term (next a b)))))
                   