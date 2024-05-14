#lang sicp

(define (cond-predicate clause) (car clause))

(define (cond-actions clause)
  (if (eq? cadr '=>)
      (apply (caddr clause) (car clause))
      (cdr clause)))
