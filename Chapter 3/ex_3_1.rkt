#lang sicp

(define (make-accumulator total)
  (lambda (i)
    (begin (set! total (+ total i))) total))