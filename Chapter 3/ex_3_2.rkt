#lang sicp

(define (make-monitored proc)
  (let ((tot-calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) tot-calls)
            ((eq? x 'reset-count) (set! tot-calls 0))
            (else (begin (set! tot-calls (+ tot-calls 1)) (proc x)))))))