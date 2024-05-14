#lang sicp

(define rand (let ((x 3))
                (lambda ()
                  (set! x (rand-update x))
                  x)))

(define (make-rand)
  (define x-init 3)
  (define (generate-random-number) (set! x-init (rand-update x-init)) x-init)
  (define (reset-init reset-value) (set! x-init reset-value))
  (define (dispatch message)
    (cond ((eq? message 'generate) (generate-random-number))
          ((eq? message 'reset) (lambda (reset-val) (reset-init reset-val)))
          (else (error "No such method: MAKE-RAND" message))))
  dispatch)

(define (rand-update x)
  (remainder (+ (* 3 x) 5) 10))