#lang sicp

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment line-segment)
  (let ((x1 (x-point (start-segment line-segment)))
        (y1 (y-point (start-segment line-segment)))
        (x2 (x-point (end-segment line-segment)))
        (y2 (y-point (end-segment line-segment))))
    (make-point (/ (abs (- x1 x2)) 2) (/ (abs (- y1 y2)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")"))
