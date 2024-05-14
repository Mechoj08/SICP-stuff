#lang sicp
(define (sign n)
  (cond ((negative? n) -1)
        ((positive? n) 1)
        (else 1)))

(define (make-rat n d)
  (let ((g (gcd n d))
        (rat-sign (* (sign n) (sign d))))
    (cons (* rat-sign (/ (abs n) g)) (/ (abs d) g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)