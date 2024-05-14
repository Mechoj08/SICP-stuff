#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sign n)
  (cond ((> n 0) 1)
        ((< n 0) -1)
        (else 1)))

(define (mul-interval x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y))
        (s-a (sign (lower-bound x)))
        (s-b (sign (upper-bound x)))
        (s-c (sign (lower-bound y)))
        (s-d (sign (upper-bound y))))
    (cond ((and (= s-a 1) (= s-b 1) (= s-c 1) (= s-d 1)) (make-interval (* a c) (* b d)))
          ((and (= s-a -1) (= s-b 1) (= s-c 1) (= s-d 1)) (make-interval (* a d) (* b d)))
          ((and (= s-a -1) (= s-b -1) (= s-c 1) (= s-d 1)) (make-interval (* a d) (* b c)))
          ((and (= s-a -1) (= s-b 1) (= s-c -1) (= s-d 1)) (make-interval (min (* b c) (* a d)) (max (* a c) (* b d))))
          ((and (= s-a -1) (= s-b -1) (= s-c -1) (= s-d 1)) (make-interval (* a d) (* a c)))
          ((and (= s-a -1) (= s-b -1) (= s-c -1) (= s-d -1)) (make-interval (* b d) (* a c)))
          ((and (= s-a 1) (= s-b 1) (= s-c -1) (= s-d 1)) (make-interval (* c b) (* b d)))
          ((and (= s-a 1) (= s-b 1) (= s-c -1) (= s-d -1)) (make-interval (* b c) (* a d)))
          ((and (= s-a -1) (= s-b 1) (= s-c -1) (= s-d -1)) (make-interval (* b c) (* a c)))
          )))
          

(define (div-interval x y)
  (if (?contains-zero y)
      (error "Cannot divide intervals: Denominator interval contains 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))


(define (sub-interval x y)
  (add-interval
   x
   (make-interval (* -1.0 (upper-bound y)) (* -1.0 (lower-bound y)))))

(define (?contains-zero interval)
  (let ((a (lower-bound interval))
        (b (upper-bound interval)))
    (and (< 0 b) (< a 0))))