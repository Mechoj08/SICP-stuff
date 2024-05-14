#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((width (* c (/ p 100))))
    (make-center-width c width)))

(define (percent i)
  (let ((center-interval (center i)))
    (* 100 (/ (- (upper-bound i) center-interval) center-interval))))

(define (mul-interval-smallperc x y)
  (let ((px (percent x))
        (py (percent y))
        (center-x (center x))
        (center-y (center y)))
    (make-center-percent (* center-x center-y) (+ px py))))
        