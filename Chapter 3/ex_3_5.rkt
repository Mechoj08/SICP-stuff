#lang sicp

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (monte-carlo trials (region-test P x1 x2 y1 y2)) (- x2 x1) (- y2 y1)))

(define (region-test P x1 x2 y1 y2)
  (lambda () (let ((x-rand (random-in-range x1 x2))
        (y-rand (random-in-range y1 y2)))
    (P x-rand y-rand))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;Unit circle predicate
(define (square x) (* x x))

(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

;;Unit-circle computation
(define (unit-circle-n n) (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 n))