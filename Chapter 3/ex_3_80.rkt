#lang sicp

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

;;Some standard maps
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

;;Integration
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)

;;Exercise 3.80
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define diL
      (add-streams (scale-stream vC (/ 1 L)) (scale-stream iL (/ (* -1 R) L))))
    (define dvC
      (scale-stream iL (/ -1 C)))
    (define vC
      (integral (delay dvC) vC0 dt))
    (define iL
      (integral (delay diL) iL0 dt))
    (cons iL vC)))
     