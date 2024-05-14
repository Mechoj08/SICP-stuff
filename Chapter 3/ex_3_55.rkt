#lang sicp

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

;;Some standard passes
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

(define (add-streams s1 s2) (stream-map + s1 s2))

;;Code for exercise 3.55
(define (partial-sums s)
  (cons-stream 0 (add-streams (partial-sums s) s)))

;;Test case
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define a (partial-sums ones))
(define b (partial-sums integers))