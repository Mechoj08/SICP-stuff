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

(define (disp-stream-til s n)
  (if (= n 0)
      (display-line (stream-car s))
      (begin (display-line (stream-car s))
             (disp-stream-til (stream-cdr s) (- n 1)))))

(define (display-line x) (newline) (display x))

(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

;;Exercise 3.81
(define (rand-update x)
  (remainder (+ (* 3 x) 5) 10))

(define (random request-stream seed)
  (define (gen-stream req-stream x)
    (if (eq? (stream-car req-stream) 'generate)
        (let ((new-x (rand-update x)))
          (cons-stream
           (rand-update x)
           (gen-stream (stream-cdr req-stream) (rand-update x))))
        (cons-stream
         seed
         (gen-stream (stream-cdr req-stream) seed))))
  (gen-stream request-stream seed))

;;Test-case
(define only-generate-stream
  (cons-stream 'generate only-generate-stream))