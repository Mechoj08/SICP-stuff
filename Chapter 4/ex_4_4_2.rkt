#lang sicp

(define (and? exp)
  (tagged-list exp 'and))

(define (or exp)
  (tagged-list exp 'or))

(define (and-clauses exp)
  (cdr exp))

(define (or-clauses exp)
  (cdr exp))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 (expand-and-clauses rest)
                 'true))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 'true
                 (expand-or-clauses rest)))))