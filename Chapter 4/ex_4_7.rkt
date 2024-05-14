#lang sicp

(define (let*? exp) (eq? (car exp) 'let*))

(define (let*-bindings exp) (cadr exp))
(define (let*-body exp) (caddr exp))

(define (first-let*-binding bindings) (car bindings))
(define (remaining-let*-bindings bindings) (cdr bindings))

(define (expand-let*->nested-lets bindings body)
  (if (null? bindings)
      body
      (make-let (first-let*-binding bindings) (expand-let*->nested-lets (remaining-let*-bindings bindings) body))))

(define (let*->nested-lets exp)
  (expand-let*->nested-lets (let*-bindings exp) (let*-body exp)))