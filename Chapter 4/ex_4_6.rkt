#lang sicp

(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (make-let bindings body)
  (list 'let bindings body))

(define (let-variables bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings) (let-variables (cdr bindings)))))

(define (let-values bindings)
  (if (null? bindings)
      '()
      (cons (cadar bindings) (let-values (cdr bindings)))))

(define (let->combination exp)
  (cons (make-lambda (let-variables (let-bindings exp)) (let-body exp))
                       (let-values (let-bindings exp))))