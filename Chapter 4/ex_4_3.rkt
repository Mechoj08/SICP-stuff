#lang sicp

;;Eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;Apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;; Exercise 4.2
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;;; Exercise 4.3
(define (install-eval-operations)
  ;;;Main procedures
  (define (quoted exp env) (text-of-quotation exp))
  (define (assignment exp env) (eval-assignment exp env))
  (define (definition exp env) (eval-definition exp env))
  (define (if-statement exp env) (eval-if exp env))
  (define (lambda-expr exp env) (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                            env))
  (define (begin-statement exp env) (eval-sequence (begin-actions exp) env))
  (define (cond-statement exp `env) (eval (cond->if exp) env))

  ;;; Auxiliary procedures (procedures which are there to help main procedures)
  ;;; To fill in
  
  ;;;Syntax
  ;;; To fill in 

  ;;;Interface
  (put 'quote 'exp quoted)
  (put 'set! 'exp assignment)
  (put 'lambda 'exp lambda-expr)
  (put 'if 'exp if-statement)
  (put 'define 'exp definition)
  (put 'begin 'exp begin-statement)
  (put 'cond 'exp cond-statement)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) look-up-variable-value exp env)
        ((get (car exp) 'exp) ((get (car exp) 'exp) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Expression is of unknown type -- EVAL" exp))))

