#lang sicp

;;;To add in the cond statement for the evaluator
((for? exp) (eval-for-loop exp env))

(define (for? exp) (tagged-list? 'for))
(define (for-iteration-parameters exp) (cadr exp))
(define (for-proc exp) (caddr exp))

(define (eval-for-loop exp env)
  (let ((par (for-iteration-parameters exp))
        (proc (for-proc exp)))
    (eval-for-loop-i (cadr par) (caddr par) proc env)))

(define (eval-for-loop-i i end-value proc env)
  (if (= i end-value)
      (proc i)
      (eval-sequence (cons (proc i env) (eval-for-loop-i (+ i 1) end-value proc env)) env)))