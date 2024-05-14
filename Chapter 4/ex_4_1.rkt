#lang sicp

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((term-1 (eval (first-operand exps) env)))
        (cons term-1 (list-of-values (rest-operands exps) env)))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((remaining-terms (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) remaining-terms))))