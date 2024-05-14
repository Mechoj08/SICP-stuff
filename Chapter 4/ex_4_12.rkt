#lang sicp

(define (scan var val vars vals val-if-none action)
  (cond ((null? vars) val-if-none)
        ((eq? var (car vars)) (action vals))
        (else (scan (cdr vars) (cdr vals)))))

(define the-empty-environment '())

(define (env-loop env scan-proc var val action)
  (if (eq? env the-empty-environment)
      (error "Unbound variable: " var)
      (let ((frame (first-frame env)))
        (scan-proc var
                   val
                   (frame-variables frame)
                   (frame-values frame)
                   (env-loop (enclosing-environment env) scan var val action)
                   action))))

(define (lookup-variable-value var env)
  (env-loop env scan var '() car))

(define (set-variable-value! var val env)
  (env-loop env scan var val (lambda (x) (set-car! x val))))

(define (define-variable! var val env)
  ;;;Assume that environment is not empty
  (let ((frame (first-frame env)))
    (scan var val (frame-variables frame) (frame-values frame)
          (add-binding-to-frame! var val frame)
          (lambda (x) (set-car! (x) val)))))
      

                  