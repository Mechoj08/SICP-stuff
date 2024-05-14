#lang sicp

(define (fringe items)
  (cond ((null? items) '())
        ((pair? (car items)) (append (fringe (car items)) (fringe (cdr items))))
        (else (cons (car items) (fringe (cdr items))))))

(define (fringe2 items)
  (cond ((null? items) '())
        (else
         (let ((first-item (car items))
               (rem-items (cdr items)))
           (cond ((pair? first-item) (append (fringe2 first-item) (fringe2 rem-items)))
                 (else (cons first-item (fringe2 rem-items))))))))
