#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch binary-mobile)
  (car binary-mobile))

(define (right-branch binary-mobile)
  (car (cdr binary-mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (weight-structure structure)
    (cond ((number? structure) structure)
          (else (total-weight structure))))

(define (total-weight binary-mobile)
  (let ((lb-struct (branch-structure (left-branch binary-mobile)))
        (rb-struct (branch-structure (right-branch binary-mobile))))
    (+ (weight-structure lb-struct) (weight-structure rb-struct))))

(define (balanced? bin-mobile)
  (define lb (left-branch bin-mobile))
  (define rb (right-branch bin-mobile))
  (define left-torque (* (branch-length (left-branch bin-mobile)) (weight-structure (branch-structure (left-branch bin-mobile)))))
  (define right-torque (* (branch-length (right-branch bin-mobile)) (weight-structure (branch-structure (right-branch bin-mobile)))))
  (and (= left-torque right-torque)
       (let ((weight-structure-left (weight-structure (branch-structure lb)))
             (weight-structure-right (weight-structure (branch-structure rb))))
         (and (if (number? weight-structure-left)
             #t
             (balanced? weight-structure-left))
         (if (number? weight-structure-right)
             #t
             (balanced? weight-structure-right))))))
          
  
(define test1 (make-mobile (make-branch 2 5) (make-branch 3 7)))
(define test2 (make-mobile (make-branch 2 3) (make-branch 2 4)))

(define test3 (make-mobile (make-branch 1 test1) (make-branch 3 test2)))

(define test4 (make-mobile (make-branch 2 5) (make-branch 2 5)))

(define test5 (make-mobile (make-branch 2 5) (make-branch 2 (make-mobile (make-branch 1 2.5) (make-branch 1 2.5)))))
