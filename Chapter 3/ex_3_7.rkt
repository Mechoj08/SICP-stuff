#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else #f))
        #f))
  dispatch)

(define (make-joint account password new-password)
  (define (dispatch p m)
    (if (eq? p new-password)
        (account password m)
        (lambda (x) "Incorrect password")))
  (if (account password 'withdraw)
      dispatch
      (display "Incorrect password, cannot join up account")))

(define paul-acc (make-account 100 'rosebud))