#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (element-of-list? symbol seq)
  (cond ((null? seq) #f)
        ((eq? symbol (car seq)) #t)
        (else (element-of-list? symbol (cdr seq)))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-ax symbol current-branch)
    (if (leaf? current-branch)
        '()
        (let ((lb (left-branch current-branch))
              (rb (right-branch current-branch)))
          (cond ((element-of-list? symbol (symbols lb)) (cons 0 (encode-symbol-ax symbol lb)))
                ((element-of-list? symbol (symbols rb)) (cons 1 (encode-symbol-ax symbol rb)))
                (else (error "Symbol cannot be encoded using this tree"))))))
  (encode-symbol-ax symbol tree))
        
        
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge list)
  (if (null? (cdr list))
      (car list)
      (successive-merge (adjoin-set (make-code-tree (car list) (cadr list)) (cddr list)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define alphabet '((A 2) (BOOM 1) (GET 2) (JOB 2) (SHA 3) (NA 16) (WAH 1) (YIP 9)))
(define huffman-tree-alphabet (generate-huffman-tree alphabet))
(define lyrics '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
