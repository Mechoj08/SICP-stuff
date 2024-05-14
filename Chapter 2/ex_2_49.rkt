#lang sicp

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define origin (make-vector 0 0))
(define lower-right (make-vector 1 0))
(define upper-right (make-vector 1 1))
(define upper-left (make-vector 0 1))

(define outline-painter
  (let ((seg1 (make-segment origin lower-right))
        (seg2 (make-segment lower-right upper-right))
        (seg3 (make-segment upper-right upper-left))
        (seg4 (make-segment upper-left origin)))
    (segments->painter (list seg1 seg2 seg3 seg4))))

(define x-painter
  (let ((seg1 (make-segment origin upper-right))
        (seg2 (make-segment upper-left lower-right)))
    (segments->painter (list seg1 seg2))))

