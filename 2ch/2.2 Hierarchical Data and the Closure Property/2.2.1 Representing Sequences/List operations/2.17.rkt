#lang racket

(define lst (list 23 72 149 34 69))

(define (last-pair lst)
  (if 
    (null? (cdr lst))
    (car lst)
    (last-pair (cdr lst))
  )
)

(last-pair lst)
