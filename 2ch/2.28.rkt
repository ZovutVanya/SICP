#lang racket

(define x 
  (list (list 1 2) (list 3 4)))

(display x) ; ((1 2) (3 4))

(define (fringe lst)
  (cond
    ((null? lst) lst)
    (
      (pair? (car lst))
      (append
        (fringe (car lst))
        (fringe (cdr lst))
      )
    )
    (else
      (cons (car lst) (fringe (cdr lst)))
    )
  )
)

(fringe x) ; '(1 2 3 4)

(fringe (list x x)) ; '(1 2 3 4 1 2 3 4)
