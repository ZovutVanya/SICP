#lang racket

(define (equal? list1 list2)
  (cond 
    ((null? list1) (null? list2))
    ((not (pair? list1)) (eq? list1 list2))
    (else (and (pair? list2)
               (equal? (car list1) (car list2))
               (equal? (cdr list1) (cdr list2))
          )
    )
  )
)

(equal? 
  '(this is a list) 
  '(this is a list)
) ; #t

(equal? 
  '(this is a list) 
  '(this (is a) list)
) ; #f
