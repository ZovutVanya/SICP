#lang racket

(define (equal? list1 list2)
  (cond 
    ((null? list1) ; if list1 is empty
      (null? list2)) ; return list2's empty-check
    ((not (pair? list1)) ; if list1 is not a pair
      (eq? list1 list2)) ; try eq? it to list2 (if both of them are pairs, eq? returns false even if they are identical)
    (else 
      (and 
        (pair? list2) ; list1 is definetly a pair at this point, so only check list2
        (equal? (car list1) (car list2)) ; first elements are equal (possibly recursive too)
        (equal? (cdr list1) (cdr list2)) ; recursive check on the remainder
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

(eq? 
  '(this is a list) 
  '(this is a list)
) ; #f
(eq? 'yes 'yes) ; #t
