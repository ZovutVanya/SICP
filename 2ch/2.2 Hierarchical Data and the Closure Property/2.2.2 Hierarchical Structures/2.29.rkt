#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile)
)
(define (right-branch mobile)
  (cadr mobile)
)

(define (branch-length branch)
  (car branch)
)
(define (branch-structure branch)
  (cadr branch)
)

#|
If make-mobile and make-branch use cons instead of list, all we need to do is change the right-branch and branch-structure selectors:

(define make-mobile cons)
(define make-branch cons)

(define right-branch cdr)
(define branch-structure cdr)
|#

(define (total-weight mobile)
  (+
    (branch-weight (left-branch mobile))
    (branch-weight (right-branch mobile))
  )
)

(define (branch-weight branch)
  (let
    ((struct (branch-structure branch)))
    (if (number? struct)
        struct
        (total-weight struct)
    )
  )
)

(define (torque branch)
  (*
    (branch-length branch)
    (branch-weight branch)
  )
)

(define (branch-balanced? branch)
  (let
    ((struct (branch-structure branch)))
    (or
      (number? struct)
      (mobile-balanced? struct)
    )
  )
)

(define (mobile-balanced? mobile)
  (and
    (=
      (torque (left-branch mobile))
      (torque (right-branch mobile))
    )
    (branch-balanced? (left-branch mobile))
    (branch-balanced? (right-branch mobile))
  )
)
