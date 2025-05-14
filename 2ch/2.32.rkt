#lang racket

(define (subsets s)
  (if
    (null? s)
    (list null)
    (let
      #| subsets without the first element of the left-over set |#
      ((rest (subsets (cdr s))))
      #| add subsets that have the first element |#
      (append 
        rest 
        (map 
          (λ (subset) (cons (car s) subset))
          rest
        )
      )
    )
  )
)

(subsets '(1 2 3)
) ; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
