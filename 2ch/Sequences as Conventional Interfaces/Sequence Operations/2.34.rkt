#lang racket

(define (accumulate operation initial sequence)
	(if (null? sequence)
		initial
		(operation 
			(car sequence)
      (accumulate operation initial (cdr sequence))
    )
	)
)

(define (horner-eval x coefficient-sequence)
  (accumulate 
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* higher-terms x))
    )
    0
    coefficient-sequence
  )
)

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79
