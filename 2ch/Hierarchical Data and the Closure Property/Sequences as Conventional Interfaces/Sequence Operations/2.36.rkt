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

(define (accumulate-n op init seqs)
  (if 
    (null? (car seqs))
    null
    (cons 
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs))
    )
  )
)

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s)
