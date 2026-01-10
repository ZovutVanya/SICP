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

(define (map p sequence)
  (accumulate
    (λ (x y) (cons (p x) y))
    null
    sequence
  )
)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate
    (λ (x y) (+ 1 y)); (car sequence) is not used, just add 1 and go on with accumulation
    0
    sequence
  )
)

(length (list 1 2 3)) ; 3
