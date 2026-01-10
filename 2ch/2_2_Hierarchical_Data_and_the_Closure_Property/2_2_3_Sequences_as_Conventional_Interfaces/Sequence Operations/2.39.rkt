#lang racket

(define (fold-right operation initial sequence)
	(if (null? sequence)
		initial
		(operation 
			(car sequence)
      (fold-right operation initial (cdr sequence))
    )
	)
)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result

      (iter 
        (op result (car rest))
        (cdr rest)
      )
    )
  )
  (iter initial sequence)
)

(define (reverse1 sequence)
  (fold-right
    (lambda (x y) (append y (list x))) 
    null
    sequence
  )
)
(reverse1 (list 1 4 9 16 25)) ; '(1 4 9 16 25)

(define (reverse2 sequence)
  (fold-left
    (lambda (x y) (cons y x)) 
    null
    sequence
  )
)
(reverse2 (list 1 2 3 4 5)) ; '(1 2 3 4 5)
