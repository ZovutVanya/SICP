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

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left  / 1 (list 1 2 3)) ; 1/6
(fold-right list null (list 1 2 3)) ; '(1 (2 (3 ())))
(fold-left  list null (list 1 2 3)) ; '(((() 1) 2) 3)

(fold-right + 0 (list 1 2 3)) ; 6
(fold-left + 0 (list 1 2 3)) ; 6

#| commutative property |#
#| (= (op x y) (op y x)) |#
