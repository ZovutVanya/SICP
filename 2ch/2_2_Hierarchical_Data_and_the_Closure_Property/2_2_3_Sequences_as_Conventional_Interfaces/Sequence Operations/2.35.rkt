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
(define (enumerate-tree tree)
  (cond 
    ((null? tree) null)
    ((not (pair? tree)) (list tree))
    (else 
      (append 
        (enumerate-tree (car tree))
        (enumerate-tree (cdr tree))
      )
    )
  )
)


(define (count-leaves t)
  (accumulate 
    + 
    0
    (map 
      (λ (x) 1)
      (enumerate-tree t)
    )
  )
)

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x) ; 4
(count-leaves (list x x)
) ; 8
