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

(define (dot-product v w)
  (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
  (map (λ (x) (dot-product x v)) m)
)

(define (transpose mat)
  (accumulate-n cons null mat)
)
(define matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(transpose matrix) ; '((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define (matrix-*-matrix m n)
  (let 
    ((cols (transpose n)))
    (map 
      (λ (x) 
        (map 
          (λ (y) dot-product x y) 
          cols
        )) 
      m
    )
  )
)
