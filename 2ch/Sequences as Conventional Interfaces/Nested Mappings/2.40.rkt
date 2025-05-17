#lang racket
(require "nm.rkt")

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map 
        (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))
      )
    )
    (enumerate-interval 1 n))
)
(define (prime-sum-pairs n)
  (map 
    make-pair-sum 
    (filter prime-sum? (unique-pairs n))
  )
)
(prime-sum-pairs 5) ; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))
