#lang racket
(require "nm.rkt")

(define (unique-triples n)
  (flatmap
    (λ (i)
      (flatmap
        (λ (j)
          (map
            (λ (k) (list i j k))
            (enumerate-interval 1 (- j 1))
          )
        )
        (enumerate-interval 1 (- i 1))
      )
    )
    (enumerate-interval 1 n)
  )
)

(define (triple-sums n s)
  (filter 
    (λ (x) (= (apply + x) s)) 
    (unique-triples n)
  )
)

(triple-sums 8 10)

(unique-triples 8)
