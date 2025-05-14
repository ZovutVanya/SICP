#lang racket

(define (tree-map procedure tree)
  (cond
    ((null? tree) 
      null
    )
    ((not (pair? tree))
      (procedure tree)
    )
    (else
      (cons (tree-map procedure (car tree))
            (tree-map procedure (cdr tree)))
    )
  )
)

(define (square-tree tree)
  (tree-map sqr tree))

(square-tree
  (list 1
    (list 2 
      (list 3 4) 5)
        (list 6 7))
) ; '(1 (4 (9 16) 25) (36 49))
