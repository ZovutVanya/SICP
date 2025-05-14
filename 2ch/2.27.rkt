#lang racket
(define x 
  (list (list 1 2) (list 3 4 5)))

(display x) ; ((1 2) (3 4 5))

(define (reverse lst)
  (define (reverse-iter new-lst old-lst)
    (if 
      (null? old-lst)
      new-lst
      (reverse-iter (cons (car old-lst) new-lst) (cdr old-lst))
    )
  )
  (reverse-iter (list) lst)
)

(reverse x) ; '((3 4 5) (1 2))

(define (deep-reverse lst)
  (if
    (list? lst)
    (map deep-reverse (reverse lst))
    lst
  )
)

(deep-reverse x) ; '((5 4 3) (2 1))
