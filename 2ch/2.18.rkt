#lang racket

(define lst (list 1 4 9 16 25))

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

(reverse lst)
