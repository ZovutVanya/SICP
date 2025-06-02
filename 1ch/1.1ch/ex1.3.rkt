#lang sicp

(define (2largest a b c)
  (cond 
    ((and (< a b) (< a c)) (cons b c))
    ((and (< b a) (< b c)) (cons a c))
    ((and (< c b) (< c a)) (cons b a))
    )
  )

(define (square x)
  (* x x))

(define (sq-sum-2l a b c)
  (define 2l (2largest a b c))
  (define x (car 2l))
  (define y (cdr 2l))
  (+ (square x) (square y))
  )

(sq-sum-2l a b c)
