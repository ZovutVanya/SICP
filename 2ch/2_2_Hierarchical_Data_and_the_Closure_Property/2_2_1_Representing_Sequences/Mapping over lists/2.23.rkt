#lang racket

(define (for-each f lst)
  (if (null? lst)
    (void)
    (begin
      (f (car lst))
      (for-each f (cdr lst))
    )
  )
)

(for-each 
  (lambda (x) (newline) (display x))
  (list 57 321 88))
