#lang racket

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) 
      (append (cdr list1) 
        list2))))

(define (same-parity first . rest)
  (define parity (= 0 (remainder first 2)))
  (define (same-parity-iter rest res)
    (if 
      (null? rest)
      res
      (if
        (equal? parity (= 0 (remainder (car rest) 2)))
        (same-parity-iter (cdr rest) (append res (list (car rest))))
        (same-parity-iter (cdr rest) res)
      )
    )
  )
  (same-parity-iter rest (list first))
)

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
