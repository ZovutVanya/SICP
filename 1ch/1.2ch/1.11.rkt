#lang racket

(define (f-rec n)
  (if (< n 3)
    n
    (+ (f-rec (- n 1))
       (* (f-rec (- n 2)) 2)
       (* (f-rec (- n 3)) 3))
  )
)

(f-rec 8)

(define (f-iter n)
  (define (f-loop n-1 n-2 n-3 nth)
    (if (= n nth)
      n-1
      (f-loop (+ n-1 (* 2 n-2) (* 3 n-3))
              n-1
              n-2
              (+ 1 nth)
      )
    )
  )
  (if (< n 3)
    n
    (f-loop 2 1 0 2)
  )
)

(f-iter 8)
