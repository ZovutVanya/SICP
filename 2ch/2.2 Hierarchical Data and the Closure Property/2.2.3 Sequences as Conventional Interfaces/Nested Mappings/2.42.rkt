#lang racket
(require "nm.rkt")

(define empty-board null)

(define (adjoin-position new-row col rest-of-queens)
  (cons (list new-row col) rest-of-queens)
)

(define (safe? k positions)
  (define row1 (caar positions))
  (define (iter rest-of-queens cols-apart)
    (or
      (null? rest-of-queens)
      (let
        ((row2 (caar rest-of-queens)))
        (and
          (not (= row1 row2))
          (not (= row1 (- row2 cols-apart)))
          (not (= row1 (+ row2 cols-apart)))
          (iter (cdr rest-of-queens) (+ 1 cols-apart))
        )
      )
    )
  )
  (iter (cdr positions) 1)
)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board); end reqursion
        (filter
          (lambda (positions) (safe? k positions)); is kth queen safe?
          (flatmap
            (lambda 
              (rest-of-queens)
              (map 
                (lambda 
                  (new-row) 
                  (adjoin-position new-row k rest-of-queens)
                )
                (enumerate-interval 1 board-size); '(1 2 3 4 5 6 7 8)
              )
            )
            (queen-cols (- k 1))
          )
        )
    )
  )
  (queen-cols board-size)
)
(queens 8)
