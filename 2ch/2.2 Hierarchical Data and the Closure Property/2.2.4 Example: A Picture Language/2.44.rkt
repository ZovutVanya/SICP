#lang racket
(require sicp-pict)
(require "pict.rkt")

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let 
        (
          (up (up-split painter (- n 1))) 
          (right (right-split painter (- n 1)))
        )
        (let 
          (
            (top-left (beside up up)) 
            (bottom-right (below right right)) 
            (corner (corner-split painter (- n 1)))
          )
          (beside (below painter top-left) (below bottom-right corner))
        )
      )
  )
)

(define (square-limit painter n)
  (let 
    ((quarter (corner-split painter n)))
    (let 
      ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half)
    )
  )
)

(paint (corner-split einstein 4))
(paint (square-limit einstein 2))

(provide corner-split)
