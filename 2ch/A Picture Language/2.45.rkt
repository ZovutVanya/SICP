#lang racket
(require sicp-pict)
(provide split)

(define (split p1 p2)
  (λ (painter n)
    (if (= n 0) painter
    ;else
      (let
        ((smaller ((split p1 p2) painter (- n 1))))

        (p1 painter (p2 smaller smaller))
      )
    )
  )
)
(define right-split2 (split beside below))
(define up-split2 (split below beside))

(paint (right-split2 einstein 2))
(paint (up-split2 einstein 2))
