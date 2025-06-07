#lang racket
(require sicp-pict)

(define wave-segments
  (list 
    (make-segment (make-vect 0.46 0.00) (make-vect 0.37 0.22))
    (make-segment (make-vect 0.37 0.22) (make-vect 0.46 0.34))
    (make-segment (make-vect 0.46 0.34) (make-vect 0.37 0.33))
    (make-segment (make-vect 0.37 0.33) (make-vect 0.22 0.45))
    (make-segment (make-vect 0.22 0.45) (make-vect 0.00 0.28))
    (make-segment (make-vect 0.00 0.33) (make-vect 0.22 0.55))
    (make-segment (make-vect 0.22 0.55) (make-vect 0.39 0.42))
    (make-segment (make-vect 0.39 0.42) (make-vect 0.31 1.00))
    (make-segment (make-vect 0.54 0.00) (make-vect 0.63 0.22))
    (make-segment (make-vect 0.63 0.22) (make-vect 0.54 0.34))
    (make-segment (make-vect 0.54 0.34) (make-vect 0.63 0.33))
    (make-segment (make-vect 0.63 0.33) (make-vect 1.00 0.67))
    (make-segment (make-vect 1.00 0.72) (make-vect 0.61 0.42))
    (make-segment (make-vect 0.61 0.42) (make-vect 0.69 1.00))
    (make-segment (make-vect 0.39 1.00) (make-vect 0.50 0.68))
    (make-segment (make-vect 0.50 0.68) (make-vect 0.61 1.00))
  )
)
#| adding a smile |#
(define smile-segments
  (list 
    (make-segment (make-vect 0.46 0.13) (make-vect 0.46 0.17))
    (make-segment (make-vect 0.46 0.24) (make-vect 0.50 0.27))
    (make-segment (make-vect 0.54 0.13) (make-vect 0.54 0.17))
    (make-segment (make-vect 0.54 0.24) (make-vect 0.50 0.27))
  )
)
(define wave (segments->painter (append wave-segments smile-segments)))

#| make corner-split use only one copy of the up-split and right-split images instead of two |#
(define (split comb split-comb)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let 
          ((smaller (splitter painter (- n 1))))
          (comb painter (split-comb smaller smaller))
        )
    )
  )
  splitter
)
(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let 
        (
          (up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))
          (corner (corner-split painter (- n 1)))
        )
        (beside (below painter up) (below right corner))
      )
  )
)

#| make square-limit orient the corners differently |#
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let 
      (
        (top (beside (tl painter) (tr painter)))
        (bottom (beside (bl painter) br painter))
      )
      (below bottom top))
  )
)

(define (square-limit painter n)
  (let 
    ((quarter (corner-split painter n)))
    (let 
      ((flipped (flip-horiz quarter)))
      (square-of-four flipped quarter flipped quarter)
    )
  )
)
