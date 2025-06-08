#lang racket
(require sicp-pict)
#|
  I do not know how to implement draw-line yet, and racket's sicp-pict provides it
  and segments->painter, make-segment, etc.
  so I comment my solutions out to avoid conflicts
|#

#| (define (segments->painter segment-list) |#
#| that constitute the image from the unit square defaults |#
#| to the corresponding coordinates in the provided frame|# |#
#| ;and draws them? |#
#|   (λ (frame) |#
#|     (for-each |#
#|       (λ (segment) |#
#|         (draw-line  |#
#|           ((frame-coord-map frame) (start-segment segment)) |#
#|           ((frame-coord-map frame) (end-segment segment)) |#
#|         ) |#
#|       ) |#
#|       segment-list |#
#|     ) |#
#|   ) |#
#| ) |#

; 2.48
;(define make-segment cons)
;(define start-segment car)
;(define end-segment cdr)


(define outline
#| The painter that draws the outline of the designated frame |#
  (segments->painter
    (list 
      (make-segment (make-vect 0 0) (make-vect 1 0))
      (make-segment (make-vect 0 1) (make-vect 1 1))
      (make-segment (make-vect 0 0) (make-vect 0 1))
      (make-segment (make-vect 1 0) (make-vect 1 1))
    )
  )
)

(define x
#| The painter that draws an “X” by connecting opposite corners of the frame |#
  (segments->painter
    (list 
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0))
    )
  )
)

(define diamond
#| The painter that draws a diamond shape by connecting the midpoints of the sides of the frame. |#
  (segments->painter
    (list 
      (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
      (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
      (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
      (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
    )
  )
)

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
(define wave (segments->painter wave-segments))
