#lang racket
(require sicp-pict)

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

#| edge1 is ~X, edge2 is ~Y |#
#| (make-vect x y) -> vect running from origin to (x,y) |#
(define (below painter1 painter2)
  (let 
    ((split-point (make-vect 0 0.5)))
    (let 
      (
        (paint-down 
          (transform-painter 
            painter1
            (make-vect 0 0); origin
            (make-vect 1 0); edge1-horis
            split-point; edge2-vert
          )
        )
        (paint-up 
          (transform-painter
            painter2
            split-point; origin
            (make-vect 1 0.5); edge1
            (make-vect 0 1); edge2
          )
        )
      )
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)
      )
    )
  )
)
(paint (below einstein mark-of-zorro))

(define (rotate90 painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 1 1)
    (make-vect 0 0)
  )
)
(define (rotate270 painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)
  )
)
(define (below2 painter1 painter2)
  (rotate90
    (beside 
      (rotate270 painter1)
      (rotate270 painter2)
    )
  )
)
(paint (below2 einstein mark-of-zorro))
