#lang racket
(require sicp-pict)

(define (flipped-pairs painter)
  (let 
    ((painter2 (beside painter (flip-vert painter)))); flip picture upside-down and place it right(?) to the orig
    (below painter2 painter2); place two pictures2 one below other
  )
)

(define (right-split painter n)
  (cond
    ((= n 0) painter); recursion ends with the orig pic
    (else
      (let 
        ((smaller (right-split painter (- n 1)))); recursively create new right splits
        (beside painter (below smaller smaller)); and place two of them right to the previous steps
      )
    )
  )
)

(define (up-split painter n)
  (if (= n 0)
      painter
      (let 
        ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller))
      )
  )
)

(define (square-of-four tl tr bl br)
  (λ (painter)
    (let 
      (
        (top (beside (tl painter) (tr painter)))
        (bottom (beside (bl painter) (br painter)))
      )
      (below bottom top)
    )
  )
)
(define (flipped-pairs2 painter)
  (let 
    ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter))
)
(define flipped-pairs3
  (square-of-four identity flip-vert identity flip-vert)
)

(define (frame-coord-map frame)
#|returning a procedure that maps provided vectors
to their couterparts in the frame|#
  (λ (v)
    (add-vect
      (origin-frame frame)
      (add-vect
        (scale-vect (xcor-vect v) (edge1-frame frame))
        (scale-vect (ycor-vect v) (edge2-frame frame))
      )
    )
  )
)

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect vect1 vect2)
  (make-vect 
    (+ (xcor-vect vect1) (xcor-vect vect2))
    (+ (ycor-vect vect1) (ycor-vect vect2))
  )
)

(define (sub-vect vect1 vect2)
  (make-vect 
    (- (xcor-vect vect1) (xcor-vect vect2))
    (- (ycor-vect vect1) (ycor-vect vect2))
  )
)

(define (scale-vect vect1 s)
  (make-vect 
    (* (xcor-vect vect1) s)
    (* (ycor-vect vect1) s)
  )
)

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)
)
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(provide 
  flipped-pairs 
  right-split 
  up-split 
  square-of-four 
  frame-coord-map 
  add-vect 
  sub-vect 
  scale-vect
  make-frame
  origin-frame
  edge1-frame
  edge2-frame
)
