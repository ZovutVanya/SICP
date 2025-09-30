#lang racket

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

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2))
)
(define origin-frame2 car)
(define edge1-frame2 cadr)
(define edge2-frame2 cddr)

