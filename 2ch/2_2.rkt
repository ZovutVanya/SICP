#lang racket

(define (make-segment p1 p2)
  (cons p1 p2)
)
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))


(define (make-point x y)
  (cons x y)
)
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (average2 a b)
  (/ (+ a b) 2)
)

(define (midpoint-segment seg)
  (let (
    (mid-x (average2 
      (x-point (start-segment seg))
      (x-point (end-segment seg)))
      )
    (mid-y (average2 
      (y-point (start-segment seg))
      (y-point (end-segment seg)))
      )
    )
    (make-point mid-x mid-y)
  )
)

(define (print-point p)
  #| (newline) |#
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (newline)
  (display "[")
  (print-point (start-segment s))
  (display ",")
  (print-point (end-segment s))
  (display "]")
  (newline)
)

#| testing |#
(print-point (make-point 2 3))
(define p1 (make-point 2 3))
(define p2 (make-point 5 6))
(define s1 (make-segment p1 p2))
(print-segment s1)
(print-point (midpoint-segment s1))
#| testing |#

#| rectangle |#
(define (side-len seg)
  (let 
    (
      (p1 (start-segment seg))
      (p2 (end-segment seg))
    )
    (cond
      ((= (x-point p1) (x-point p2)) (abs (- (y-point p2) (y-point p1))))
      ((= (y-point p2) (y-point p1)) (abs (- (x-point p2) (x-point p1))))
      (else (sqrt (+ 
                    (sqr (- (x-point p2) (x-point p1)))
                    (sqr (- (y-point p2) (y-point p1)))
                  )
            )
      )
    )
  )
)

#| (define (make-rectangle1 height width) |#
#|   (cons height width) |#
#| ) |#
#| (define (rect-height rect) (car rect)) |#
#| (define (rect-width rect) (cdr rect)) |#


(define (make-rectangle2 p1 p2 p3)
  (cons p1 (cons p2 p3))
)
(define (rect-height rect)
    (make-segment (car rect) (car (cdr rect)))
)
(define (rect-width rect)
    (make-segment (car rect) (cdr (cdr rect)))
)

(define (perimeter rect)
 (let
  (
   (height-len (side-len (rect-height rect)))
   (width-len (side-len (rect-width rect)))
  )
  (+
   (* 2 height-len)
   (* 2 width-len)
  )
 )
)

(define (area rect)
 (let
  (
   (height-len (side-len (rect-height rect)))
   (width-len (side-len (rect-width rect)))
  )
  (* height-len width-len)
 )
)

#| (define my-rect (make-rectangle1 |#
#|                   (make-segment (make-point 10 3) (make-point 10 7)) |#
#|                   (make-segment (make-point 10 7) (make-point 16 7)) |#
#| )) |#
(define my-rect (make-rectangle2
  (make-point 4 2)
  (make-point 2 4)
  (make-point 10 8)
))

(area my-rect)
(perimeter my-rect)


#| (define (cons x y) |#
#|   (define (dispatch m) |#
#|     (cond ((= m 0) x) |#
#|           ((= m 1) y) |#
#|           (else  |#
#|            (error "Argument not 0 or 1: |#
#|                    CONS" m)))) |#
#|   dispatch) |#
#||#
#| (define (car z) (z 0)) |#
#| (define (cdr z) (z 1)) |#
#||#
#| (define my-pair (cons 1 2)) |#
#| (display my-pair) |#
#| (car my-pair) |#
#| (cdr my-pair) |#

#| (define my-pair (cons 1 2)) |#
#| (display my-pair) |#
