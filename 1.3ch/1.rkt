#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (sum-ints a b)
  (sum (λ (a) a) a inc b))

#| same thing! |#
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-ints 1 10)
(sum-integers 1 10)

(define (square x) (* x x))


(define (f x y)
  (let (
    (a (+ 1 (* x y ))) ;new local var
    (b (- 1 y)) ;new local var
  )
  (+
    (* x (square a))
    (* y b)
    (* a b)
  )
  )
)

#| Let allows one to bind variables as locally as possible to where they are to be used |#
(define x 5)
(+ (let ((x 3))
     (+ x (* x 10))); body of let
   x);=> 38
#|
Here, the x in the body of the let is 3, so the value of the let expression is 33
On the other hand, the x that is the second argument to the outermost + is still 5
so let is not just defining a local var, it has a body, that is evaluated
|#

#|
The defined local variables’ values are computed outside the let
This matters when the expressions that provide the values for the local variables
depend upon variables having the same names as the local variables themselves.
|#
(define x 2)
(let ((x 3)
      (y (+ x 2))); y is defined with outer x, not newly created local x
  (* x y))
; internal define can be used instead of let, but it is preferable to use it for internal procedures

(define (f g) (g 2))
(f f)
;(f 2)
;(2 2) => not a procedure 
