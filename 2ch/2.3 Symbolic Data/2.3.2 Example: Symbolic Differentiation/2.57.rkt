#lang racket
(require "procedures.rkt")

#| Extend the differentiation program to handle sums  |#
#| and products of arbitrary numbers of (two or more) terms. |#

#| Try to do this by changing only the representation for  |#
#| sums and products, without changing the deriv procedure at all. |#
#| For example, the addend of a sum would be the first term, |#
#| and the augend would be the sum of the rest of the terms. |#

(define (accumulate op initial xs)
  (if (null? xs)
      initial
      (op (car xs)
          (accumulate op initial (cdr xs)))))

(define addend cadr)
(define (augend sum)
  (accumulate make-sum 0 (cddr sum)))
(define multiplier cadr)
(define (multiplicand product)
  (accumulate make-product 1 (cddr product)))

#| Then the last example above could be expressed as |#

(deriv '(* x y (+ x 3)) 'x)
