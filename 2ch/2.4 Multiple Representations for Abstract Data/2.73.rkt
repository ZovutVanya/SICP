#lang racket
#| 2.3.2 described a program that performs symbolic differentiation: |#

(define (deriv-orig exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [(sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))]
    [(product? exp)
     (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var) (multiplicand exp)))]
    #| ⟨more rules can be added here⟩ |#
    [else (error "unknown expression type:
                      DERIV" exp)]))
#| We can regard this program as performing a dispatch on the type
of the expression to be differentiated.
In this situation the “type tag” of the datum is the algebraic operator symbol
(such as +) and the operation being performed is deriv.
We can transform this program into data-directed style
by rewriting the basic derivative procedure as |#

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [else ((get 'deriv (operator exp)) (operands exp) var)]))

(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))

#| 1. Explain what was done above.
Why can’t we assimilate the predicates number? and variable?
into the data-directed dispatch? |#
;;number? and variable? expressions don’t use operators or operands
;;so the same generic dispatch mechanism can’t be used.

#| 2. Write the procedures for derivatives of sums and products,
and the auxiliary code required to install them in the table
used by the program above. |#
(define (install-deriv-package)
  ;; internal
  (define (sum exp var)
    (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
  (define (product exp var)
    (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var) (multiplicand exp))))
  ;; interface
  (put 'deriv '+ sum)
  (put 'deriv '* product)
  'done)

#| 3. Choose any additional differentiation rule that you like,
such as the one for exponents (Exercise 2.56),
and install it in this data-directed system. |#
(define (install-deriv-exponent-extension)
  (define (exponentiation exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                                (deriv (base exp) var))))
  (put 'deriv '** exponentiation)
  'done)

#| 4. In this simple algebraic manipulator the type of an expression
is the algebraic operator that binds it together.
Suppose, however, we indexed the procedures in the opposite way,
so that the dispatch line in deriv looked like |#

((get (operator exp) 'deriv) (operands exp) var)

#| What corresponding changes to the derivative system are required?  |#
;; swap the 'deriv and operators in the put statements in
;; the installation procedures
