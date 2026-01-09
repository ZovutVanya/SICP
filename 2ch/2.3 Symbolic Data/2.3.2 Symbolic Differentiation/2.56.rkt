#lang racket
(require "procedures.rkt")


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
          (make-product
            (exponent exp)
            (make-product 
              (make-exponentiation
                (base exp)
                (- (exponent exp) 1)
              )
              (deriv (base exp) var)
            )
          )
        )
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**))
)

(define (exponent e) (caddr e))

(define (base e) (cadr e))

#| (define (make-exponentiation a1 a2) (list '** a1 a2)) |#
(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? e 1) b)
    ((=number? b 1) 1)
    ((and (number? b) (number? e)) 
      (expt b e)
    )
    (else (list '** b e))
  )
)

(deriv '(** x 4) 'x)
