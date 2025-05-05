#lang racket


((λ (x) (* x x)) 5) ;=> 25

(λ (x) (* x x)) ;=> #<procedure>

(define square (λ (x) (* x x)))
(square 5)

(define (square2 x) (* x x))
(square2 5)

(define fact
  (λ (n)
    (if (= n 1)
      1
      (* n (fact (- n 1))))))

