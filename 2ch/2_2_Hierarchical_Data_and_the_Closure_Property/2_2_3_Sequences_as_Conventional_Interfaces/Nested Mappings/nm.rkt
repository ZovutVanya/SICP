#lang racket
(provide accumulate enumerate-interval flatmap make-pair-sum prime-sum?)

(define (accumulate operation initial sequence)
	(if (null? sequence)
		initial
		(operation
			(car sequence)
      (accumulate operation initial (cdr sequence))
    )
	)
)

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons
        low
        (enumerate-interval (+ low 1) high)
      )
  )
)

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (make-pair-sum pair)
  (list
    (car pair)
    (cadr pair)
    (+ (car pair) (cadr pair))
  )
)

(require math/number-theory)
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

