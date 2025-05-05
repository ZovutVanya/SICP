#lang racket
(require threading)

(define (sstl lst)
  (~> lst
      (sort >)
      (take 2)
      (map (λ (x) (* x x)) _)
      (foldl + 0 _)))

(define 3nums (list 1 2 3))

(sstl 3nums)
