#lang racket
(require cadnr)

(car (cdr (car (cddr '(1 3 (5 7) 9))))) ; 7
(cadaddr '(1 3 (5 7) 9)) ; 7

(caar '((7))) ; 7

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7)))))))))))))))))) ; 7
(cadadadadadadr '(1 (2 (3 (4 (5 (6 7))))))) ; 7
