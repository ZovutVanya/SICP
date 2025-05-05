#lang racket

#| (define (append list1 list2) |#
#|   (if (null? list1) |#
#|     list2 |#
#|     (cons (car list1)  |#
#|       (append (cdr list1)  |#
#|         list2)))) |#

#|this pushes new values to the back of the list, preppending values 
instead of appending|#
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (sqr (car things))
                    answer))))
  (iter items null))

#|this places numbers instead of pairs to the end of the pair|#
(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (sqr 
                     (car things))))))
  (iter items null))

(square-list3 (list 1 2 3)) ;=> '(9 4 1)
(square-list4 (list 1 2 3)) ;=> '(((() . 1) . 4) . 9)

#| (define (square-list_corr items) |#
#|   (define (iter things answer) |#
#|     (if  |#
#|       (null? things) |#
#|       answer |#
#|       (iter (cdr things) (cons answer (list (sqr (car things))))) |#
#|     ) |#
#|   ) |#
#|   (iter items null) |#
#| ) |#
#| (square-list_corr (list 1 2 3)) |#
