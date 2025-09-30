#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        ((> x (entry set))
          (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

(append (list 1 2) (list 3 4)) ; '(1 2 3 4)
(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(tree->list-1 t1)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

#| Exercise 2.64 |#
#| The following procedure list->tree converts an ordered list to a balanced binary tree. The helper procedure partial-tree takes as arguments |#
#| an integer n and list of at least n elements and constructs a balanced tree containing the first n elements of the list. The result returned |#
#| by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the |#
#| tree. |#
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
  (if (= n 0)
      (cons '() elements)
      ; else
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elements left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
#|  Write a short paragraph explaining as clearly as you can how partial-tree works.

Well, it clearly breaks the list in two halves. It uses `quotient` to make sure that we a working with ints.
First count left half, then recursively use partial tree to assemble the left tree
As the result of partial tree is a pair with a tree and left-over elements, we can grab the elements we need for the right tree
The first element of unused elements is the head/root, and the rest is welded into a right tree
"In the end" assemble with `make-tree`

Draw the tree produced by list->tree for the list (1 3 5 7 9 11). |#
(list->tree '(1 3 5 7 9 11))
'(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
   #|   5 |#
   #|  / \ |#
   #| 1   9 |#
  #|  \  / \ |#
 #|   3 7  11 |#


#| What is the order of growth in the number of steps required by list->tree to convert a list of n elements?

 - Each step reduces the problem set by half but makes 2 calls so the growth is O(n) |#
