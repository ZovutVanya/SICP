#lang racket
(require (rename-in "sets_unordered_lists.rkt" [element-of-set? is-in-symbols?]))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
    [(= bit 0) (left-branch branch)]
    [(= bit 1) (right-branch branch)]
    [else (error "bad bit:
               CHOOSE-BRANCH" bit)]))

(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(< (weight x) (weight (car set))) (cons x set)]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))
(make-leaf-set '((A 4) (B 2) (C 1) (D 1))) ; '((leaf D 1) (leaf A 4) (leaf B 2) (leaf C 1))

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))
(display
 sample-tree) ; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; '(A D A B B C A)

#| Exercise 2.68:
The encode procedure takes as arguments a message and a tree
and produces the list of bits that gives the encoded message. |#
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

#| Encode-symbol is a procedure, which you must write, that
returns the list of bits that encodes a given symbol according to
a given tree. You should design encode-symbol so that it signals
an error if the symbol is not in the tree at all. Test your
procedure by encoding the result you obtained in Exercise 2.67
with the sample tree and seeing whether it is the same as the
original sample message.  |#

(define (encode-symbol symbol tree)
  (cond
    [(leaf? tree) '()]
    [(is-in-symbols? symbol (symbols (left-branch tree)))
     (cons 0 (encode-symbol symbol (left-branch tree)))]
    [(is-in-symbols? symbol (symbols (right-branch tree)))
     (cons 1 (encode-symbol symbol (right-branch tree)))]
    [else (error 'encode-symbol "symbol not in tree" symbol)]))

(encode '(A D A B B C A) sample-tree) ; '(0 1 1 0 0 1 0 1 0 1 1 1 0)

#| Exercise 2.69:  |#
#| The following procedure takes as its argument a |#
#| list of symbol-frequency pairs (where no symbol appears in more |#
#| than one pair) and generates a Huffman encoding tree according to |#
#| the Huffman algorithm. |#
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

#| Make-leaf-set is the procedure given above that transforms the |#
#| list of pairs into an ordered set of leaves. Successive-merge is |#
#| the procedure you must write, using make-code-tree to |#
#| successively merge the smallest-weight elements of the set until |#
#| there is only one element left, which is the desired Huffman |#
#| tree. (This procedure is slightly tricky, but not really |#
#| complicated. If you find yourself designing a complex procedure, |#
#| then you are almost certainly doing something wrong. You can take |#
#| significant advantage of the fact that we are using an ordered |#
#| set representation.) |#

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set)))))

(define abcd-tree (generate-huffman-tree '((A 5) (B 10) (C 2) (D 1))))
(encode-symbol 'A abcd-tree) ; '(1 0 1)
(encode-symbol 'B abcd-tree) ; '(1 1)
(encode-symbol 'C abcd-tree) ; '(0)
(encode-symbol 'D abcd-tree) ; '(1 0 0)
(successive-merge (make-leaf-set '((A 5) (B 10) (C 2) (D 1))))
; '((leaf C 2)
;   (((leaf D 1) (leaf A 5) (D A) 6) (leaf B 10) (D A B) 16)
;   (C D A B)
;   18)
