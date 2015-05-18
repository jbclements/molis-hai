#lang typed/racket/base

;; Copyright 2015 John Clements <clements@racket-lang.org>

(require typed/rackunit
         "huffman.rkt")

(provide count-hash->huff-tree
         pick-leaf
         (struct-out Branch)
         (struct-out Leaf))



;; this file defines functions to create and choose from huffman trees

;; given a "count-hash" mapping keys to naturals, return a
;; huffman tree for choosing those keys
(: count-hash->huff-tree (All (T) ((HashTable T Natural) -> (MyTree T))))
(define (count-hash->huff-tree letterhash)
  (clump (count-hash->leafs letterhash)))

;; a decision-tree is either 
;; a (non-cons) value, or
;; (cons decision-tree decision-tree)

;; given a decision tree and a boolean generator,
;; return a leaf and the remaining bools
(: pick-leaf (All (T) ((MyTree T) (Listof Boolean) -> (Pair T (Listof Boolean)))))
(define (pick-leaf dtree bools)
  (cond [(Branch? dtree)
         ;; if we run out, just go to #t all the time:
         (cond [(null? bools)
                (pick-leaf (Branch-l dtree) null)]
               [(car bools)
                (pick-leaf (Branch-l dtree) (cdr bools))]
               [else
                (pick-leaf (Branch-r dtree) (cdr bools))])]
        [else (cons (Leaf-n dtree) bools)]))


;; given a letter-hash, convert it to a list of Leaf structures
(: count-hash->leafs (All (T) ((HashTable T Natural) -> (Listof (Leaf T)))))
(define (count-hash->leafs letterhash)
  (map (lambda: ([pr : (Pair T Natural)]) (Leaf (cdr pr) (car pr)))
       (hash->list letterhash)))




(let ()
  (define-type NumConsTree (U (cons NumConsTree NumConsTree) Number))
  
  ;; convert a cons tree of numbers into a mytree with weights zero (for testing)
  (: num-cons-tree->mytree (NumConsTree -> (MyTree Number)))
  (define (num-cons-tree->mytree t)
    (cond [(number? t) (Leaf 0 t)]
          [(pair? t) (Branch 0 (num-cons-tree->mytree (car t))
                             (num-cons-tree->mytree (cdr t)))]
          [else (error 'num-cons-tree->mytree "expected conses or numbers, given: ~v"
                       t)]))
  
  (define test-tree '(3 . ((1 . 2) . 4)))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '())
                (cons 3 '()))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#t))
                (cons 3 '()))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#t #t))
                (cons 3 '(#t)))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#f #t))
                (cons 1 '()))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#f #f))
                (cons 4 '())))

