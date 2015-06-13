#lang typed/racket/base

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; this file defines functions to create and choose from huffman trees


(require typed/rackunit
         "huffman.rkt"
         racket/match)

(provide count-hash->huff-tree
         pick-leaf
         jsexpr->huffman-tree
         huffman-tree->jsexpr
         #;(struct-out Branch)
         #;(struct-out Leaf)
         CountHash)

(define-type (CountHash K) (HashTable K Natural))

;; given a "count-hash" mapping keys to naturals, return a
;; huffman tree for choosing those keys
(: count-hash->huff-tree (All (T) ((CountHash T) -> (MyTree T))))
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


(define-type (HuffJSexpr T)
  (U T (HashTable Symbol (HuffJSexpr T))))

(: huffman-tree->jsexpr (All (T U) ((T -> U) (MyTree T) -> (HuffJSexpr U))))
(define (huffman-tree->jsexpr leaf-converter tree)
  (cond [(Branch? tree)
         (hash 'a (huffman-tree->jsexpr leaf-converter (Branch-l tree))
               'b (huffman-tree->jsexpr leaf-converter (Branch-r tree)))]
        [else
         (leaf-converter (Leaf-n tree))]))

;; can't use (HuffJSexpr T) as input, because if-splitting can't distinguish the
;; types for an arbitrary T.
(: jsexpr->huffman-tree (All (T) ((Any -> T) Any -> (MyTree T))))
(define (jsexpr->huffman-tree leaf-converter jsexpr)
  (cond [(hash? jsexpr)
         (match (hash-keys jsexpr)
           [(list-no-order 'a 'b)
            (Branch 0
                    (jsexpr->huffman-tree leaf-converter (hash-ref jsexpr 'a))
                    (jsexpr->huffman-tree leaf-converter (hash-ref jsexpr 'b)))]
           [other  (error 'jsexpr->huffman-tree
                          "expected hash table with keys 'a and 'b")])]
        [else (Leaf 0 (leaf-converter jsexpr))]))

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
                (cons 4 '()))

    (define (leaf-converter s) s)
  
  (check-equal?
   (huffman-tree->jsexpr leaf-converter
                         (Branch 13 (Branch 13 (Leaf 13 "a")
                                            (Leaf 13 "b"))
                                 (Leaf 13 "c")))
   (hash 'a (hash 'a "a"
                  'b "b")
         'b "c"))

  (check-equal?
   (jsexpr->huffman-tree leaf-converter
                         (hash 'a (hash 'a "a"
                                        'b "b")
                               'b "c"))
   
   (Branch 0 (Branch 0 (Leaf 0 "a")
                      (Leaf 0 "b"))
           (Leaf 0 "c"))))
