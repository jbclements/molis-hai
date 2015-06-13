#lang typed/racket/base

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; this file defines functions to create and choose from huffman trees


(require typed/rackunit
         "huffman.rkt"
         racket/match)

(provide jsexpr->huffman-tree
         huffman-tree->jsexpr
         CountHash
         Trans)

;; represents a map from states to a huffman tree of transitions:
(define-type (Trans S T) (HashTable S (MyTree T)))


(define-type (CountHash K) (HashTable K Natural))



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
