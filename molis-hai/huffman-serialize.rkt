#lang racket

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; functions for serializing and deserializing MyTree's. It
;; appears that this can't be done in Typed Racket, currently.

(require "huffman.rkt"
         json)

(provide (contract-out
          [huffman-tree->jsexpr (-> (-> any/c any) mytree? jsexpr?)]
          [jsexpr->huffman-tree (-> (-> any/c any) jsexpr? mytree?)]))

(define (mytree? m)
  (or (Branch? m)
      (Leaf? m)))

;; translate a huffman tree to a jsexpr
(define (huffman-tree->jsexpr leaf-converter tree)
  (cond [(Branch? tree)
         (hash 'a (huffman-tree->jsexpr leaf-converter (Branch-l tree))
               'b (huffman-tree->jsexpr leaf-converter (Branch-r tree)))]
        [else
         (leaf-converter (Leaf-n tree))]))

;; translate a jsexpr to a huffman tree with uniform weights of 0
(define (jsexpr->huffman-tree leaf-converter jsexpr)
  (match jsexpr
    [(hash-table ('a a) ('b b))
     (Branch 0
             (jsexpr->huffman-tree leaf-converter a)
             (jsexpr->huffman-tree leaf-converter b))]
    [other (Leaf 0 jsexpr)]))

(module+ test
  (require rackunit)

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