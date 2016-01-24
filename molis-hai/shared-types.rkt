#lang typed/racket/base

(require "huffman.rkt")

(provide (struct-out Model)
         CountHash
         Trans)

;; COMMON TYPES:

;; a (model S T) is a model with states S and transitions T.
;; it contains a Trans and a seed-choser
(define-struct (S T) Model ([trans : (Trans S T)] [seed-chooser : (MyTree S)]))

;; represents a map from states S to a huffman tree of transitions T:
(define-type (Trans S T) (HashTable S (MyTree T)))

;; a hash that counts key occurrences:
(define-type (CountHash K) (HashTable K Natural))