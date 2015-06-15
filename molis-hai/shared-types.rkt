#lang typed/racket/base

(require "huffman.rkt")

(provide (struct-out Model)
         CountHash
         Trans)

;; COMMON TYPES:

;; a model contains a Trans and a seed-choser
(define-struct (S T) Model ([trans : (Trans S T)] [seed-chooser : (MyTree S)]))

;; represents a map from states to a huffman tree of transitions:
(define-type (Trans S T) (HashTable S (MyTree T)))

;; a hash that counts key occurrences:
(define-type (CountHash K) (HashTable K Natural))