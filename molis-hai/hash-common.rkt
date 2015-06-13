#lang typed/racket

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; functions useful in building textual models

(require "huffman-wrapper.rkt")

(provide (all-defined-out))

(define-type (CountHash K V) (HashTable K (HashTable V Natural)))

;; add a new sequence pair to a hash
(: extend-hash (All (K V) ((CountHash K V) K V -> (CountHash K V))))
(define (extend-hash h key value)
  (define follow-hash (hash-ref h key
                                (lambda () (hash))))
  (define new-hash (hash-set follow-hash value
                             (add1 (hash-ref follow-hash value
                                             (lambda () 0)))))
  (hash-set h key new-hash))

;; convert a count hash to a distribution hash
#;(define (count-hash->dist count-hash)
  (for/hash ([k (in-hash-keys count-hash)])
    (define sub-hash-list 
      (hash->list (hash-ref count-hash k)))
    (values k (discrete-dist (map car sub-hash-list)
                             (map cdr sub-hash-list)))))

;; convert a count hash to a hash table of huffman trees
(: count-hash->trees (All (K V) ((CountHash K V) -> (HashTable K (MyTree V)))))
(define (count-hash->trees count-hash)
  (for/hash ([(k v) (in-hash count-hash)])
    (values k (count-hash->huff-tree v))))

;; given a (state+transition->state) mapping
;; and a seed (markov cell) and a list of booleans and a hash
;; of huffman trees, generate a sequence of (cons thingy number),
;; where the number indicates how many bits of entropy were used for
;; each thingy.
(: generate-sequence-from-bools
   (All (S V)
        ((S V -> S) S (Listof Boolean) (HashTable K (MyTree V)) -> (Listof V))))
(define (generate-sequence-from-bools rotate seed bools tree-hash)
  (let loop ([chars seed]
             [bools bools])
    (cond [(empty? bools) empty]
          [else (match-define (cons next remaining)
                  (pick-leaf (hash-ref tree-hash chars) bools))
                (cons (cons next (- (length bools) (length remaining)))
                      (loop (rotate chars next)
                            remaining))])))


;; given a count-hash, return a huffman tree for choosing a seed (an n-gram
;; starting with a space)
(: count-hash->seed-chooser*
   (All (K) ((K -> Boolean) (CountHash K V) -> (MyTree K))))
(define (count-hash->seed-chooser* starts-with-space? count-hash)
  (define key-count-hash (count-hash->n-gram-counts count-hash))
  (define space-starters (for/hash ([(k v) (in-hash key-count-hash)]
                                    #:when (starts-with-space? k))
                           (values k v)))
  (count-hash->huff-tree space-starters))

;; given a count-hash, compute a raw count of each n-gram, to use 
;; in picking an initial seed
(: count-hash->n-gram-counts
   (All (K V) ((CountHash K V) (HashTable K Natural))))
(define (count-hash->n-gram-counts count-hash)
  (for/hash ([(k v) (in-hash count-hash)])
    (values k (apply + (hash-values (hash-ref count-hash k))))))

;; make a list of random booleans of the specified length
(define (make-bools-list len)
  (for/list ([i len]) (= (random 2) 0)))

