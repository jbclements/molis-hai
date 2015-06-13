#lang typed/racket

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; functions useful in building textual models

(require "huffman-wrapper.rkt"
         "huffman.rkt")

(provide (all-defined-out))

;; SPECIFIC TO STRINGS AND CHARS:

;; given a number of characters and a source text,
;; build a hash of the number of times each n-gram
;; transitions to a given letter
(: n-letter-count-hash (Natural String -> (KVCount String Char)))
(define (n-letter-count-hash n text)
  (for/fold ([h : (KVCount String Char) (hash)])
            ([i (in-range (- (string-length text) n))])
    (extend-hash h (substring text i (+ i n))
                 (string-ref text (+ i n)))))


;; crunch whitespace in a string
(: whitespace-crunch (String -> String))
(define (whitespace-crunch str)
  (regexp-replace* #px"[ \t\r\n]+" str " "))

;; GENERAL ACROSS ALL TYPES:

;; represents a map from states to a huffman tree of transitions:
(define-type (Model S T) (HashTable S (MyTree T)))
;; represents a map from states to counts of transitions
(define-type (KVCount S T) (HashTable S (HashTable T Natural)))

;; add a new sequence pair to a hash
(: extend-hash (All (K V) ((KVCount K V) K V -> (KVCount K V))))
(define (extend-hash h key value)
  (: follow-hash (HashTable V Natural))
  (define follow-hash (hash-ref h key
                                (lambda () (ann (hash) (HashTable V Natural)))))
  (define new-hash (hash-set follow-hash value
                             (add1 (hash-ref follow-hash value
                                             (lambda () 0)))))
  (hash-set h key new-hash))

;; convert a count hash to a hash table of huffman trees
(: kvcount->model (All (K V) ((KVCount K V) -> (Model K V))))
(define (kvcount->model count-hash)
  #;(for/hash : (Model K V)
    ([k : K (in-list (hash-keys count-hash))]
     (values k (count-hash->huff-tree (hash-ref count-hash k)))))
  (for/hash : (Model K V)
    ([(k v) (in-hash count-hash)])
    (values k (count-hash->huff-tree v))))

;; given a (state+transition->state) mapping
;; and a seed (markov cell) and a list of booleans and a hash
;; of huffman trees, generate a sequence of (cons thingy number),
;; where the number indicates how many bits of entropy were used for
;; each thingy.
(: generate-sequence-from-bools
   (All (S V)
        ((S V -> S) S (Listof Boolean) (Model S V) ->
                    (Listof (Pair V Natural)))))
(define (generate-sequence-from-bools rotate seed bools tree-hash)
  (let loop ([chars seed]
             [bools bools])
    (cond [(empty? bools) empty]
          [else (match-define (cons next remaining)
                  (pick-leaf (hash-ref tree-hash chars) bools))
                (cons (cons next (ensure-nonnegative
                                  (- (length bools) (length remaining))))
                      (loop (rotate chars next)
                            remaining))])))

(: ensure-nonnegative (Integer -> Nonnegative-Integer))
(define (ensure-nonnegative f)
  (cond [(< f 0) (error 'ensure-nonnegative "expected positive number, got: ~v" f)]
        [else f]))

;; given a count-hash, return a huffman tree for choosing a seed (an n-gram
;; starting with a space)
(: kvcount->seed-chooser*
   (All (K V) ((K -> Boolean) (KVCount K V) -> (MyTree K))))
(define (kvcount->seed-chooser* good-key? count-hash)
  (: key-count-hash (CountHash K))
  (define key-count-hash (kvcount->n-gram-counts count-hash))
  (: space-starters (CountHash K))
  (define space-starters (for/hash : (CountHash K)
                           ([(k v) (in-hash key-count-hash)]
                                    #:when (good-key? k))
                           (values k (ensure-nonnegative v))))
  (count-hash->huff-tree space-starters))

;; given a count-hash, compute a raw count of each n-gram, to use 
;; in picking an initial seed
(: kvcount->n-gram-counts
   (All (K V) ((KVCount K V) -> (CountHash K))))
(define (kvcount->n-gram-counts count-hash)
  (for/hash : (CountHash K)
    ([(k v) (in-hash count-hash)])
    (values k (apply + (hash-values (hash-ref count-hash k))))))

;; make a list of random booleans of the specified length
(: make-bools-list (Natural -> (Listof Boolean)))
(define (make-bools-list len)
  (for/list ([i len]) (= (random 2) 0)))

