#lang typed/racket

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; transforming a text into a model

(require "huffman-wrapper.rkt"
         "huffman.rkt")

(provide build-model)

;; SPECIFIC TO STRINGS AND CHARS:

;; run the analyses for a given "order"
(: build-model (Natural String -> (Model String Char)))
(define (build-model n text)
  (define cleaned-text
    (kill-quotes
     (whitespace-crunch
      text)))
  (: kvcount (KVCount String Char))
  (define kvcount (n-letter-kvcount n cleaned-text))
  (define tree-hash (kvcount->trans kvcount))
  (define seed-tree (kvcount->seed-chooser kvcount))
  (Model tree-hash seed-tree))

;; given a number of characters and a source text,
;; build a hash of the number of times each n-gram
;; transitions to a given letter
(: n-letter-kvcount (Natural String -> (KVCount String Char)))
(define (n-letter-kvcount n text)
  (for/fold ([h : (KVCount String Char) (hash)])
            ([i (in-range (- (string-length text) n))])
    (extend-hash h (substring text i (+ i n))
                 (string-ref text (+ i n)))))

;; given a count-hash, return a huffman tree for choosing a seed (an n-gram
;; starting with a space)
(: kvcount->seed-chooser ((KVCount String Char) -> (MyTree String)))
(define (kvcount->seed-chooser count-hash)
  (kvcount->seed-chooser* starts-with-space? count-hash))

;; does this string start with a space?
(: starts-with-space? (String -> Boolean))
(define (starts-with-space? str)
  (equal? (string-ref str 0) #\space))


;; crunch whitespace in a string
(: whitespace-crunch (String -> String))
(define (whitespace-crunch str)
  (regexp-replace* #px"[ \t\r\n]+" str " "))

;; remove all quotes from a string
(: kill-quotes (String -> String))
(define (kill-quotes str)
  (regexp-replace* #px"\"" str ""))

(module+ test
  (require typed/rackunit)
  
  (check-equal? (whitespace-crunch "  a \t\nbc de   ")
                " a bc de ")
  
  (check-equal? (kill-quotes "the \"only\" way")
                "the only way"))

;; GENERAL ACROSS ALL TYPES:

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
(: kvcount->trans (All (K V) ((KVCount K V) -> (Trans K V))))
(define (kvcount->trans count-hash)
  #;(for/hash : (Trans K V)
    ([k : K (in-list (hash-keys count-hash))]
     (values k (count-hash->huff-tree (hash-ref count-hash k)))))
  (for/hash : (Trans K V)
    ([(k v) (in-hash count-hash)])
    (values k (count-hash->huff-tree v))))

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



;; given a "count-hash" mapping keys to naturals, return a
;; huffman tree for choosing those keys
(: count-hash->huff-tree (All (T) ((CountHash T) -> (MyTree T))))
(define (count-hash->huff-tree letterhash)
  (clump (count-hash->leafs letterhash)))

;; given a letter-hash, convert it to a list of Leaf structures
(: count-hash->leafs (All (T) ((CountHash T) -> (Listof (Leaf T)))))
(define (count-hash->leafs letterhash)
  (map (lambda: ([pr : (Pair T Natural)]) (Leaf (cdr pr) (car pr)))
       (hash->list letterhash)))
