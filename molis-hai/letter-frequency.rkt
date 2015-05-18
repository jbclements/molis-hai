#lang racket/base

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; this file reads a text file, generates a bunch of
;; models, and creates a bunch of passwords using them

(require json
         rackunit
         "bits-to-english.rkt"
         "hash-common.rkt"
         "random-password.rkt"
         racket/contract
         racket/runtime-path
         (only-in racket/file file->string))

#;(provide (contract-out
          [build-model (-> string? model?)]
          [write-model (-> model? output-port? void?)]))

;; a run-model maps a state to a huffman tree of results
(define model? (cons/c hash? any/c)
  #;(HashTable state? (HuffTree result?)))

;; a seed-tree is a huffman tree of states, used to pick
;; an initial state
(define seed-tree? any/c #;(HuffTree result?))

(define state? any/c)
(define result? any/c)


(define-runtime-path here ".")

;; given a number of characters and a source text,
;; build a hash of the number of times each n-gram
;; transitions to a given letter
(define (n-letter-count-hash n text)
  (for/fold ([h (hash)])
            ([i (in-range (- (string-length text) n))])
    (extend-hash h (substring text i (+ i n))
                 (string-ref text (+ i n)))))

;; write a jsexpr to a js file so it can be loaded in javascript
(define (write-jsexpr top-level-varname jsexpr port)
  (fprintf port "~a = \n" top-level-varname)
  (write-json jsexpr port)
  (display ";\n" port))

;; write a jsexpr to a js file so it can be loaded in js
(define (jsexpr->file top-level-varname jsexpr filename)
  (call-with-output-file filename
    #:exists 'truncate
    (lambda (port) (write-jsexpr top-level-varname jsexpr port))))

;; serialize a tree-hash to a file
(define (trees-hash->file trees-hash filename)
  (define the-jsexpr
    (for/hash ([(k v) (in-hash trees-hash)])
      (values (string->symbol k)
              (huffman-tree->jsexpr v))))
  (jsexpr->file "TextModel" the-jsexpr filename))

;; serialize a single huffman tree to a file
(define (tree->file tree filename)
  (jsexpr->file "SeedModel" (huffman-tree->jsexpr tree) filename))

;; translate a huffman tree to a jsexpr
(define (huffman-tree->jsexpr tree)
  (cond [(Branch? tree)
         (hash 'a (huffman-tree->jsexpr (Branch-l tree))
               'b (huffman-tree->jsexpr (Branch-r tree)))]
        [else
         (cond [(char? (Leaf-n tree)) (string (Leaf-n tree))]
               [(string? (Leaf-n tree)) (Leaf-n tree)]
               [else (error 'huffman-tree->jsexpr
                            "expected tree containing only strings or chars, got a tree containing: ~e\n"
                            (Leaf-n tree))])]))

(check-equal? (huffman-tree->jsexpr (Branch 13 (Branch 13 (Leaf 13 #\a)
                                                       (Leaf 13 #\b))
                                            (Leaf 13 #\c)))
              (hash 'a (hash 'a "a"
                             'b "b")
                    'b "c"))


;; crunch whitespace in a string
(define (whitespace-crunch str)
  (regexp-replace* #px"[ \t\r\n]+" str " "))

(check-equal? (whitespace-crunch "  a \t\nbc de   ")
              " a bc de ")

;; remove all quotes from a string
(define (kill-quotes str)
  (regexp-replace* #px"\"" str ""))

(check-equal? (kill-quotes "the \"only\" way")
              "the only way")

;; given a string and a character, add the char to the end and drop the first
(define (string-rotate str chr)
  (string-append (substring str 1) (string chr)))

;; given a count-hash, return a huffman tree for choosing a seed (an n-gram
;; starting with a space)
(define (count-hash->seed-chooser count-hash)
  (count-hash->seed-chooser* starts-with-space? count-hash))

;; does this string start with a space?
(define (starts-with-space? str)
  (equal? (string-ref str 0) #\space))

;; given a seed, a list of bools, and a tree-hash, generate a sequence
;; of (cons leaf bits-used)
(define (generate-char-sequence-from-bools seed bools tree-hash)
  (generate-sequence-from-bools string-rotate seed bools tree-hash))

;; given a seed tree, a list of bools and a tree-hash, generate a sequence
;; of (cons leaf bits-used)
(define (generate-char-sequence/noseed seed-huff-tree bits tree-hash)
  (define leaf-pair (pick-leaf seed-huff-tree bits))
  (define seed (car leaf-pair))
  (define bits-remaining (cdr leaf-pair))
  (define bits-used-for-seed (- (length bits) (length bits-remaining)))
  ;; strip the space off:
  (define seed-chars (string->list seed))
  (define list-head
    (cons (cons (car seed-chars) bits-used-for-seed)
          (for/list ([char (cdr seed-chars)])
            (cons char 0))))
  (append
   list-head
   (generate-char-sequence-from-bools seed bits-remaining tree-hash)))

;; given a seed (markov cell) and a list of booleans and a hash
;; of huffman trees, generate a string
(define (generate-string-from-bools seed bools tree-hash)
  (string-append
   seed
   (list->string
    (map car (generate-char-sequence-from-bools seed bools tree-hash)))))

#;(generate-string "ot" dist-2-hash)

(define ENTROPY-BITS 56)

;; generate a password string, using a seed chosen from the tree-hash
(define (make-pwd-str/noseed seed-huff-tree tree-hash)
  (sequence->string
   (generate-char-sequence/noseed seed-huff-tree (make-bools-list ENTROPY-BITS)
                                  tree-hash)))

;; generate a password string with the required number of bits of entropy
(define (make-pwd-str seed tree-hash)
  (string-append
   seed
   (sequence->string
    (generate-char-sequence-from-bools seed (make-bools-list ENTROPY-BITS)
                                       tree-hash))))

;; given a pair
(define (sequence->string-pair seq)
  (when (for/or ([count (map cdr seq)])
          (not (< count 16)))
    (raise-argument-error 'sequence->string-pair 
                          "sequence with all counts < 16"
                          seq))
  (list (list->string (map car seq))
        (apply string-append (map (lambda (n)
                                    (number->string n 16))
                                  (map cdr seq)))))

;; convert a sequence to a string
(define (sequence->string seq)
  (list->string (map car seq)))


(define-values (source-path abbrev)
  #;"/tmp/legalese.txt" 
  (values (build-path here "a-tale-of-two-cities.txt") "atotc")
  #;(values (build-path here "king-lear.txt") "kinglear")
  #;(values (build-path here "tagore.txt") "tagore")
  #;(build-path here "ascii-email-texts.txt")
  #;"/tmp/dancing-queen.txt")

(define text
  (kill-quotes
   (whitespace-crunch
    (file->string source-path))))

;; run the analyses for a given "order"
(define (build-hashes n)
  (define count-hash (n-letter-count-hash n text))
  (define tree-hash (count-hash->trees count-hash))
  (define seed-tree (count-hash->seed-chooser count-hash))
  (trees-hash->file tree-hash (build-path here
                                          (format "~a-~a-tree-hash.js"
                                                  abbrev n)))
  (tree->file seed-tree (build-path here (format "~a-~a-seed-tree.js"
                                                 abbrev n)))
  (values count-hash tree-hash seed-tree))

(define (run order)
  (define-values (count-hash tree-hash seed-tree) (build-hashes order))
  (cons (format "passwords of order ~a" order)
        (cons
         (sequence->string-pair
          (generate-char-sequence/noseed seed-tree
                                         (make-bools-list ENTROPY-BITS)
                                         tree-hash))
         (for/list ([i 8])
           ;; strip off space:
           (substring (make-pwd-str/noseed seed-tree tree-hash) 1)))))


(random-seed 2722197)

(run 1)
(run 2)
(run 3)

#;(check-equal? (generate-char-sequence-from-bools "Th" '() tree-hash-2)
              '())

;; compute the average length of a password chosen by a particular
;; scheme. Just do monte carlo...
(define (avg-len generator)
  (/ (for/sum ([i (in-range NUM-TRIALS)])
       (string-length (generator)))
     NUM-TRIALS))

(define NUM-TRIALS 10000)

#;(printf "average length of passwords using 2-grams: ~v\n"
        (+ 2 (time (avg-len (lambda () (make-pwd-str "Th" tree-hash-2))))))

#;(define-values (count-hash-2 tree-hash-2 seed-tree-2) (build-hashes 2))
#;(printf "average length of passwords using 2-grams/noseed: ~v\n"
        (sub1 
         (time (avg-len (lambda () (make-pwd-str/noseed seed-tree-2 tree-hash-2))))))

;; without seed randomization: 23.9
;; with seed randomization: 19.1 (!)

#;(+ 19 623/625)

#;(sequence->string-pair
 (generate-char-sequence-from-bools "T" (make-bools-list ENTROPY-BITS) tree-hash-1))

#;(sequence->string-pair
 (generate-char-sequence-from-bools "Th" (make-bools-list ENTROPY-BITS) tree-hash-2))

#;(for/list ([i 8])
  (make-pwd-str/noseed seed-tree-2 tree-hash-2))


;; *** FOUR AND HIGHER: ***

;(run 4)
;(run 5)


