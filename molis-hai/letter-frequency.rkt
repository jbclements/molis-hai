#lang racket/base

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; this file reads a text file, generates a bunch of
;; models, and creates a bunch of passwords using them

(require json
         rackunit
         "huffman-wrapper.rkt"
         "build-model.rkt"
         "use-model.rkt"
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
              (huffman-tree->jsexpr char->str v))))
  (jsexpr->file "TextModel" the-jsexpr filename))

;; serialize a single huffman tree to a file
(define (tree->file tree filename)
  (jsexpr->file "SeedModel" (huffman-tree->jsexpr str->str tree) filename))

(define (str->str s) s)
(define (char->str ch) (string ch))


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

(define text (file->string source-path))



(define (run order)
  (define model  (build-model order text))
  (define tree-hash (Model-trans model))
  (define seed-tree  (Model-seed-chooser model))
  (trees-hash->file tree-hash (build-path here
                                          (format "~a-~a-tree-hash.js"
                                                  abbrev order)))
  (tree->file seed-tree (build-path here (format "~a-~a-seed-tree.js"
                                                 abbrev order)))
  (cons (format "passwords of order ~a" order)
        (cons
         (sequence->string-pair
          (generate-char-sequence/noseed model (make-bools-list ENTROPY-BITS)))
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


