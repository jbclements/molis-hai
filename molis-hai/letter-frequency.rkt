#lang racket/base

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; this file reads a text file, generates a bunch of
;; models, and creates a bunch of passwords using them

(require json
         rackunit
         "shared-types.rkt"
         "huffman-convert.rkt"
         "char-model.rkt"
         "use-model.rkt"
         "json-read-write.rkt"
         "random-password.rkt"
         "random-bits.rkt"
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



#;(generate-string "ot" dist-2-hash)

(define ENTROPY-BITS 56)


;; given a generated, flatten it into two strings, one with the chars, one with
;; the number of bits of entropy at each choice.
#;(define (generated->string-pair g)
  (generated->sequence g (lambda (str) ))
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
  (define model  (time (build-char-model order text)))
  (model-write model (build-path "/tmp/" (format "~a-~a-model.js" abbrev order)))
  (cons (format "passwords of order ~a" order)
        (for/list ([i 8])
           ;; strip off space:
          (substring (generate-char-pwd model (random-bool-list ENTROPY-BITS)) 1))
        #;(cons
         #;(sequence->string-pair
          (generate/char model (random-bool-list ENTROPY-BITS)))
         )))


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

#;(for/list ([i 8])
  (make-pwd-str/noseed seed-tree-2 tree-hash-2))


;; *** FOUR AND HIGHER: ***

;(run 4)
;(run 5)


