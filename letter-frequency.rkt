#lang racket

(require json
         rackunit
         "bits-to-english.rkt"
         "hash-common.rkt"
         shelly/random-password
         racket/runtime-path)


(define-runtime-path here ".")

;; given a number of characters and a source text,
;; build a hash of the number of times each n-gram
;; transitions to a given letter
(define (n-letter-count-hash n text)
  (for/fold ([h (hash)])
            ([i (in-range (- (string-length text) n))])
    (extend-hash h (substring text i (+ i n))
                 (string-ref text (+ i n)))))

;; write a jsexpr to a js file so it can be loaded in js
(define (jsexpr->file top-level-varname filename jsexpr)
  (with-output-to-file filename
    #:exists 'truncate
    (lambda ()
      (display (~a top-level-varname" = \n"))
      (write-json jsexpr)
      (display ";\n"))))

;; serialize a tree-hash to a file
(define (trees-hash->file trees-hash filename)
  (jsexpr->file "TextModel" filename
                (for/hash ([(k v) (in-hash trees-hash)])
                  (values (string->symbol k)
                          (huffman-tree->jsexpr v)))))

;; serialize a single huffman tree to a file
(define (tree->file tree filename)
  (jsexpr->file "SeedModel" filename
                (huffman-tree->jsexpr tree)))

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

(define (string-rotate str chr)
  (string-append (substring str 1) (string chr)))

(define (generate-char-sequence-from-bools seed bools tree-hash)
  (generate-sequence-from-bools string-rotate seed bools tree-hash))

;; given a seed (markov cell) and a list of booleans and a hash
;; of huffman trees, generate a string
(define (generate-string-from-bools seed bools tree-hash)
  (string-append
   seed
   (list->string
    (map car (generate-char-sequence-from-bools seed bools tree-hash)))))

#;(generate-string "ot" dist-2-hash)

;; given a count-hash, return a huffman tree for choosing a seed (an n-gram
;; starting with a space)
(define (count-hash->seed-chooser count-hash)
  (count-hash->seed-chooser* starts-with-space? count-hash))

;; does this string start with a space?
(define (starts-with-space? str)
  (equal? (string-ref str 0) #\space))

;; compute the average length of a password chosen by a particular
;; scheme. Just do monte carlo...
(define (avg-len generator)
  (/ (for/sum ([i (in-range NUM-TRIALS)])
       (string-length (generator)))
     NUM-TRIALS))

(define NUM-TRIALS 10000)

(define ENTROPY-BITS 69)

;; generate a password string with the required number of bits of entropy
(define (make-pwd-str seed tree-hash)
  (string-append
   seed
   (list->string
    (map car (generate-char-sequence-from-bools seed (make-bools-list ENTROPY-BITS)
                                           tree-hash)))))

(define (make-pwd-str/noseed seed-huff-tree tree-hash)
  (define bits (make-bools-list ENTROPY-BITS))
  (match-define (cons seed bits-remaining)
    (pick-leaf seed-huff-tree bits))
  (string-append 
   ;; strip the space off:
   (substring seed 1)
   (list->string 
    (map car (generate-char-sequence-from-bools seed bits-remaining tree-hash)))))





(define text 
  (whitespace-crunch
   (file->string #;"/tmp/legalese.txt" 
                 (build-path here "a-tale-of-two-cities.txt")
                 #;(build-path here "ascii-email-texts.txt")
                 #;"/tmp/dancing-queen.txt")))

(define count-hash-1 (time (n-letter-count-hash 1 text)))
(define tree-hash-1 (time (count-hash->trees count-hash-1)))

(define count-hash-2 (n-letter-count-hash 2 text))
(define tree-hash-2 (time (count-hash->trees count-hash-2)))
(define seed-tree-2 (count-hash->seed-chooser count-hash-2))

(hash-ref count-hash-2 "pr")

(define count-hash-3 (time (n-letter-count-hash 3 text)))
(define tree-hash-3 (time (count-hash->trees count-hash-3)))
(define seed-tree-3 (count-hash->seed-chooser count-hash-3))



(check-equal? (generate-char-sequence-from-bools "Th" '() tree-hash-2)
              '())

#;(printf "average length of passwords using 2-grams: ~v\n"
        (+ 2 (time (avg-len (lambda () (make-pwd-str "Th" tree-hash-2))))))

#;(printf "average length of passwords using 2-grams/noseed: ~v\n"
        (sub1 
         (time (avg-len (lambda () (make-pwd-str/noseed seed-tree-2 tree-hash-2))))))

;; without seed randomization: 23.9
;; with seed randomization: 19.1 (!)

;; write the tree out as js:
(trees-hash->file tree-hash-2 "tree-2-hash.js")
(tree->file seed-tree-2 "seed-tree-2.js")

(random-seed 2722196)



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

(sequence->string-pair
 (generate-char-sequence-from-bools "T" (make-bools-list ENTROPY-BITS) tree-hash-1))

(for/list ([i 7])
  (make-pwd-str "T" tree-hash-1))

(sequence->string-pair
 (generate-char-sequence-from-bools "Th" (make-bools-list ENTROPY-BITS) tree-hash-2))

(for/list ([i 4])
  (make-pwd-str/noseed seed-tree-2 tree-hash-2))


;; *** FOUR AND HIGHER: ***

(define count-hash-4 (time (n-letter-count-hash 4 text)))
(define tree-hash-4 (time (count-hash->trees count-hash-4)))
(define seed-tree-4 (count-hash->seed-chooser count-hash-4))

(define quintuple-letter-count-hash (time (n-letter-count-hash 5 text)))
(define tree-5-hash (time (count-hash->trees quintuple-letter-count-hash)))

(for/list ([i 8])
  (make-pwd-str/noseed seed-tree-3 tree-hash-3))

(for/list ([i 8])
  (make-pwd-str/noseed seed-tree-4 tree-hash-4))

(sequence->string-pair
 (generate-char-sequence-from-bools "The b" (make-bools-list ENTROPY-BITS)
                               tree-5-hash))

(for/list ([i 7])
  (make-pwd-str "The b" tree-5-hash))

#;(sort (hash->list (hash-ref letter-count-hash #\"))
      < #:key cdr)


"ades nown us offick ar" 
"2234251221633312233132"