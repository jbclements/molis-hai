#lang typed/racket/base

;; the char-based model

(require "shared-types.rkt"
         "build-model.rkt"
         "use-model.rkt")

(provide build-char-model
         generate-char-pwd
         generate/char
         whitespace-crunch
         non-alnum-to-hyphens
         starts-with-space?
         starts-with-hyphen?)
         

;; run the analyses for a given "order"
(: build-char-model (Natural String
                             (String -> Boolean)
                             -> (Model String Char)))
(define (build-char-model n text good-start?)
  (kvcount->model (n-letter-kvcount n text)
                  good-start?))


;; crunch whitespace in a string
(: whitespace-crunch (String -> String))
(define (whitespace-crunch str)
  (regexp-replace* #px"[ \t\r\n]+" str " "))

;; convert chars other than [:alnum:] to hyphens
(: non-alnum-to-hyphens (String -> String))
(define (non-alnum-to-hyphens str)
  (regexp-replace* #px"[^[:alnum:]]+" str "-"))


;; does this string start with a space?
(: starts-with-space? (String -> Boolean))
(define (starts-with-space? str)
  (equal? (string-ref str 0) #\space))

;; does this string start with a hyphen?
(: starts-with-hyphen? (String -> Boolean))
(define (starts-with-hyphen? str)
  (equal? (string-ref str 0) #\-))



;; remove all quotes from a string
(: kill-quotes (String -> String))
(define (kill-quotes str)
  (regexp-replace* #px"\"" str ""))

;; given a number of characters and a source text,
;; build a hash of the number of times each n-gram
;; transitions to a given letter
(: n-letter-kvcount (Natural String -> (KVCount String Char)))
(define (n-letter-kvcount n text)
  (cond
    [(< (string-length text) n)
     (raise-argument-error 'n-letter-kvcount
                           "string longer than the given order"
                           1 n text)]
    [else
     ;; add the beginning to the end to get the wraparound transitions,
     ;; too:
     (define t2 (string-append text (substring text 0 n)))
     (for/fold ([h : (KVCount String Char) (kvcount-new)])
               ([i (in-range (- (string-length t2) n))])
       (kvcount-extend h (substring t2 i (+ i n))
                       (string-ref t2 (+ i n))))]))


;;; USING the model

;; given a model and a number of bits of entropy, generate a password
(: generate-char-pwd ((Model String Char) (Listof Boolean) -> String))
(define (generate-char-pwd model bools)
  (generated->string
   (generate/char model bools)))

(: generated->string ((Generated String Char) -> String))
(define (generated->string g)
  (list->string
   (generated->sequence g string->list (lambda ([ch : Char]) (list ch)))))

;; given a seed tree, a list of bools and a tree-hash, generate a sequence
;; of (cons leaf bits-used)
(: generate/char
   ((Model String Char) (Listof Boolean) -> (Generated String Char)))
(define (generate/char model bits)
  (generate model bits string-rotate))

;; given a string and a character, add the char to the end and drop the first
(: string-rotate : (String Char -> String))
(define (string-rotate str chr)
  (substring (string-append str (string chr)) 1))

(module+ test
  (require #;(submod "..")
           "shared-types.rkt"
           "huffman.rkt"
           typed/rackunit)
  
  (check-not-exn
   (lambda () (Model-trans (build-char-model 2
                                             "Marlatplace "
                                             starts-with-space?))))

  (check-not-exn
   (lambda () (Model-trans (build-char-model 2
                                             (non-alnum-to-hyphens
                                              "Marlatplace ")
                                             starts-with-hyphen?))))
  
  (check-equal?
   (hash-ref
    (Model-trans (build-char-model 2 "Marlatplace "
                                   starts-with-space?))
    " M")
   (Leaf 1 #\a))
  (check-exn
   (regexp (regexp-quote "string longer than"))
   (lambda ()(build-char-model 2 "P" starts-with-space?)))
  (check-exn
   (regexp (regexp-quote "source text with at least one choice"))
   (lambda () (begin (build-char-model 2 "abcd efg"
                                       starts-with-space?) 'zzz)))

  (check-equal? (whitespace-crunch "  a \t\nbc de   ")
                " a bc de ")
  
  (check-equal? (kill-quotes "the \"only\" way")
                "the only way")

  (check-equal? (non-alnum-to-hyphens "  ABC9 97).4 d\n\n ")
                "-ABC9-97-4-d-")
  
  ;; this is a crummy test, because the huffman encoding here
  ;; is wildly underconstrained:
  #;(check-equal?
   (generate-char-pwd (build-char-model 0 "abcde" (λ (x) #t))
                      (list #t #t #t #f #f #t #f #f #t #f #f #f))
   "abecd"))