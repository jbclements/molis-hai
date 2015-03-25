#lang racket

(require rackunit
         "hash-common.rkt"
         racket/runtime-path
         #;parsack)


(define-runtime-path here ".")

(define text (file->string (build-path here "a-tale-of-two-cities.txt")))



(define punct-chars
  (string->list ",;:\"_"))

(define regexps
  (append
   (list
    #px"^[0-9a-zA-Z']+"
    #px"^-+"
    #px"^[ \t\n]+"
    #px"^\\."
    #px"^\\("
    #px"^\\)"
    #px"^\\!"
    #px"^\\?"
    #px"^\\*")
   #;(map (lambda (ch) (string-append "^\\" (string ch))) punct-chars)
   (map (lambda (ch) (string-append "^" (string ch))) punct-chars))
  )

(define tokens
  (call-with-input-file (build-path here "a-tale-of-two-cities.txt")
    (lambda (ip)
      (let loop ([matchers regexps] [so-far empty])
        (cond [(eof-object? (peek-char ip))
               (reverse so-far)]
              [(empty? matchers)
               (error 'parsing "failed parsing at this point: ~e"
                      (read-string 30 ip))]
              [else
               (match (regexp-try-match (first matchers) ip)
                 [#f (loop (rest matchers) so-far)]
                 [(list match) (loop regexps (cons match so-far))])])))))

(define standard-tokens
(for/list ([t (in-list tokens)])
  (cond [(regexp-match #px"[ \n\t]+" t) #" "]
        [else t])))

;; given a number of characters and a source text,
;; build a hash of the number of times each n-gram
;; transitions to a given letter
(define (n-word-count-hash n text)
  (define first-word (first text))
  (let loop ([h (hash)] [remaining text] [subremaining (drop text n)])
    (cond [(empty? subremaining)
           (extend-hash h remaining first-word)]
          [else
           (loop (extend-hash h (take remaining n) (first subremaining))
                 (rest remaining)
                 (rest subremaining))])))

(define count-2-hash (n-word-count-hash 2 standard-tokens))
(define tree-2-hash (count-hash->trees count-2-hash))

(define count-3-hash (n-word-count-hash 3 standard-tokens))
(define tree-3-hash (count-hash->trees count-3-hash))

(define (list-rotate l n)
  (append (rest l) (list n)))

(define ENTROPY-BITS 56)

(define (make-pwd-str seed tree-hash)
  (apply
   bytes-append
   (append
    seed
    (map car (generate-sequence-from-bools list-rotate
                                           seed
                                           (make-bools-list ENTROPY-BITS)
                                           tree-hash)))))

(define SEED '(#"It" #" " #"was"))

(for/list ([i 8])
  (make-pwd-str SEED tree-3-hash))


