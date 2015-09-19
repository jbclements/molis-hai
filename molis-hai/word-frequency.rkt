#lang racket

;; this file generates passwords using the word-based as
;; opposed to the character-based model. It takes a long
;; time to generate the models, and the resulting passwords
;; are (in my opinion) needlessly wordy.

;; also... something's not right with this file; it seems to
;; be running for a long long time.

(require rackunit
         racket/runtime-path
         "word-model.rkt"
         "random-bits.rkt"
         #;parsack)

;; no need to test this file:
(module* test racket/base)

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
    (cond [(regexp-match #px"[ \n\t]+" t) " "]
          [else (bytes->string/utf-8 t)])))

(printf "done tokenizing\n")

(define model-2 (time (build-word-model 2 standard-tokens)))

(define model-3 (time (build-word-model 3 standard-tokens)))

(define ENTROPY-BITS 56)

(for/list ([i 8])
  (generate-word-pwd model-3 (random-bool-list ENTROPY-BITS)))


