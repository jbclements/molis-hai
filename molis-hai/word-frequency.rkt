#lang racket


(require rackunit
         racket/runtime-path
         "use-model.rkt"
         "word-model.rkt"
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


(define model-2 (build-word-model 2 standard-tokens))

(define model-3 (build-word-model 3 standard-tokens))

(define (list-rotate l n)
  (append (rest l) (list n)))

(define ENTROPY-BITS 56)

(define (make-pwd-str model)
  (generate list-rotate
            model
            (make-bools-list ENTROPY-BITS))
  (apply
   bytes-append
   (append
    seed
    (map car (generate-sequence-from-bools list-rotate
                                           seed
                                           (make-bools-list ENTROPY-BITS)
                                           tree-hash)))))

(define SEED '(#" " #"that" #" "))

(for/list ([i 8])
  (make-pwd-str SEED tree-3-hash))


