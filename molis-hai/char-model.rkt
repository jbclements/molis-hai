#lang typed/racket

;; the char-based model

(require "shared-types.rkt"
         "build-model.rkt")

(provide build-char-model)

;; run the analyses for a given "order"
(: build-char-model (Natural String -> (Model String Char)))
(define (build-char-model n text)
  (define cleaned-text
    (kill-quotes
     (whitespace-crunch
      text)))
  (kvcount->model (n-letter-kvcount n cleaned-text) starts-with-space?))


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

;; given a number of characters and a source text,
;; build a hash of the number of times each n-gram
;; transitions to a given letter
(: n-letter-kvcount (Natural String -> (KVCount String Char)))
(define (n-letter-kvcount n text)
  ;; add the beginning to the end to get the wraparound transitions, too:
  (define t2 (string-append text (substring text 0 (sub1 n))))
  (for/fold ([h : (KVCount String Char) (kvcount-new)])
            ([i (in-range (- (string-length t2) n))])
    (kvcount-extend h (substring t2 i (+ i n))
                    (string-ref t2 (+ i n)))))

(module+ test
  (require typed/rackunit)
  
  (check-equal? (whitespace-crunch "  a \t\nbc de   ")
                " a bc de ")
  
  (check-equal? (kill-quotes "the \"only\" way")
                "the only way"))