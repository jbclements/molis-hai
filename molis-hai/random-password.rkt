#lang racket

;; this is basically obsolete, now that you can build order-0 models.
(require "random-bits.rkt")

(define CHARS 
  (string->list #;"01" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890/+"))

(define CHAR-BITS (inexact->exact (floor (/ (log (length CHARS)) (log 2)))))

(provide
 (contract-out
  [random-password (-> nat? string?)]))

(define nat? exact-nonnegative-integer?)

(define (random-char)
  (list-ref CHARS (random (length CHARS))))

(define (random-password bits)
  (define pw-length (ceiling (/ bits CHAR-BITS)))
  (define bitlist (random-bool-list (* pw-length CHAR-BITS )))
  (list->string
   (let loop ([bitlist bitlist])
     (cond [(empty? bitlist) empty]
           [else (cons (list-ref CHARS (bits->num (take bitlist CHAR-BITS)))
                       (loop (drop bitlist CHAR-BITS)))]))))

(module+ main
  (define BITS 256)
  (define num-choices 1)
  (for ([p (build-list num-choices (lambda (x) (random-password BITS)))])
    (display p)
    (newline)))

