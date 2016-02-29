#lang typed/racket

(require molis-hai/random-bits
         molis-hai/char-model
         molis-hai/example-model)

(require/typed sha
               [sha224 (Bytes -> Bytes)])

(define TESTING-STR-SALT #"OOH BABY BABY")
(define TESTING-STR-BITS 56)

;; generate the training string for a user (must always be the
;; same string...)
(: generate-training-str (String String Boolean -> String))
(define (generate-training-str uid pwd experimental-group?)
  (define bits (generate-password-bits uid pwd))
  (cond [experimental-group?
         ;; atotc-model strings always start with a space:
         (molis-hai-cleanup (generate-char-pwd atotc-model bits))]
        [else
         (generate-char-pwd base-model bits)]))

;; generate the set of bits to be used in generating the training string
(: generate-password-bits (String String -> (Listof Boolean)))
(define (generate-password-bits uid pwd)
  (take (bytes->bits
          (bytes->list
           (sha224 (bytes-append (string->bytes/utf-8 uid)
                                 (string->bytes/utf-8 pwd)
                                 TESTING-STR-SALT))))
         TESTING-STR-BITS))

;; remove the leading space, add an 'a' to the end if it ends
;; with a space.
(: molis-hai-cleanup (String -> String))
(define (molis-hai-cleanup str)
  (define str2 (substring str 1))
  (cond [(string=? (substring str2 (- (string-length str2) 1)) " ")
         (string-append str2 "a")]
        [else str2]))

(for/list : (Listof String)
  ([i (in-range 8)])
  (generate-training-str (string-append "abc" (number->string i)) "def" #f))
