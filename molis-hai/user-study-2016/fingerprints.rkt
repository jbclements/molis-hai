#lang racket

(require molis-hai/random-bits
         molis-hai/char-model
         molis-hai/example-model)

(define bytes
  (map (λ (s) (string->number s 16))
  (regexp-split #px":"
                "46:DC:1D:39:45:88:2A:6B:90:D2:AC:9E:0A:81:5E:9A:33:26:03:B1")))

(define bits (bytes->bits bytes))
(generate-char-pwd atotc-model bits)