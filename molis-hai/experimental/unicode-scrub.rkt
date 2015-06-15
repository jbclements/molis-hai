#lang racket

(require "unicode-scrub-lib.rkt")

(define char-mapping
  (hash #\’ "'"
        #\‘ "'"
        #\” "\""
        #\“ "\""
        #\ufffd "?"
        #\u2026 "..."
        #\u2013 "--"
        #\u2014 "---"
        #\ñ "n"
        #\ä "a"
        #\ö "o"
        #\é "e"
        #\u2022 "."
        #\u25e6 "."
        #\u25aa "."
        #\π "pi"
        ))

(de-unicode/path "/tmp/email-texts.txt"
                 "/tmp/ascii-email-texts.txt"
                 #:char-mapping char-mapping)