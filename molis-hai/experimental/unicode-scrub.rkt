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
        #\è "e"
        #\u2022 "."
        #\u25e6 "."
        #\u25aa "."
        #\π "pi"
        #\u2da "degrees"
        #\ua0 " "
        #\ø "empty set"
        ))
#px""
(de-unicode/path "/tmp/bbb.txt" #;"email-texts.txt"
                 "/tmp/ascii-email-texts.txt"
                 #:char-mapping char-mapping)