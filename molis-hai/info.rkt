#lang info

(define name "Molis Hai")

;; hastily copied from portaudio. leaving these here as templates...
#;(define scribblings '(("portaudio.scrbl" () (tool))))
#;(define version "2015-10-13-12:44")
#;(define release-notes '((p "improved windows host api handling, "
                           "bugfixes")))
(define compile-omit-paths '("email-extraction"))

(define raco-commands
  (list
   (list "molis-hai"
         "cmd-line.rkt"
         "generate secure passwords using a source text"
         #f)))