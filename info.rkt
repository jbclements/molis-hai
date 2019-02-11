#lang setup/infotab

(define collection 'multi)

(define deps
  (list '["base" #:version "6.3"]
        "rackunit-lib"
        "scribble-lib"
        "typed-racket-lib"
        "typed-racket-more"
        "web-server-lib"
        "pfds"))

(define build-deps
  (list "rackunit-lib"
        "racket-doc"))

